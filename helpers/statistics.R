library(magrittr)
library(dplyr)
library(stringr)

mod_.call_too_far <- function (smooth, input, reference, cols, dist){
  was_nested <- FALSE
  if (
    !is.null(input[["data"]]) && 
    (inherits(input, "tbl_df") && 
     is.list(input[["data"]]))
  ) {
    input <- unnest(input, cols = all_of("data"))
    was_nested <- TRUE
  }
  sm_vars <- gratia:::smooth_variable(smooth)
  if (is.numeric(input[[sm_vars[1L]]]) && is.numeric(input[[sm_vars[2L]]])) {
    ind <- gratia:::too_far(
      x = input[[sm_vars[1L]]], 
      y = input[[sm_vars[2L]]], 
      ref_1 = reference[[sm_vars[1L]]],
      ref_2 = reference[[sm_vars[2L]]], 
      dist = dist
    )
  } else {
    ind <- rep(FALSE, nrow(input))
  }
  input <- mutate(
    input, 
    across(all_of(cols), function(x) to_na(x, i = ind))
  )
  if (was_nested) {
    input <- nest(
      input, 
      data = !all_of(c(".smooth", ".type",".by"))
    )
  }
  input
}

mod_too_far_to_na <- function (smooth, input, reference, cols, dist = NULL) {
  sm_dim <- smooth_dim(smooth)
  if (sm_dim < 2L || sm_dim > 3L) {
    return(input)
  }
  input <- mod_.call_too_far(
    smooth = smooth, 
    input = input, 
    reference = reference,
    cols = cols, 
    dist = dist
  )
  input
}

mod_eval_smooth.tensor.smooth <- function (
    smooth, 
    model, 
    n = 100, 
    n_3d = NULL, 
    n_4d = NULL, 
    data = NULL,
    unconditional = FALSE, 
    overall_uncertainty = TRUE, 
    dist = NULL,
    ...
) {
  by_var <- by_variable(smooth)
  if (by_var == "NA") {
    by_var <- NA_character_
  }
  var_order <- gratia:::reorder_tensor_smooth_terms(smooth)
  id <- gratia:::which_smooth(model, smooth_label(smooth))
  data <- gratia:::process_user_data_for_eval(
    data = data, model = model, 
    n = n, n_3d = n_3d, n_4d = n_4d, id = id, var_order = var_order
  )
  eval_sm <- spline_values(
    smooth, data = data, unconditional = unconditional, 
    model = model, overall_uncertainty = overall_uncertainty
  )
  eval_sm <- gratia:::add_by_var_column(eval_sm, by_var = by_var)
  eval_sm <- gratia:::add_smooth_type_column(eval_sm, sm_type = smooth_type(smooth))
  if (
    smooth_dim(smooth) == 2L && 
    (!is.null(dist) && dist > 0)
  ) {
    eval_sm <- mod_too_far_to_na(
      smooth, input = eval_sm, reference = model[["model"]], 
      cols = c(".estimate", ".se"), dist = dist
    )
  }
  tensor_term_order <- setNames(list(var_order), smooth_label(smooth))
  attr(eval_sm, "tensor_term_order") <- tensor_term_order
  class(eval_sm) <- append(
    class(eval_sm), 
    c("tensor_eval_sm", "eval_sm"), 
    after = 0L
  )
  eval_sm
}

mod_smooth_estimates <- function(
    object,
    select = NULL,
    smooth = lifecycle::deprecated(),
    n = 100,
    n_3d = 16,
    n_4d = 4,
    data = NULL,
    unconditional = FALSE,
    overall_uncertainty = TRUE,
    dist = NULL,
    unnest = TRUE,
    partial_match = FALSE,
    ...
) {
  if (lifecycle::is_present(smooth)) {
    lifecycle::deprecate_warn(
      "0.8.9.9", 
      "smooth_estimates(smooth)",
      "smooth_estimates(select)"
    )
    select <- smooth
  }
  model_name <- rlang::expr_label(substitute(object))
  ## if particular smooths selected
  S <- smooths(object) # vector of smooth labels - "s(x)"
  
  # select smooths
  select <- gratia:::check_user_select_smooths(
    smooths = S, select = select,
    partial_match = partial_match,
    model_name = model_name
  )
  smooth_ids <- which(select)
  
  ## extract the mgcv.smooth objects
  smooths <- get_smooths_by_id(object, smooth_ids)
  
  ## loop over the smooths and evaluate them
  sm_list <- vector(mode = "list", length = length(smooths))
  
  ## if user data supplied, check for and remove response
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("'data', if supplied, must be a numeric vector or data frame.",
           call. = FALSE
      )
    }
    gratia:::check_all_vars(object, data = data, smooths = smooths)
    data <- gratia:::delete_response(object, data = data)
  }
  
  # # fix up the n, n_3d, n_4d. If `n_3d` is `NULL` set `n_3d <- n`
  # if (is.null(n_3d)) {
  #     n_3d <- n
  # }
  # # likewise fix up n_4d; set it to `n` if `n_4d` is NULL
  # if (is.null(n_4d)) {
  #     n_4d <- n
  # }
  
  for (i in seq_along(sm_list)) {
    if (inherits(smooths[[i]], "tensor.smooth")) {
      eval_fn <- mod_eval_smooth.tensor.smooth
    } else {
      eval_fn <- eval_smooth
    }
    
    sm_list[[i]] <- eval_fn(
      smooths[[i]],
      model = object,
      n = n,
      n_3d = n_3d,
      n_4d = n_4d,
      data = data,
      unconditional = unconditional,
      overall_uncertainty = overall_uncertainty,
      dist = dist
    )
  }
  
  # see if we have any tensor term orders to collect & apply
  tensor_term_order <- lapply(sm_list, attr, "tensor_term_order")
  ## create a single df of all the smooths
  sm_list <- bind_rows(sm_list)
  
  ## need to unnest the `data` column?
  if (isTRUE(unnest)) {
    sm_list <- unnest(sm_list, all_of("data"))
  }
  
  # add back any special attributes
  attr(sm_list, "tensor_term_order") <- do.call("c", tensor_term_order)
  
  ## add a class
  class(sm_list) <- c("smooth_estimates", class(sm_list))
  
  ## return
  sm_list
}

mod_draw <- function(
    object,
    data = NULL,
    select = NULL,
    parametric = FALSE,
    terms = NULL,
    residuals = FALSE,
    scales = c("free", "fixed"),
    ci_level = 0.95,
    n = 100,
    n_3d = 16,
    n_4d = 4,
    unconditional = FALSE,
    overall_uncertainty = TRUE,
    constant = NULL,
    fun = NULL,
    dist = 0.1,
    rug = TRUE,
    contour = TRUE,
    grouped_by = FALSE,
    ci_alpha = 0.2,
    ci_col = "black",
    smooth_col = "black",
    resid_col = "steelblue3",
    contour_col = "black",
    n_contour = NULL,
    partial_match = FALSE,
    discrete_colour = NULL,
    discrete_fill = NULL,
    continuous_colour = NULL,
    continuous_fill = NULL,
    position = "identity",
    angle = NULL,
    ncol = NULL, nrow = NULL,
    guides = "keep", widths = NULL, heights = NULL,
    crs = NULL,
    default_crs = NULL,
    lims_method = "cross",
    wrap = TRUE,
    caption = TRUE,
    envir = environment(formula(object)),
    ...
) {
  # fixed or free scale?
  scales <- match.arg(scales)
  
  # adding a caption?
  caption <- as.logical(caption)
  
  # fix up default scales
  # if (is.null(discrete_colour)) {
  #    discrete_colour <- scale_colour_discrete()
  # }
  if (is.null(continuous_colour)) {
    continuous_colour <- scale_colour_continuous()
  }
  if (is.null(continuous_fill)) {
    continuous_fill <- scale_fill_distiller(palette = "RdBu", type = "div")
  }
  
  # if not using select, set parametric TRUE if not set to FALSE
  if (!is.null(select)) {
    if (is.null(parametric)) {
      parametric <- FALSE
    }
  } else {
    if (is.null(parametric)) {
      parametric <- TRUE
    }
  }
  
  # sort out n_3d and n_4d. If these are `NULL` then do sensible thing at set
  # them small. Default is 3 for n_4d and for 12 for n_3d, but if we have a kD
  # smooth (k >= 4) we want to
  
  S <- smooths(object) # vector of smooth labels - "s(x)"
  
  # select smooths
  select <-
    gratia:::check_user_select_smooths(
      smooths = S, select = select,
      partial_match = partial_match,
      model_name = rlang::expr_label(substitute(object))
    )
  
  # this is needed for the parametric terms below
  sm_plts <- NULL
  ylims <- NULL
  
  # do we have any smooths to plot?
  if (length(select) > 0L) {
    # evaluate all requested smooths
    sm_eval <- mod_smooth_estimates(
      object,
      select = S[select],
      n = n,
      n_3d = n_3d,
      n_4d = n_4d,
      data = data,
      unconditional = unconditional,
      overall_uncertainty = overall_uncertainty,
      dist = dist,
      unnest = FALSE
    )
    
    # grab tensor term order if present, if not it is NULL & that's OK
    tensor_term_order <- attr(sm_eval, "tensor_term_order")
    
    # add confidence interval
    sm_eval <- sm_eval %>%
      rowwise() %>%
      mutate(data = list(add_confint(.data$data, coverage = ci_level))) %>%
      ungroup()
    
    # Take the range of the smooths & their confidence intervals now
    # before we put rug and residuals on
    if (utils::packageVersion("dplyr") > "1.0.10") {
      sm_rng <- sm_eval |>
        rowwise() |>
        utils::getFromNamespace("reframe", "dplyr")(
          rng = range(c(data$.estimate, data$.lower_ci, data$.upper_ci))
        ) |>
        pluck("rng")
    } else {
      sm_rng <- sm_eval |>
        rowwise() |>
        summarise(rng = range(c(
          data$.estimate, data$.lower_ci,
          data$.upper_ci
        ))) |>
        pluck("rng")
    }
    
    # Add partial residuals if requested - by default they are
    # At the end of this, sm_eval will have a new list column containing the
    # partial residuals, `partial_residual`
    p_resids_rng <- NULL
    if (isTRUE(residuals)) {
      if (is.null(residuals(object)) || is.null(weights(object))) {
        residuals <- FALSE
      } else {
        # get residuals in a suitable format
        p_resids <- gratia:::nested_partial_residuals(object, terms = S[select])
        
        # compute the range of residuals for each smooth
        # p_resids_rng <- p_resids |>
        #     rowwise() |>
        #     dplyr::reframe(rng =
        #         range(.data$partial_residual$partial_residual)) |>
        #     pluck("rng")
        if (utils::packageVersion("dplyr") > "1.0.10") {
          p_resids_rng <- p_resids |>
            rowwise() |>
            utils::getFromNamespace("reframe", "dplyr")(
              rng = range(.data$partial_residual$partial_residual)) |>
            pluck("rng")
        } else {
          p_resids_rng <- p_resids |>
            rowwise() |>
            summarise(
              rng =
                range(.data$partial_residual$partial_residual)
            ) |>
            pluck("rng")
        }
        # merge with the evaluated smooth
        sm_eval <- gratia:::suppress_matches_multiple_warning(
          left_join(sm_eval, p_resids, by = ".smooth")
        )
      }
    }
    
    # add rug data?
    if (isTRUE(rug)) {
      # get rug data in a suitable format
      rug_data <- gratia:::nested_rug_values(object, terms = S[select])
      
      # merge with the evaluated smooth
      sm_eval <- gratia:::suppress_matches_multiple_warning(
        left_join(sm_eval, rug_data, by = ".smooth")
      )
    }
    
    # need to figure out scales if "fixed"
    if (isTRUE(identical(scales, "fixed"))) {
      ylims <- range(sm_rng, p_resids_rng)
    }
    
    # draw smooths
    sm_l <- if (isTRUE(grouped_by)) {
      sm_levels <- unique(sm_eval$.smooth)
      levs <- unique(str_split_fixed(sm_eval$.smooth, ":", n = 2)[, 1])
      sm_l <- sm_eval |>
        mutate(
          ..smooth.. = factor(.data$.smooth, levels = S[select]),
          .term = str_split_fixed(.data$.smooth, ":", n = 2)[, 1],
          ..by.. = if_else(is.na(.data$.by), "..no_level..", .data$.by)
        ) |>
        relocate(".term", .before = 1L)
      grp_by_levs <- unique(sm_l$"..by..")
      sm_l <- sm_l |>
        group_split(
          factor(.data$.term, levels = sm_levels),
          factor(.data$"..by..", levels = grp_by_levs)
        )
      # sometimes the steps to get the order right above don't work
      sm_l_levs <- vapply(sm_l, \(x) unique(x$.term), character(1L))
      if (!identical(unique(sm_l_levs), levs)) {
        names(sm_l) <- sm_l_levs
        sm_l <- sm_l[levs]
        names(sm_l) <- NULL
      }
      sm_l
    } else {
      # the factor is to reorder to way the smooths entered the model
      group_split(sm_eval, factor(.data$.smooth, levels = S[select]))
    }
    sm_plts <- map(
      sm_l,
      gratia:::draw_smooth_estimates,
      constant = constant,
      fun = fun,
      contour = contour,
      contour_col = contour_col,
      n_contour = n_contour,
      ci_alpha = ci_alpha,
      ci_col = ci_col,
      smooth_col = smooth_col,
      resid_col = resid_col,
      partial_match = partial_match,
      discrete_colour = discrete_colour,
      discrete_fill = discrete_fill,
      continuous_colour = continuous_colour,
      continuous_fill = continuous_fill,
      angle = angle,
      ylim = ylims,
      crs = crs,
      default_crs = default_crs,
      lims_method = lims_method,
      tensor_term_order = tensor_term_order,
      caption = caption,
      ... # FIXME: temporary fix to allow captions to be suppressed-ish
    )
  } # end stuff for smooths...
  
  # Are we plotting parametric effects too?
  if (isTRUE(parametric)) {
    if (length(parametric_terms(object)) == 0L) {
      message("The model contains no parametric terms")
      parametric <- FALSE
    } else {
      para <- parametric_effects(
        object,
        select = terms, data = data,
        unconditional = unconditional,
        unnest = TRUE, ci_level = ci_level, envir = envir
      )
      
      if (is.null(para)) {
        parametric <- FALSE
      } else {
        # Add CI
        # crit <- coverage_normal(ci_level)
        # object <- mutate(para,
        #    .lower_ci = .data$.partial - (crit * .data$.se),
        #    .upper_ci = .data$.partial + (crit * .data$.se))
        object <- para |> add_confint(coverage = ci_level)
        # need to alter the ylim if scales are fixed
        if (isTRUE(identical(scales, "fixed"))) {
          ylims <- range(
            ylims, object$.partial, object$.upper_ci,
            object$.lower_ci
          )
        }
        
        f_levels <- attr(para, "factor_levels")
        
        para_plts <- para %>%
          group_by(.data$.term) %>%
          group_map(
            .keep = TRUE,
            .f = ~ gratia:::draw_parametric_effect(
              .x,
              ci_level = ci_level,
              ci_col = ci_col,
              ci_alpha = ci_alpha,
              line_col = smooth_col,
              constant = constant,
              fun = fun,
              rug = rug,
              position = position,
              angle = angle,
              ylim = ylims,
              factor_levels = f_levels
            )
          )
      }
    }
  }
  
  if (isTRUE(parametric)) {
    sm_plts <- append(sm_plts, para_plts)
  }
  
  # filter out NULLs as those are types of smooths we can't plot (yet)
  no_plot <- map_lgl(sm_plts, is.null)
  sm_plts <- sm_plts[!no_plot]
  
  if (all(no_plot)) {
    message("Unable to draw any of the model terms.")
    return(invisible())
  }
  
  # return
  n_plots <- length(sm_plts)
  if (is.null(ncol) && is.null(nrow)) {
    ncol <- ceiling(sqrt(n_plots))
    nrow <- ceiling(n_plots / ncol)
  }
  if (n_plots > 1L && is.null(widths)) {
    # it doesn't matter about the widths if only one plot, but if we have
    # more than one plot and the user didn't change `widths`, then we will
    # force a value of 1 to give all plots the same relative width
    widths <- 1
  }
  
  if (wrap) {
    sm_plts <- patchwork::wrap_plots(
      sm_plts,
      byrow = TRUE, ncol = ncol, nrow = nrow,
      guides = guides, widths = widths, heights = heights, ...
    )
  }
  
  # return
  sm_plts
}

mod_overview <- function(
    model, 
    parametric = TRUE, 
    parametric_effect_sizes = FALSE,
    random_effects = TRUE,
    dispersion = NULL,
    frequentist = FALSE,
    accuracy = 0.001,
    stars = FALSE,
    ...
) {
  smry <- model %>% 
    summary(
      dispersion = dispersion,
      re.test = random_effects,
      freq = frequentist
    )
  
  nms <- c("term", "type", "estimate", "se", "k", "edf", "statistic", "p.value")
  
  # smooth terms
  types <- vapply(model$smooth, smooth_type, character(1))
  dfs <- vapply(model$smooth, basis_size, double(1))
  
  out <- smry$s.table %>% 
    as.data.frame() %>%
    rownames_to_column() %>%
    as_tibble()
  
  if (nrow(out) > 0) {
    out <- out %>%
      select(!matches("Ref.df")) %>%
      rename(
        statistic = 3
      ) %>% 
      add_column(
        type = types,
        Estimate = NA_real_,
        `Std. Error` = NA_real_,
        k = dfs, 
        .after = 1L
      )
  }
  
  # parametric terms
  para <- NULL
  if (isTRUE(parametric) && !is.null(smry$p.table)) {
    if (isFALSE(parametric_effect_sizes)) {
      para <- as.data.frame(smry$pTerms.table) %>%
        rownames_to_column() %>%
        as_tibble()
      if (nrow(para) > 0) {
        para <- para %>% 
          rename(
            edf = df,
            statistic = 3,
          ) %>% 
          add_column(
            type = "parametric",
            Estimate = NA_real_,
            `Std. Error` = NA_real_,
            k = NA_real_,
            .after = 1L
          )
      }
    } else {
      para <- smry$p.table %>% 
        as.data.frame() %>%
        rownames_to_column() %>%
        as_tibble()
      
      if (nrow(para) > 0) {
        para <- para %>% 
          rename(
            statistic = 4,
            `p-value` = 5
          ) %>% 
          add_column(
            type = "parametric", 
            .after = 1L
          ) %>% 
          add_column(
            k = NA_real_,
            edf = 1,
            .after = 4L
          )
      }
    }
    
    out <- bind_rows(para, out)
  }
  
  out <- set_names(out, nms)
  
  if (isFALSE(parametric_effect_sizes)) out <- out %>% 
    select(!c(estimate, se))
  
  if (stars) {
    sstars <- out$p.value %>% 
      symnum(
        corr = FALSE,
        na = FALSE,
        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
        symbols = c("***", "**", "*", ".", " ")
      )
    out <- out %>% 
      mutate(
        # p = .data$p.value,
        p.value = format.pval(.data$p.value, eps = accuracy),
        stars = sstars
      ) # not sure why as.character(sstars) is wrong here "***"
    attr(out, "legend") <- attr(sstars, "legend")
  } else {
    out <- out %>% 
      mutate(
        p.value = format.pval(.data$p.value, eps = accuracy)
      )
  }
  
  class(out) <- append(class(out), values = "overview", after = 0)
  out
}


# str_escape_dot_parentheses <- function(s) str_replace_all(s, "(\\.|\\(|\\))", "\\\\\\1")
# 
# assign_label <- function(x, labels) {
#   if (is.null(names(labels))) names(labels) <- labels
#   
#   label_ord <- order(sapply(labels, nchar), decreasing = TRUE)
#   label_pat <- str_c("^", labels[label_ord]) %>% 
#     str_escape_dot_parentheses
#   out_label <- names(labels)[label_ord]
#   
#   sapply(x, function(y) {
#     str_detect(y, label_pat) %>% 
#       {ifelse(any(.), which(.), NA)} %>%
#       min
#   }) %>% 
#     out_label[.]
# }
# 
# .tidy_summarize <- function(model, parametric) {
#   if (parametric) {
#     terms <- c("Intercept" = "(Intercept)", parametric_terms(model))
#   } else {
#     sterms <- model %>% 
#       smooth_terms
#     labels <- sterms %>% 
#       sapply(str_c, collapse=":") %>% 
#       {str_c("")}
#     terms <- sterms %>% 
#       sapply(str_c, collapse=",") %>% 
#       {str_c("\\w{1,2}.*[0-9]*(", . ,")")}
#     names(terms) <- labels
#   }
#   print(terms)
#   
#   sm <- model %>% 
#     broom::tidy(parametric=parametric)
#   
#   if (nrow(sm) > 0) {
#     sm <- sm %>%
#       print %>% 
#       mutate(
#         label = assign_label(term, terms),
#         level = ifelse(
#           rep(parametric, n()),
#           str_remove(term, str_c("^", str_escape(label))),
#           ""
#         ),
#         order = str_count(label, "\\:"),
#         # pretty_label = ifelse(
#         #   level == "" | term == "(Intercept)",
#         #   as.character(label),
#         #   str_c(label, " (", level, ")")
#         # )
#       )
#   }
#   
#   if (parametric) {
#     sm <- sm %>% 
#       mutate(type = ifelse(level == "" | term == "(Intercept)", "numeric", "factor"))
#   } else {
#     sm <- sm %>% 
#       mutate(type = "smooth")
#   }
#   
#   list(
#     summary = sm,
#     term_order = names(terms)
#   )
# }
# 
# tidy_summarize <- function(model, type=c("all", "parametric", "smooth")) {
#   type <- match.arg(type)
#   
#   pterm_summary <- .tidy_summarize(model, T)
#   sterm_summary <- .tidy_summarize(model, F)
#   
#   if (type == "all") {
#     summaries <- list(
#       pterm_summary$summary,
#       sterm_summary$summary
#     )
#     terms <- c(pterm_summary$term_order, sterm_summary$term_order)
#   } else if (type == "parametric") {
#     summaries <- list(
#       pterm_summary$summary
#     )
#     terms <- pterm_summary$term_order
#   } else {
#     summaries <- list(
#       sterm_summary$summary
#     )
#     terms <- sterm_summary$term_order
#   }
#   
#   bind_rows(
#     summaries
#   ) %>% 
#     relocate(term, label, level, type, order, .before=0) %>% 
#     mutate(
#       type = factor(type, c("numeric", "factor", "smooth")),
#       label = factor(label, terms)
#     ) %>% 
#     arrange(type, order, label) 
# }
# 
