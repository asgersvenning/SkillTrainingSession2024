library(tidyverse)
library(mgcv)
library(gratia)

source("helpers/predict.R")
source("helpers/statistics.R")

theme_set(
  theme_bw() +
    theme(
      text = element_text(family = "serif"),
      strip.text = element_text(hjust = 0.5, size = 16, face = "bold"),
      strip.text.y.right = element_text(angle = 0),
      strip.text.y.left = element_text(angle = 0),
      title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.title = element_text(size = 20, face = "plain"),
      legend.title = element_text(hjust = 0.5),
      legend.text = element_text(size = 14),
      panel.border = element_rect(color = "black", linewidth = 1)
    )
)

df <- zooplankton %>% 
  mutate(
    time = year + day/364.24,
    time = time - min(time),
    time = time / max(time),
    density_adj_log10 = log10(density_adj),
    density_scaled_exp = exp(density_scaled)
  )

# Fit a GAMM model using the scat family heavy-tailed "gaussian" data
gamm_model <- bam(
  density_adj_log10 ~ 
    time * taxon +
    te(day, time, taxon, bs=c("cc", "tp", "re"), k=c(10, 5, 10)), 
  family = "scat", 
  method = "fREML",
  data = df, 
  discrete = T
)

# Smooth terms
broom::tidy(gamm_model)
# Parametric terms
broom::tidy(gamm_model, parametric = T)

# Diagnostics
gratia::appraise(gamm_model)

# Visualize the model fit
gratia::draw(gamm_model, guides = "collect")

# Visualize parametric model estimates
gamm_model %>%
  tidy_summarize_parametric %>% 
  ggplot(aes(estimate, pretty_label, fill = label)) + 
  geom_col(
    color = "black",
    linewidth = 1,
    key_glyph = draw_key_point
  ) +
  geom_text(
    aes(
      color=after_scale(fill), 
      label=scales::label_pvalue(add_p=T)(p.value), 
      hjust=ifelse(estimate >= 0, -0.1, 1.1)
    ),
    fontface = "bold",
    show.legend = F
  ) +
  geom_label(
    aes(
      x = 0,
      label = scales::number(estimate, accuracy = 0.01), 
      hjust=ifelse(estimate >= 0, 1.25, -0.25)
    ), 
    label.size = 1,
    color = "black",
    fontface = "bold",
    show.legend = F
  ) +
  geom_vline(xintercept = 0, linewidth = 1) +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "Estimate",
    y = "Parameter",
    fill = "Parameter\nGroup"
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        shape = 21, 
        color = "black",
        size = 7,
        stroke = 1
      )
    )
  )

# Visualize parametric predictions
df %>% 
  select(time, taxon, day) %>% 
  {
    expand_grid(
      time = seq(0, 1, length.out = 1000),
      taxon = unique(.$taxon),
      day = seq(min(.$day), max(.$day), length.out = 1)
    )
  } %>% 
  gratia::add_fitted_samples(
    gamm_model,
    n = 100,
    terms = c("time", "taxon", "time:taxon")
  ) %>% 
  arrange(taxon, time, day) %>% 
  ggplot(aes(time, .fitted, color = taxon, group = paste0(taxon, .draw))) +
  stat_summary_bin(
    aes(group = taxon),
    fun.data = median_hilow,
    geom = "ribbon",
    alpha = 0.15,
    bins = 100
  ) +
  stat_summary_bin(
    aes(group = taxon),
    fun = median,
    geom = "line",
    linewidth = 1,
    bins = 100
  ) +
  geom_line(
    linewidth = 0.25,
    alpha = 0.15
  ) +
  facet_wrap(~taxon)
