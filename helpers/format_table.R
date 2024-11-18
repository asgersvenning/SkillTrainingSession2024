library(kableExtra)
library(magrittr)

format_table <- function(tab, caption=NULL, digits=3, align="c", template=kable_material, ...) {
  tab %>% 
    kbl(
      caption = "Result of `add_fitted_samples(iris_data, lm_iris, n = 100)`",
      digits = 3, 
      align = "c",
      ...
    ) %>% 
    template(
      full_width = F
    ) %>% 
    row_spec(0, bold = T) %>% 
    row_spec(0:nrow(tab), font_size = 14, extra_css = "border-bottom: 0px;") %>% 
    row_spec(seq(nrow(tab)-1, by=-2), extra_css = "background-color: var(--datatable-odd-background);") %>% 
    row_spec(seq(nrow(tab), by=-2), extra_css = "background-color: var(--datatable-even-background);")
}