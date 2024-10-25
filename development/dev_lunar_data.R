library(mgcv)
library(gratia)
library(tidyverse)

data1 <- new.env()
load("data/InputData1.RData", data1)

data2 <- new.env()
load("data/InputData2.RData", data2)

data3 <- new.env()
load("data/InputData3.RData", data3)

tdat1 <- bind_cols(c(data1$nimData1, data1$species.info1)) %>% 
  as_tibble %>% 
  mutate(
    across(c(daylight, twilight, night), ~.x / rowSums(across(c(daylight, twilight, night)))),
    across(c(fullmoon, newmoon, transitional), ~.x / rowSums(across(c(fullmoon, newmoon, transitional)))),
    across(where(is.character), factor)
  ) 

tdat1 %>% 
  ggplot(aes(night, fullmoon)) +
  geom_point() +
  geom_smooth(method = "gam", method.args = list(family = "betar"))


tmod1 <- bam(
  fullmoon ~ night + s(order, bs="re"), # + s(realm, bs="re") + ti(order, realm, bs="re") + ti(night, order, realm, bs="re"), 
  data = tdat1, 
  family = "quasibinomial",
  method = "fREML",
  discrete = T
)

tmod1 %>% 
  {
    print(
      draw(., parametric=T) /
            appraise(.)
    )
    mod_overview(., parametric_effect_sizes = T)
  }

