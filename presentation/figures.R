library(tidyverse)
library(gganimate)

get_sfun <- function(
    zlim = c(-0.1, 1.1), 
    slim = c(-1, 1), 
    ctrl.pts=10, 
    plot=F
) {
  z <- seq(zlim[1], zlim[2], length.out = ctrl.pts)
  c <- runif(ctrl.pts, min = slim[1], max = slim[2]) - z
  
  sfun <- splinefun(z, c, method = "natural")
  if (plot) plot_smooth(sfun, z, c, zlim)
  
  return(invisible(sfun))
}

sfun <- get_sfun(c(4, 8))

iris %>% 
  as_tibble %>% 
  filter(Species != "setosa") %>% 
  mutate(
    Petal.Length = sfun(Petal.Length),
  ) %>% 
  ggplot(aes(Sepal.Length, Petal.Length)) +
  geom_point() +
  geom_smooth(
    method = "gam",
    se = FALSE,
    # method.args = list(family = "quasipoisson"),
    fullrange = F,
    linewidth = 2,
    color = "black"
  ) +
  scale_y_continuous(limits = c(-7.25, -3.5), oob = scales::oob_squish) +
  labs(x = "X", y = "Y") +
  theme(
    axis.text = element_blank(),
    axis.title = element_text(size = 32)
  )

par(mfrow = c(2, 2))

iris_mod <- iris %>% 
  as_tibble %>% 
  filter(Species != "setosa") %>% 
  mutate(
    Petal.Length = sfun(Petal.Length),
  )

iris_mod %>% 
  # lm(Petal.Length ~ Sepal.Length, data = .) %>%
  gam(Petal.Length ~ s(Sepal.Length, bs = "tp", k = 25), data = ., method = "REML", family= "gaussian") %>%
  # gratia::appraise()
  {
    tmp <- .
    gratia::fitted_values(tmp, iris_mod) %>%
      mutate(
        `Linear Predictor` = predict(tmp, type = "link") %>% 
          as.vector
      )
  } %>% 
  select(Sepal.Length, Petal.Length, `Linear Predictor`, .fitted) %>% 
  pivot_longer(c(Sepal.Length, `Linear Predictor`), names_to = "Variable", values_to = "value") %>% 
  ggplot(aes(x = value)) +
  geom_point(aes(y = Petal.Length), color = "black", size = 0.5) +
  geom_line(aes(y = .fitted), color = "black", linewidth = 2) +
  facet_wrap(~Variable, scales = "free") 
  
  residuals %>% 
  hist(15, main = "Residual Histogram", xlab = "Residuals", ylab = "Count")

curve(20 * dnorm(x, mean = 0, sd = 1), -10, 10, col = "red", lwd = 2, ylab = "Density", xlab = "X", add = T, n = 1000)



data <- tibble(
  x = runif(1000),
  y = rnorm(1000, x)
) 

par(mfrow = c(1, 1))

lm(y ~ x, data = data) %>%
  residuals %>% 
  hist(main = "Residual Histogram", xlab = "Residuals", ylab = "Count")

curve(525 * dnorm(x, mean = 0, sd = 1), -10, 10, col = "red", lwd = 2, ylab = "Density", xlab = "X", add = T, n = 1000)


# gam(y ~ x, data = data)
# 
# gam(y ~ x + s(z, bs = "re"), data = data)
# 
# gam(y ~ x, data = data, family = "binomial")
# 
# gam(y ~ x + s(z, bs = "re"), data = data, family = "binomial")
# 
# gam(y ~ s(x), data = data, method = "REML")
# 
# gam(y ~ s(x) + s(z, bs = "re"), data = data, method = "REML", family = "binomial")
# 
# 
# 


library(mgcv)
library(gratia)

# Examples for mgcv families

# scat
data <- tibble(
  x = runif(100),
  y = rnorm(100, x) %>% 
    add(sign(.) * (x - .) ^2)
)

data %>% 
  ggplot(aes(x, y)) +
  geom_point()

gam(y ~ x, data = data) %>% 
  appraise()

gam(y ~ x, data = data, family = "scat") %>% 
  appraise()


# ziP
data <- tibble(
  x = runif(100) * 5,
  y = rpois(x, exp(x)) 
    * (runif(100) > 0.5)
) 

data %>% 
  ggplot(aes(x, y)) +
  geom_point()

gam(y ~ x, data = data, family = "poisson") %>%
  appraise()

gam(y ~ x, data = data, family = "ziP") %>% 
  appraise()

# nb
data <- tibble(
  x = runif(100) * 3,
  y = rnorm(100, x) %>% 
    exp %>% 
    round
)

data %>% 
  ggplot(aes(x, y)) +
  geom_point()

gam(y ~ x, data = data, family = "poisson") %>% 
  appraise()

gam(y ~ x, data = data, family = "nb") %>%
  appraise()

# gaulss
data <- tibble(
  x = runif(100) * 3,
  y = rnorm(100, x, x)
)

data %>% 
  ggplot(aes(x, y)) +
  geom_point()

gam(y ~ x, data = data) %>%
  appraise()  

gam(list(y ~ x, ~ x), data = data, family = "gaulss") %>% 
  appraise()

# ziplss
data <- tibble(
  x = runif(100) * 4,
  y = rpois(x, exp(x)) 
    * (rnorm(100, x, 2) > 0)
)

data %>% 
  ggplot(aes(x, y)) +
  geom_point()

gam(y ~ x, data = data, family = "poisson") %>%
  appraise()

gam(list(y ~ x, ~ x), data = data, family = "ziplss") %>% 
  appraise()

# Examples for mgcv smooth terms

sfun <- get_sfun(c(0, 1), ctrl.pts=5)

curve(sfun(x), 0, 1)

# "tp"
data <- tibble(
  x = runif(100),
  y = 3 * sfun(x) + rnorm(100, 0.25)
)

data %>% 
  ggplot(aes(x, y)) +
  geom_point()

gam(y ~ x, data = data) %>%
  appraise()

gam(y ~ s(x, bs = "tp"), data = data, method = "REML") %>% 
  appraise()

# "ts"/"cs"
data <- tibble(
  x = runif(100),
  y = rnorm(100)
)

data %>% 
  ggplot(aes(x, y)) +
  geom_point()

gam(y ~ s(x, bs = "tp"), data = data, method = "REML") %>% 
  appraise

gam(y ~ s(x, bs = "ts"), data = data, method = "REML") %>%
  appraise

# "cc"
data <- tibble(
  x = c(0, (runif(198) + runif(198)) * pi, 2 * pi),
  y = sfun((sin(x) + 1) / 2) * 5 + rnorm(200, 0.15)
)

data %>%
  ggplot(aes(x, y)) +
  geom_point()

gam(y ~ s(x, bs = "cr", k = 25), data = data, method = "REML") %>%
  appraise

gam(y ~ s(x, bs = "cc", k = 25), data = data, method = "REML") %>%
  appraise
  
data %>% 
  add_fitted(gam(y ~ s(x, bs = "cr", k = 25), data = ., method = "REML")) %>% 
  rename("cr" = .fitted) %>% 
  add_fitted(gam(y ~ s(x, bs = "cc", k = 25), data = ., method = "REML")) %>%
  rename("cc" = .fitted) %>%
  pivot_longer(c(cr, cc), names_to = "Smooth", values_to = "Fitted") %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_function(fun = ~ sfun((sin(.x) + 1)/2) * 5, linewidth = 2) +
  geom_line(aes(y = Fitted, color = Smooth), size = 1) 

# re
data <- tibble(
  x = runif(200),
  w = abs(((matrix(rep(1:4, 200), 200, byrow = T) - 1/2) / 4) - x),
  z = apply(w, 1, function(x) sample(letters[1:4], 1, prob = x)) %>% 
    factor
) %>% 
  group_by(z) %>% 
  mutate(
    r = rnorm(1) * 5
  ) %>% 
  ungroup %>% 
  mutate(
    y = 3 * x + rnorm(100, 0.1) + r
  )

data %>%
  ggplot(aes(x, y, color = z)) +
  geom_point()

gam(y ~ x, data = data, method = "REML") %>%
  appraise()

gam(y ~ x + s(z, bs = "re"), data = data, method = "REML") %>%
  appraise()

# "fs"
data <- tibble(
  x = runif(200),
  w = abs(((matrix(rep(1:4, 200), 200, byrow = T) - 1/2) / 4) - x),
  z = apply(w, 1, function(x) sample(letters[1:4], 1, prob = x)) %>% 
    factor
) %>% 
  group_by(z) %>% 
  mutate(
    r = get_sfun(c(0, 1), ctrl.pts=5)(x) * 5
  ) %>% 
  ungroup %>% 
  mutate(
    y = 3 * sfun(x) + rnorm(100, 0.1) + r
  )

data %>%
  ggplot(aes(x, y, color = z)) +
  geom_point()

gam(y ~ s(x), data = data, method = "REML") %>%
  appraise()

gam(y ~ s(x, z, bs = "fs"), data = data, method = "REML") %>%
  appraise()

# by
gam(y ~ s(x, by = z), data = data, method = "REML") %>%
  appraise()

data %>% 
  reframe(
    z = unique(z)
  ) %>% 
  group_by(z) %>% 
  reframe(
    x = seq(0, 1, length.out = 1000)
  ) %>% 
  ungroup %>% 
  add_fitted(gam(y ~ s(x), data = data, method = "REML")) %>% 
  rename("s" = .fitted)  %>% 
  add_fitted(gam(y ~ s(x, z, bs = "fs"), data = data, method = "REML")) %>%
  rename("fs" = .fitted) %>%
  add_fitted(gam(y ~ s(x, by = z), data = data, method = "REML")) %>%
  rename("by" = .fitted) %>%
  pivot_longer(c(s, fs, by), names_to = "Smooth", values_to = "Fitted") %>%
  ggplot(aes(x, y, color = z)) +
  geom_point(data = data) +
  geom_line(aes(y = Fitted), size = 1) +
  guides(color = guide_none()) +
  facet_wrap(~Smooth)


par(mfrow = c(2, 2))

gam(y ~ s(x, by = z, k = 3), data = data, method = "REML") %>% 
  gam.check


