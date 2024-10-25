library(magrittr)
library(dplyr)

tidy_predict <- function(model, newdata, type = "link", se = FALSE) {
  # Predict using the model
  pred <- predict(model, newdata = newdata, type = "link", se.fit = T) %>% 
    lapply(function(x) {
      if (is.matrix(x)) {
        x <- x[, 1]
      }
      return(as.vector(x))
    })
  
  # Extract the predictions
  output <- tibble(
    pred = pred$fit
  )
  if (se) {
    se <- pred$se.fit
    linkinv <- gratia::inv_link(model, "location") # Location: mean of the response
    output <- output %>% 
      mutate(
        pred.upper = pred + 1.96 * se,
        pred.lower = pred - 1.96 * se,
        across(everything(), linkinv)
      ) 
  }
  
  return(output) 
}