library(magrittr)
library(tibble)

library(AppliedPredictiveModeling)
data("schedulingData")

str(schedulingData)

num_vars <- unlist(lapply(schedulingData[, -8], is.numeric))
num_vars <- names(num_vars)[num_vars]

cat_vars <- unlist(lapply(schedulingData[, -8], is.factor))
cat_vars <- names(cat_vars)[cat_vars]


pp <- recipe()
pp <- pp %>% 
  add_response("Class") %>%
  add_predictor(names(schedulingData)[-8])

normalized <- pp %>% standardize(features = num_vars)

with_comp <- normalized %>% pca_extract(features = num_vars)

with_ints <- with_comp %>% interact(~ (Protocol+Compound+Iterations)^3)
