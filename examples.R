library(magrittr)
library(tibble)

library(AppliedPredictiveModeling)
data("schedulingData")

str(schedulingData)

num_vars <- unlist(lapply(schedulingData[, -8], is.numeric))
num_vars <- as.formula(paste0("~", paste0(names(num_vars)[num_vars], collapse = "+")))

cat_vars <- unlist(lapply(schedulingData[, -8], is.factor))
cat_vars <- as.formula(paste0("~", paste0(names(cat_vars)[cat_vars], collapse = "+")))


pp <- recipe()
pp <- pp %>% 
  add_role("Class", role = "response") %>% # or add_response(~Class)
  add_role(names(schedulingData)[-8])

pp <- as.recipe(Class ~ ., data = schedulingData)

normalized <- pp %>% standardize(form = num_vars)

with_comp <- normalized %>% pca_extract(form = ~. -Class, data = schedulingData)
with_comp


with_ints <- with_comp %>% interact(~(Protocol+Compounds+Iterations)^3)
with_ints


library(QSARdata)





