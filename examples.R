library(magrittr)
library(tibble)
library(caret)

library(AppliedPredictiveModeling)
data("schedulingData")

str(schedulingData)

num_vars <- unlist(lapply(schedulingData[, -8], is.numeric))
num_vars <- as.formula(paste0("~", paste0(names(num_vars)[num_vars], collapse = "+")))

cat_vars <- unlist(lapply(schedulingData[, -8], is.factor))
cat_vars <- as.formula(paste0("~", paste0(names(cat_vars)[cat_vars], collapse = "+")))

set.seed(34188)
in_train <- createDataPartition(schedulingData$Class, p = .75, list = FALSE)

hpc_train <- schedulingData[ in_train,]
hpc_test  <- schedulingData[-in_train,]

pp <- recipe()
pp <- pp %>% 
  add_role("Class", role = "response") %>% # or add_role(~Class, role = "response")
  add_role(names(hpc_train)[-8])

pp <- as.recipe(Class ~ ., data = hpc_train)

normalized <- pp %>% standardize(form = num_vars)

## not the final API (I hope)
normalized <- compute_predictors(normalized, training = hpc_train, verbose = TRUE, keep_data = TRUE)
scaled_train <- apply_predictors(normalized, data = hpc_train, verbose = TRUE)
scaled_test  <- apply_predictors(normalized, data = hpc_test,  verbose = TRUE)
all.equal(normalized$data, scaled_train)

with_comp <- normalized %>% pca_extract(form =num_vars, data = schedulingData)
with_comp
with_comp <- compute_predictors(with_comp, training = hpc_train, verbose = TRUE)
comps_train <- apply_predictors(with_comp, data = hpc_train, verbose = TRUE)
comps_test  <- apply_predictors(with_comp, data = hpc_test,  verbose = TRUE)



with_ints <- with_comp %>% interact(~(Protocol+Compounds+Iterations)^3)
with_ints
