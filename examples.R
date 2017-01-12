library(magrittr)
library(tibble)
library(caret)

library(AppliedPredictiveModeling)
data("schedulingData")

str(schedulingData)

num_vars <- ~ Compounds + InputFields + Iterations + NumPending + Hour
cat_vars <- ~ Protocol + Day

set.seed(34188)
in_train <- createDataPartition(schedulingData$Class, p = .75, list = FALSE)

hpc_train <- schedulingData[ in_train,]
hpc_test  <- schedulingData[-in_train,]

###################################################################

# pp <- recipe()
# pp <- pp %>% 
#   add_role("Class", role = "response") %>% # or add_role(~Class, role = "response")
#   add_role(names(hpc_train)[-8])

raw <- recipe(Class ~ ., data = hpc_train)

standardized <- raw %>% 
  step_center(formula = num_vars) %>% 
  step_scale(formula = num_vars) %>%
  step_dummy(formula = ~ Protocol + Day)

standardized <- learn(standardized)

standarized_test <- process(standardized, newdata = hpc_test)

