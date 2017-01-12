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

## This doesn't currently work since, right now, the formula
## is evaluated on the data _prior_ to the `learn` function. 

standardized <- standardized %>% 
  step_nzv(formula = ~ Protocol + Compounds + InputFields + Iterations + NumPending + Hour + Day)

## The effect is that variables that exist before `learn` may not
## be there. 

## If we wait until `learn` to evaluate the formula:
## 
## - Do we even need the data and `var_info` prior to that? 
##   I don't think so. 
## - We would have to check that the columns exist each time
##   the calculations are executed
## - We wouldn't;'t be able to pre-train the recipe. This may 
##   be good to avoid leakage but also slows things down and 
##   you might get errors during model training

## If we keep things as-is in terms of the formulas: 
## 
## - We would need to keep and revaluate the formulas during
##   learning anyway
## - We could just ignore columns that are not there



