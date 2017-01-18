# Data Recipes



This is a document to explore an approach to pre-processing data for modeling. The idea is to define a plan/recipe/blueprint that can be used to eventually create a design matrix,  outcome array, or other collections of data.

## Definitions

* __variables__ are the original data columns in a data frame or tibble. For example, in the formula `Y ~ A + B + A:B`, the variables are `A`, `B`, and `Y`. 
* __roles__ define how variables will be used in the model. Examples are: `predictor` (independent variables), `response`, and `case weight`. This is meant to be open-ended and extensible. Some rules will exist such as "outcomes must be main effects" (e.g. not interactions) or "certain operations can only be used on predictors", etc. 
* __terms__ are columns in a design matrix such as `A`, `B`, and `A:B`. These can be other derived entities that are grouped such a a set of principal components or a set of columns that define a basis function for a variable. These are synonymous with features in machine learning. Variables that have `predictor` roles would automatically be main effect terms  

## The General Process

To write a recipe, the process would be:
  
1. Define the variables and their roles. This can be done in the absence of any data set. 
1. By default, the predictors are considered main effects for the model. If more terms are needed they can be added by a sequence of __verbs__, which are operations on one or more variables or terms. For example, creating an interaction effect or logging a variable are verbs. Lists of verbs are below. 
1. When the recipe is complete, an initial data set can be used to validate or __check__ the recipe. This evaluates common roles based on the data types (e.g. logging a categorical predictor). This process might eliminate or alter verbs. Note that no statistical quantities have been estimated so some rules cannot be validated until the final data types of all terms (existing or derived) are available. The checking process can also find logical inconsistencies (for example, centering the data then estimating a Box-Cox transformation). This step might be optional but we would want to avoid during this repeatedly (i.e. during resampling)

The recipe is trained from a data set. Here, the verbs are taken with the training set and all of the statistical quantities are estimated. At this point, any derived terms are defined such as creating columns for interactions or the determination of the principal components. 

The recipe and the associated verbs are then applied to a data set. The data could be the original data set or a new set of data points being predicted.



### Verbs

Below are example classes of verbs and their characteristics. Each verb might replace the existing terms, remove terms, or add derived terms. They may also affect the rows of the data set too.
                                                                                                  
* *standardization* (replacement, one:one): centering, scaling, and other re-encoding of individual terms. 
* *interactions* or *nesting* (derived, many:many)
* *imputation* (replacement, many:one): models or rules are used to replace missing terms. 
* encoding *dummy variables* from categorical data (derived, one:many)
* *transformations* of data (replacement, one:one or many:many): this includes logging, Box-Cox, arc-sin, spatial sign or other transformations of data. This also encompasses cases where a date variable is transformed into one or more terms (e.g. day of the week, month number).
* *binning* (derived, one:many): the unfortunate process of creating categories from numeric data
* feature *extraction* (derived, multivariate): PCA, ICA, autoencoders, basis functions, and other procedures that estimate new terms from the original data
* *filters* (removal): filters on zero-variance terms or for reducing correlations between predictors. 
* *subsampling* (changes to rows): procedures to remove or create new *rows* of the data set. Examples are down-sampling or SMOTE sampling procedures for class imbalances. 

## An Example

The data used to demonstrate recipes will be the high performance computing example from _Applied Predictive Modeling_:


```r
library(AppliedPredictiveModeling)
data("schedulingData")

str(schedulingData)
```

```
## 'data.frame':	4331 obs. of  8 variables:
##  $ Protocol   : Factor w/ 14 levels "A","C","D","E",..: 4 4 4 4 4 4 4 4 4 4 ...
##  $ Compounds  : num  997 97 101 93 100 100 105 98 101 95 ...
##  $ InputFields: num  137 103 75 76 82 82 88 95 91 92 ...
##  $ Iterations : num  20 20 10 20 20 20 20 20 20 20 ...
##  $ NumPending : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Hour       : num  14 13.8 13.8 10.1 10.4 ...
##  $ Day        : Factor w/ 7 levels "Mon","Tue","Wed",..: 2 2 4 5 5 3 5 5 5 3 ...
##  $ Class      : Factor w/ 4 levels "VF","F","M","L": 2 1 1 1 1 1 1 1 1 1 ...
```

We want to predict the `Class` column as a function of the others. For some models, we would need to center and scale the predictors, estimate some univariate transformations, and also convert the data to dummy variables. 

First, split the data into training and testing:


```r
library(caret)
set.seed(34188)
in_train <- createDataPartition(schedulingData$Class, p = .75, list = FALSE)

hpc_train <- schedulingData[ in_train,]
hpc_test  <- schedulingData[-in_train,]
```

I'll define some formulas up-front to use the specify variables and terms (a different and better interface is needed here; see below).


```r
# Based on variable types
num_vars <- ~ Compounds + InputFields + Iterations + NumPending + Hour
cat_vars <- ~ Protocol + Day
```

To start, a data set and role specification is required. The easiest way to do that is to use the model formula approach: 


```r
library(recipes)
raw <- recipe(Class ~ ., data = hpc_train)
raw$var_info
```

```
## # A tibble: 8 × 4
##      variable    type      role   source
##         <chr>   <chr>     <chr>    <chr>
## 1    Protocol nominal predictor original
## 2         Day nominal predictor original
## 3       Class nominal   outcome original
## 4   Compounds numeric predictor original
## 5 InputFields numeric predictor original
## 6  Iterations numeric predictor original
## 7  NumPending numeric predictor original
## 8        Hour numeric predictor original
```

A lower-level API is available that will allow for different roles that are manually created. For example: 


```r
data_set <- recipe(data = hpc_train)
data_set <- data_set %>%
  add_role("Class", role = "response") %>%
  add_role(num_vars, role = "predictor") %>%
  add_role(~ obs_counts, role = "case_weight") %>%
  add_role(~ variable, role = "something else") # etc
```

Back to the HPC data, a simple preprocessing specification could be:


```r
library(magrittr)
standardized <- raw %>% 
  step_center(num_vars) %>% 
  step_scale(num_vars) %>%
  step_dummy(cat_vars)
standardized
```

```
## Data Recipe
## 
## Inputs:
## 
##       role #variables
##    outcome          1
##  predictor          7
## 
## Steps:
## 
## Centering with Compounds + InputFields + Iterations + {more}
## Scaling with Compounds + InputFields + Iterations + {more}
## Dummy variables from Protocol + Day
```

We haven't done anything here beyond stating the _plan_ for data processing.

Order matters here; if we were interested in making sure that the resulting dummy variables are on the same scale as the others, we would switch the order. The downside is that the simple formula will need to know the names of the dummy variables columns that have not been created yet. In some cases, such as PCA, we won't even know the names or numbers of the predictors at the time that the formula needs to be specified. 

As an example of that issue, we can create an alternate workflow that filters the predictors that are unbalanced and sparse (aka "near zero variance" columns):


```r
filtered <- standardized %>% 
  step_nzv(~ Compounds + InputFields + Iterations + NumPending + Hour + 
             Protocol_C + Protocol_D + Protocol_E + Protocol_F + Protocol_G + 
             Protocol_H + Protocol_I + Protocol_J + Protocol_K + Protocol_L + 
             Protocol_M + Protocol_N + Protocol_O + Day_Tue + Day_Wed + Day_Thu + 
             Day_Fri + Day_Sat + Day_Sun)
filtered
```

```
## Data Recipe
## 
## Inputs:
## 
##       role #variables
##    outcome          1
##  predictor          7
## 
## Steps:
## 
## Centering with Compounds + InputFields + Iterations + {more}
## Scaling with Compounds + InputFields + Iterations + {more}
## Dummy variables from Protocol + Day
## Near-zero variance filter on Compounds + InputFields + Iterations + {more}
```

We can start the actual computations using the `learn` function:


```r
filtered <- learn(filtered, training = hpc_train, verbose = FALSE)
filtered
```

```
## Data Recipe
## 
## Inputs:
## 
##       role #variables
##    outcome          1
##  predictor          7
## 
## Steps:
## 
## Centering with Compounds + InputFields + Iterations + {more} [learned]
## Scaling with Compounds + InputFields + Iterations + {more} [learned]
## Dummy variables from Protocol + Day [learned]
## Near-zero variance filter on Compounds + InputFields + Iterations + {more} [learned]
```
**aside**: As the recipe is trained, we keep track of the changes at each step:  

```r
filtered$term_info
```

```
## # A tibble: 16 × 4
##       variable    type      role   source
##          <chr>   <chr>     <chr>    <chr>
## 1        Class nominal   outcome original
## 2    Compounds numeric predictor original
## 3  InputFields numeric predictor original
## 4   Iterations numeric predictor original
## 5         Hour numeric predictor original
## 6   Protocol_H numeric predictor  derived
## 7   Protocol_I numeric predictor  derived
## 8   Protocol_J numeric predictor  derived
## 9   Protocol_L numeric predictor  derived
## 10  Protocol_M numeric predictor  derived
## 11  Protocol_N numeric predictor  derived
## 12  Protocol_O numeric predictor  derived
## 13     Day_Tue numeric predictor  derived
## 14     Day_Wed numeric predictor  derived
## 15     Day_Thu numeric predictor  derived
## 16     Day_Fri numeric predictor  derived
```
**/aside**

To apply the trained recipe to any data set:

```r
filtered_test <- process(filtered, newdata = hpc_test)
```

### Interface Ideas

One outstanding item to figure out is how to allow variables, which may not exist at the time, to be specified for steps. 

Right now, I'm using simple formulas but it would be good to allow the user to specify them by

* name (via some regex)
* type ("all numeric")
* role ("all predictors")

or some combination of these. The object to "hit" is the `terms_info` tibble that is changed as each step is processed. For example:


```r
library(dplyr)
filtered$term_info %>% 
  filter(source == "derived" & role == "predictor") %>%
  select(variable)
```

```
## # A tibble: 11 × 1
##      variable
##         <chr>
## 1  Protocol_H
## 2  Protocol_I
## 3  Protocol_J
## 4  Protocol_L
## 5  Protocol_M
## 6  Protocol_N
## 7  Protocol_O
## 8     Day_Tue
## 9     Day_Wed
## 10    Day_Thu
## 11    Day_Fri
```
Of course, this won't work as an argument so some magical-functional-programming-with-a-formula is probably the right direction.
