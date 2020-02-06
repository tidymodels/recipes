
# recipes <img src="man/figures/logo.png" align="right" height="80px"/>

[![Build
Status](https://travis-ci.org/tidymodels/recipes.svg?branch=master)](https://travis-ci.org/tidymodels/recipes)
[![R build
status](https://github.com/tidymodels/recipes/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/recipes)
[![Coverage
status](https://codecov.io/gh/tidymodels/recipes/branch/master/graph/badge.svg)](https://codecov.io/github/tidymodels/recipes?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/recipes)](http://cran.r-project.org/web/packages/recipes)
[![Downloads](http://cranlogs.r-pkg.org/badges/recipes)](http://cran.rstudio.com/package=recipes)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

## Introduction

The `recipes` package is an alternative method for creating and
preprocessing design matrices that can be used for modeling or
visualization. From
[wikipedia](https://en.wikipedia.org/wiki/Design_matrix):

> In statistics, a **design matrix** (also known as regressor matrix or
> model matrix) is a matrix of values of explanatory variables of a set
> of objects, often denoted by X. Each row represents an individual
> object, with the successive columns corresponding to the variables and
> their specific values for that object.

While R already has long-standing methods for creating these matrices
(e.g. [formulas](https://rviews.rstudio.com/2017/02/01/the-r-formula-method-the-good-parts/)
and `model.matrix`), there are some [limitations to what the existing
infrastructure can
do](https://rviews.rstudio.com/2017/03/01/the-r-formula-method-the-bad-parts/).

The idea of the `recipes` package is to define a recipe or blueprint
that can be used to sequentially define the encodings and preprocessing
of the data (i.e. “feature engineering”). For example, to create a
simple recipe containing only an outcome and predictors and have the
predictors centered and scaled:

``` r
library(recipes)
library(mlbench)
data(Sonar)
sonar_rec <- recipe(Class ~ ., data = Sonar) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())
```

To install it, use:

``` r
install.packages("recipes")

## for development version:
require("devtools")
install_github("tidymodels/recipes")
```
