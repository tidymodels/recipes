
# recipes <a href='https://recipes.tidymodels.org'><img src='man/figures/logo.png' align="right" height="139" /></a>

[![R-CMD-check](https://github.com/tidymodels/recipes/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/recipes/actions)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/recipes/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/recipes?branch=main)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/recipes)](https://CRAN.R-project.org/package=recipes)
[![Downloads](https://cranlogs.r-pkg.org/badges/recipes)](https://CRAN.R-project.org/package=recipes)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

## Introduction

With recipes, you can use [dplyr](https://dplyr.tidyverse.org/)-like
pipeable sequences of feature engineering steps to get your data ready
for modeling. For example, to create a recipe containing an outcome plus
two numeric predictors and then center and scale (“normalize”) the
predictors:

``` r
library(recipes)
data(ad_data, package = "modeldata")

ad_rec <- recipe(Class ~ tau + VEGF, data = ad_data) |>
  step_normalize(all_numeric_predictors())

ad_rec
```

More information on recipes can be found at the [*Get
Started*](https://www.tidymodels.org/start/recipes/) page of
[tidymodels.org](https://www.tidymodels.org).

You may consider recipes as an alternative method for creating and
preprocessing design matrices (also known as model matrices) that can be
used for modeling or visualization. While R already has long-standing
methods for creating such matrices
(e.g. [formulas](https://rviews.rstudio.com/2017/02/01/the-r-formula-method-the-good-parts/)
and `model.matrix`), there are some [limitations to what the existing
infrastructure can
do](https://rviews.rstudio.com/2017/03/01/the-r-formula-method-the-bad-parts/).

## Installation

There are several ways to install recipes:

``` r
# The easiest way to get recipes is to install all of tidymodels:
install.packages("tidymodels")

# Alternatively, install just recipes:
install.packages("recipes")

# Or the development version from GitHub:
# install.packages("pak")
pak::pak("tidymodels/recipes")
```

## Contributing

- For questions and discussions about tidymodels packages, modeling, and
  machine learning, please [post on RStudio
  Community](https://forum.posit.co/c/ml/15).

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/tidymodels/recipes/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code.

- Check out further details on [contributing guidelines for tidymodels
  packages](https://www.tidymodels.org/contribute/) and [how to get
  help](https://www.tidymodels.org/help/).
