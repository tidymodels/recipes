# Using case weights with recipes

Case weights are positive numeric values that may influence how much
each data point has during the preprocessing. There are a variety of
situations where case weights can be used.

## Details

tidymodels packages differentiate *how* different types of case weights
should be used during the entire data analysis process, including
preprocessing data, model fitting, performance calculations, etc.

The tidymodels packages require users to convert their numeric vectors
to a vector class that reflects how these should be used. For example,
there are some situations where the weights should not affect operations
such as centering and scaling or other preprocessing operations.

The types of weights allowed in tidymodels are:

- Frequency weights via
  [`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html)

- Importance weights via
  [`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html)

More types can be added by request.

For recipes, we distinguish between supervised and unsupervised steps.
Supervised steps use the outcome in the calculations, this type of steps
will use frequency and importance weights. Unsupervised steps don't use
the outcome and will only use frequency weights.

There are 3 main principles about how case weights are used within
recipes. First, the data set that is passed to the
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md)
function should already have a case weights column in it. This column
can be created beforehand using
[`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html)
or
[`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html).
Second, There can only be 1 case weights column in a recipe at any given
time. Third, You can not modify the case weights column with most of the
steps or using the
[`update_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
and
[`add_role()`](https://recipes.tidymodels.org/dev/reference/roles.md)
functions.

These principles ensure that you experience minimal surprises when using
case weights, as the steps automatically apply case weighted operations
when supported. The printing method will additionally show which steps
where weighted and which steps ignored the weights because they were of
an incompatible type.

## See also

[`frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html),
[`importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html)
