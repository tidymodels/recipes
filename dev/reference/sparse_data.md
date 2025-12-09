# Using sparse data with recipes

[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md),
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md), and
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) all
accept sparse tibbles from the `sparsevctrs` package and sparse matrices
from the `Matrix` package. Sparse matrices are converted to sparse
tibbles internally as each step expects a tibble as its input, and is
expected to return a tibble as well.

## Details

Several steps work with sparse data. A step can either work with sparse
data, ruin sparsity, or create sparsity. The documentation for each step
will indicate whether it will work with sparse data or create sparse
columns. If nothing is listed it is assumed to ruin sparsity.

Sparse tibbles or `data.frame`s will be returned from
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md) if
sparse columns are present in data, either from being generated in steps
or because sparse data was passed into
[`recipe()`](https://recipes.tidymodels.org/dev/reference/recipe.md),
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md), or
[`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md).
