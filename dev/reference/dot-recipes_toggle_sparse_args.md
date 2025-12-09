# Toggle all auto sparse arguments

Toggle all auto sparse arguments

## Usage

``` r
.recipes_toggle_sparse_args(x, choice)
```

## Arguments

- x:

  A recipe.

- choice:

  A character string for separating values.

## Value

A recipe

## Details

If a step has an argument `sparse = "auto"`, then workflows can use this
function to fill these values with the preferred action. This preferred
action is calculated in workflows dependent on the model and data
heuristics. Hence why it has to be passed in.

Only arguments where `sparse = "auto"` are affected, thus a user can set
`sparse = "no"` and it will be respected.
