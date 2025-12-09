# Create interaction variables

`step_interact()` creates a *specification* of a recipe step that will
create new columns that are interaction terms between two or more
variables.

## Usage

``` r
step_interact(
  recipe,
  terms,
  role = "predictor",
  trained = FALSE,
  objects = NULL,
  sep = "_x_",
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("interact")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- terms:

  A traditional R formula that contains interaction terms. This can
  include `.` and selectors. See
  [`selections()`](https://recipes.tidymodels.org/dev/reference/selections.md)
  for more details, and consider using
  [`tidyselect::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html)
  when dummy variables have been created.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- objects:

  A list of `terms` objects for each individual interaction.

- sep:

  A character value used to delineate variables in an interaction (e.g.
  `var1_x_var2` instead of the more traditional `var1:var2`).

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `TRUE`.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/dev/reference/bake.md)?
  While all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md) is
  run, some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

`step_interact()` can create interactions between variables. It is
primarily intended for **numeric data**; categorical variables should
probably be converted to dummy variables using
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md)
prior to being used for interactions.

Unlike other step functions, the `terms` argument should be a
traditional R model formula but should contain no inline functions (e.g.
`log`). For example, for predictors `A`, `B`, and `C`, a formula such as
`~A:B:C` can be used to make a three way interaction between the
variables. If the formula contains terms other than interactions (e.g.
`(A+B+C)^3`) only the interaction terms are retained for the design
matrix.

The separator between the variables defaults to "`_x_`" so that the
three way interaction shown previously would generate a column named
`A_x_B_x_C`. This can be changed using the `sep` argument.

When dummy variables are created and are used in interactions, selectors
can help specify the interactions succinctly. For example, suppose a
factor column `X` gets converted to dummy variables `x_2`, `x_3`, ...,
`x_6` using
[`step_dummy()`](https://recipes.tidymodels.org/dev/reference/step_dummy.md).
If you wanted an interaction with numeric column `z`, you could create a
set of specific interaction effects (e.g. `x_2:z + x_3:z` and so on) or
you could use `starts_with("x_"):z`. When
[`prep()`](https://recipes.tidymodels.org/dev/reference/prep.md)
evaluates this step, `starts_with("x_")` resolves to
`(x_2 + x_3 + x_4 + x_5 + x_6)` so that the formula is now
`(x_2 + x_3 + x_4 + x_5 + x_6):z` and all two-way interactions are
created.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/dev/reference/tidy.recipe.md)
this step, a tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## Examples

``` r
data(penguins, package = "modeldata")
penguins <- penguins |> na.omit()

rec <- recipe(flipper_length_mm ~ ., data = penguins)

int_mod_1 <- rec |>
  step_interact(terms = ~ bill_depth_mm:bill_length_mm)

# specify all dummy variables succinctly with `starts_with()`
int_mod_2 <- rec |>
  step_dummy(sex, species, island) |>
  step_interact(terms = ~ body_mass_g:starts_with("species"))

int_mod_1 <- prep(int_mod_1, training = penguins)
int_mod_2 <- prep(int_mod_2, training = penguins)

dat_1 <- bake(int_mod_1, penguins)
dat_2 <- bake(int_mod_2, penguins)

names(dat_1)
#> [1] "species"                        "island"                        
#> [3] "bill_length_mm"                 "bill_depth_mm"                 
#> [5] "body_mass_g"                    "sex"                           
#> [7] "flipper_length_mm"              "bill_depth_mm_x_bill_length_mm"
names(dat_2)
#>  [1] "bill_length_mm"                 
#>  [2] "bill_depth_mm"                  
#>  [3] "body_mass_g"                    
#>  [4] "flipper_length_mm"              
#>  [5] "sex_male"                       
#>  [6] "species_Chinstrap"              
#>  [7] "species_Gentoo"                 
#>  [8] "island_Dream"                   
#>  [9] "island_Torgersen"               
#> [10] "body_mass_g_x_species_Chinstrap"
#> [11] "body_mass_g_x_species_Gentoo"   

tidy(int_mod_1, number = 1)
#> # A tibble: 1 × 2
#>   terms                        id            
#>   <chr>                        <chr>         
#> 1 bill_depth_mm:bill_length_mm interact_WkM5w
tidy(int_mod_2, number = 2)
#> # A tibble: 2 × 2
#>   terms                         id            
#>   <chr>                         <chr>         
#> 1 body_mass_g:species_Chinstrap interact_7qfjM
#> 2 body_mass_g:species_Gentoo    interact_7qfjM
```
