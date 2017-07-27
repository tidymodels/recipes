# recipes 0.1.0

First CRAN release. 

* Changed `prepare` to `prep` per [issue #59](https://github.com/topepo/recipes/issues/59)

# recipes 0.0.1.9003

 * Two of the main functions [changed names](https://github.com/topepo/recipes/issues/57). `learn` has become `prepare` and `process` has become `bake`


# recipes 0.0.1.9002

New steps:

  * `step_lincomb` removes variables involved in linear combinations to resolve them. 
  * A step for converting binary variables to factors (`step_bin2factor`)
  *  `step_regex` applies a regular expression to a character or factor vector to create dummy variables. 

Other changes: 

* `step_dummy` and `step_interact` do a better job of respecting missing values in the data set. 


# recipes 0.0.1.9001

* The class system for `recipe` objects was changed so that [pipes can be used to create the recipe with a formula](https://github.com/topepo/recipes/issues/46).
* `process.recipe` lost the `role` argument in factor of a general set of [selectors](https://topepo.github.io/recipes/articles/Selecting_Variables.html). If no selector is used, all the predictors are returned. 
* Two steps for simple imputation using the mean or mode were added. 
