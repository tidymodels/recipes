# recipes 0.0.1.9002

New steps:

  * `step_lincomb` removes variables involved in linear combinations to resolve them. 
  * A step for converting binary variables to factors (`step_bin2factor`)
  *  `step_regex` applies a regular expression to a character or factor vector to create dummy variables. 

Other changes: 

* `step_dummy` and `step_interact` do a better job of respecting missing values in the data set. 


# recipes 0.0.1.9001

* The class system for `recipe` objects was changed so that [pipes can be used to create the recipe with a formula](https://github.com/topepo/recipes/issues/46).
* [`process.recipe`](https://topepo.github.io/recipes/reference/process.html) lost the `role` argument in factor of a general set of [selectors](https://topepo.github.io/recipes/articles/Selecting_Variables.html). If no selector is used, all the predictors are returned. 
* Two steps for simple imputation using the mean or mode were added. 
