# recipes 0.0.1.9001

* The class system for `recipe` objects was changed so that [pipes can be used to create the recipe with a formula](https://github.com/topepo/recipes/issues/46).
* [`process.recipe`](https://topepo.github.io/recipes/reference/process.html) lost the `role` argument in factor of a general set of [selectors](https://topepo.github.io/recipes/articles/Selecting_Variables.html). If no selector is used, all the predictors are returned. 