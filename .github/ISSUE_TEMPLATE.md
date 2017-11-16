If you are filing a bug, make sure these boxes are checked before submitting your issue— thank you!

- [ ] Start a new R session
- [ ] Install the latest version of of the package: `update.packages(oldPkgs="recipes", ask=FALSE)`
- [ ] [Write a minimal reproducible example](http://stackoverflow.com/a/5963610)
- [ ] run `sessionInfo()` and add the results to the issue. Even better would be to use the [`sessioninfo`](https://github.com/r-lib/sessioninfo) package's `session_info()`.  

### Minimal, reproducible example:

__Please read this page__: [reprex = {repr}oducible {ex}ample](https://github.com/jennybc/reprex#what-is-a-reprex) 

Text and example code modified from [the R FAQ on stackoverflow](http://stackoverflow.com/a/5963610)

#### _Minimal_ Reproducible Dataset:

If you are not using a data set in a package then use e.g. `dput()` to give us something that can be put in R immediately, e.g. 

```r
dput(head(iris,4))
```

Without a dataset, there usually isn't much that we can do to help. 

If your data frame has a factor with many levels, the `dput` output can be unwieldy because it will still list all the possible factor levels even if they aren't present in the the subset of your data. To solve this issue, you can use the `droplevels()` function. Notice below how species is a factor with only one level: `dput(droplevels(head(iris, 4)))`.

#### Minimal, runnable code:

```r
library(recipes)
data(biomass)

biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr)

center_trans <- rec %>%
  step_center(carbon, contains("gen"), -hydrogen)

center_obj <- prep(center_trans, training = biomass_tr)

transformed_te <- bake(center_obj, biomass_te) 
```

### Session Info:

```r
sessionInfo()

# or sessioninfo::session_info()

```

Be sure to test your chunks of code in an empty R session before submitting your issue!