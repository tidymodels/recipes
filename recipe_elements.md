Elements of a data recipe:

 * a collection of the raw variables and their roles and data types (if available). An example would be
```
     variable      role    type
        <chr>     <chr>   <chr>
1       Class  response  factor
2    Protocol predictor  factor
3   Compounds predictor numeric
4 InputFields predictor numeric
5  Iterations predictor numeric
6  NumPending predictor numeric
7        Hour predictor numeric
8         Day predictor  factor
```

 * any tags used (not yet implemented)
 * the verbs, in the order specified, that are used on the data


The contents of the verbs will be: 

* a label for verbosity in a slot called `verb`
* a formula indicating which variables are to be processed (`inputs`)
* any options that are specified in the call that are not formulas (`arg`)
* the computational function used to initially process the data (`compute`)
* the application function that applies the processing to any data set (`apply`)
* the `values` obtained by applying the method to the original dataset. This can have almost anything but should include a character vectors 
  * `add` indicating which columns will be added to the training set (think principal components or basis functions),
  * `remove` specifies columns should be eliminated after the processing (for predictor filters or removing the original columns after PCA), or
  * `replace` for which columns are put back into the data set
* the required packages (I think that we should do this)



Questions:

 1. What is the API for getting the predictors, case weights, outcome, or everything? 
 2. Should we always keep the data in a tibble? I think so
 3. It looks like we will be using `model.matrix` or `model.frame` to get the data form the formula. This kinda sucks especially since it saves the `terms` attribute repeatedly. I'm wonder if we can (eventually) do something more efficient.  