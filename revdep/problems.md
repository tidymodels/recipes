# caret

Version: 6.0-80

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        R        2.0Mb
        data     1.5Mb
        models   2.4Mb
    ```

# embed

Version: 0.0.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 4 SKIPPED: 10 FAILED: 10
      1.  Error: factor encoded predictor (@test_mixed.R#12) 
      2.  Error: character encoded predictor (@test_mixed.R#68) 
      3.  Error: factor encoded predictor (@test_mixed.R#127) 
      4.  Error: character encoded predictor (@test_mixed.R#183) 
      5.  Error: printing (@test_mixed.R#259) 
      6.  Error: factor encoded predictor (@test_no_pooling.R#12) 
      7.  Error: character encoded predictor (@test_no_pooling.R#68) 
      8.  Error: factor encoded predictor (@test_no_pooling.R#127) 
      9.  Error: character encoded predictor (@test_no_pooling.R#183) 
      10. Error: printing (@test_no_pooling.R#259) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    Warning: declared S3 method 'tidy.step_embed' not found
    Warning: declared S3 method 'tidy.step_lencode_bayes' not found
    Warning: declared S3 method 'tidy.step_lencode_glm' not found
    Warning: declared S3 method 'tidy.step_lencode_mixed' not found
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# healthcareai

Version: 2.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘healthcareai-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: evaluate
    > ### Title: Get model performance metrics
    > ### Aliases: evaluate evaluate.predicted_df evaluate.model_list
    > 
    > ### ** Examples
    > 
    > models <- machine_learn(pima_diabetes[1:40, ],
    +                        patient_id,
    +                        outcome = diabetes,
    +                        models = c("XGB", "RF"),
    +                        tune = FALSE,
    +                        n_folds = 3)
    Training new data prep recipe...
    
    Error in lapply(newdata[vars], function(x) { : 
      argument "newdata" is missing, with no default
    Calls: machine_learn ... <Anonymous> -> prep.recipe -> bake -> bake.step_missing -> lapply
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat-5.R’ failed.
    Last 13 lines of output:
      5: bake(x$steps[[i]], new_data = training)
      6: bake.step_missing(x$steps[[i]], new_data = training)
      7: lapply(newdata[vars], function(x) {
             levels(x) <- c(levels(x), "missing")
             x
         })
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 0 SKIPPED: 0 FAILED: 1
      1. Error: the fundamentals work (@test-cran_only.R#4) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      replacing previous import 'recipes::tidy' by 'broom::tidy' when loading 'healthcareai' 
      Execution halted
    ```

*   checking whether package ‘healthcareai’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘recipes::tidy’ by ‘broom::tidy’ when loading ‘healthcareai’
    See ‘/Users/max/github/recipes/revdep/checks.noindex/healthcareai/new/healthcareai.Rcheck/00install.out’ for details.
    ```

# modelgrid

Version: 1.1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘lattice’
      All declared Imports should be used.
    ```

# rsample

Version: 0.0.2

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        intersect, setdiff, setequal, union
    
    
    Attaching package: 'recipes'
    
    The following object is masked from 'package:rsample':
    
        prepper
    
    The following object is masked from 'package:broom':
    
        tidy
    
    The following object is masked from 'package:stats':
    
        step
    
    Quitting from lines 199-202 (Working_with_rsets.Rmd) 
    Error: processing vignette 'Working_with_rsets.Rmd' failed with diagnostics:
    no applicable method for 'tidy' applied to an object of class "c('rsplit', 'boot_split')"
    Execution halted
    ```

# tidymodels

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘ggplot2’ ‘infer’ ‘pillar’ ‘recipes’ ‘rsample’
      ‘tidyposterior’ ‘tidypredict’ ‘tidytext’ ‘yardstick’
      All declared Imports should be used.
    ```

