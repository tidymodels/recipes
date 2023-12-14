# healthcareai

<details>

* Version: 2.5.1
* GitHub: https://github.com/HealthCatalyst/healthcareai-r
* Source code: https://github.com/cran/healthcareai
* Date/Publication: 2022-09-05 14:50:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::revdep_details(, "healthcareai")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘cli’
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘healthcareai-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: evaluate
    > ### Title: Get model performance metrics
    > ### Aliases: evaluate evaluate.predicted_df evaluate.model_list
    > 
    > ### ** Examples
    > 
    > models <- machine_learn(pima_diabetes[1:40, ],
    ...
    +                        models = c("XGB", "RF"),
    +                        tune = FALSE,
    +                        n_folds = 3)
    Training new data prep recipe...
    
    Variable(s) ignored in prep_data won't be used to tune models: patient_id
    Error in setup_training(d, rlang::enquo(outcome), model_class, models,  : 
      All predictors must be numeric, but the following variables are not numeric. Consider using prep_data to get data ready for model training: weight_class
    Calls: machine_learn -> flash_models -> setup_training
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat-1.R’
      Running ‘testthat-2.R’
      Running ‘testthat-3.R’
      Running ‘testthat-4.R’
      Running ‘testthat-5.R’
     ERROR
    Running the tests in ‘tests/testthat-5.R’ failed.
    Last 13 lines of output:
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      
    ...
          metric, positive_class, n_folds)`: All predictors must be numeric, but the following variables are not numeric. Consider using prep_data to get data ready for model training: weight_class
      Backtrace:
          ▆
       1. └─healthcareai::machine_learn(...) at test-cran_only.R:4:3
       2.   └─healthcareai::flash_models(...)
       3.     └─healthcareai:::setup_training(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    prep:
      function(x, ...)
    prep.step_add_levels:
      function(x, training, info)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    setup_training: no visible binding for global variable ‘is_numeric’
    Undefined global functions or variables:
      is_numeric
    ```

