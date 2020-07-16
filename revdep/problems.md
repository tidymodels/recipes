# embed

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/embed
* URL: https://embed.tidymodels.org, https://github.com/tidymodels/embed
* BugReports: https://github.com/tidymodels/embed/issues
* Date/Publication: 2020-05-25 05:10:02 UTC
* Number of recursive dependencies: 155

Run `revdep_details(,"embed")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > set.seed(111)
    > in_training <- sample(1:nrow(credit_data), 2000)
    > 
    > credit_tr <- credit_data[ in_training, ]
    > credit_te <- credit_data[-in_training, ]
    > 
    > rec <- recipe(Status ~ ., data = credit_tr) %>%
    +   step_woe(Job, Home, outcome = Status)
    > 
    > woe_models <- prep(rec, training = credit_tr)
    Error: `call` must be a quoted call
    Backtrace:
    [90m    [39m█
    [90m 1. [39m├─recipes::prep(rec, training = credit_tr)
    [90m 2. [39m└─recipes:::prep.recipe(rec, training = credit_tr)
    [90m 3. [39m  └─purrr::map_lgl(x$steps[[i]], is_tune)
    [90m 4. [39m    └─recipes:::.f(.x[[i]], ...)
    [90m 5. [39m      └─rlang::call_name(x)
    [90m 6. [39m        └─rlang:::abort_call_input_type("call")
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12. purrr::map_lgl(x$steps[[i]], is_tune)
       13. recipes:::.f(.x[[i]], ...)
       14. rlang::call_name(x)
       15. rlang:::abort_call_input_type("call")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 222 | SKIPPED: 13 | WARNINGS: 0 | FAILED: 5 ]
      1. Failure: xgb_binning for classification (@test_discretize_xgb.R#194) 
      2. Failure: xgb_binning for multi-classification (@test_discretize_xgb.R#235) 
      3. Failure: xgb_binning for regression (@test_discretize_xgb.R#276) 
      4. Error: step_woe (@test_woe.R#138) 
      5. Error: printing (@test_woe.R#190) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

