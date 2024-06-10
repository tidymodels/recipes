# tidybins

<details>

* Version: 0.1.0
* GitHub: https://github.com/Harrison4192/tidybins
* Source code: https://github.com/cran/tidybins
* Date/Publication: 2021-10-14 12:20:02 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::revdep_details(, "tidybins")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidybins-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bin_cols
    > ### Title: Bin Cols
    > ### Aliases: bin_cols
    > 
    > ### ** Examples
    > 
    > 
    ...
      4.   └─recipes:::prep.recipe(rec2, training = .data)
      5.     ├─recipes:::recipes_error_context(...)
      6.     │ ├─base::withCallingHandlers(...)
      7.     │ └─base::force(expr)
      8.     ├─recipes::prep(x$steps[[i]], training = training, info = x$term_info)
      9.     └─embed:::prep.step_discretize_xgb(x$steps[[i]], training = training, info = x$term_info)
     10.       └─recipes::recipes_eval_select(x$terms, training, info)
     11.         └─cli::cli_abort(...)
     12.           └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘badger’ ‘ggplot2’ ‘lubridate’ ‘scales’ ‘xgboost’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) bin_cols.Rd:50: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) bin_cols.Rd:51: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) bin_cols.Rd:52: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) bin_cols.Rd:53: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) bin_cols.Rd:54: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) bin_cols.Rd:55: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) bin_cols.Rd:56: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) bin_cols.Rd:57: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) bin_cols.Rd:58: Lost braces in \itemize; meant \describe ?
    ```

# tidysdm

<details>

* Version: 0.9.4
* GitHub: https://github.com/EvolEcolGroup/tidysdm
* Source code: https://github.com/cran/tidysdm
* Date/Publication: 2024-03-05 20:30:02 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::revdep_details(, "tidysdm")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      * `Y` must have class <vctrs_unspecified>, not <numeric>.
      Backtrace:
          ▆
       1. ├─recipes::prep(lacerta_rec, training = lacerta_thin %>% sf::st_drop_geometry()) at test_recipe_sf.R:38:3
       2. ├─tidysdm:::prep.spatial_recipe(...)
    ...
       3. ├─base::NextMethod(...)
       4. └─recipes:::prep.recipe(...)
       5.   └─recipes:::check_training_set(training, x, fresh)
       6.     └─recipes::recipes_ptype_validate(...)
       7.       └─cli::cli_abort(msg, call = call)
       8.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 200 ]
      Error: Test failures
      Execution halted
    ```

# viraldomain

<details>

* Version: 0.0.3
* GitHub: NA
* Source code: https://github.com/cran/viraldomain
* Date/Publication: 2024-01-21 13:00:08 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::revdep_details(, "viraldomain")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘viraldomain-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: normalized_domain_plot
    > ### Title: Create a Normalized Domain Plot
    > ### Aliases: normalized_domain_plot
    > 
    > ### ** Examples
    > 
    > data(viral)
    ...
      7.       ├─hardhat::run_mold(blueprint, data = data)
      8.       └─hardhat:::run_mold.default_recipe_blueprint(blueprint, data = data)
      9.         └─hardhat:::mold_recipe_default_process(...)
     10.           ├─recipes::prep(...)
     11.           └─recipes:::prep.recipe(...)
     12.             └─recipes:::check_training_set(training, x, fresh)
     13.               └─recipes::recipes_ptype_validate(...)
     14.                 └─cli::cli_abort(msg, call = call)
     15.                   └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8.     ├─hardhat::mold(x, data)
        9.     └─hardhat:::mold.recipe(x, data)
       10.       ├─hardhat::run_mold(blueprint, data = data)
       11.       └─hardhat:::run_mold.default_recipe_blueprint(blueprint, data = data)
       12.         └─hardhat:::mold_recipe_default_process(...)
       13.           ├─recipes::prep(...)
       14.           └─recipes:::prep.recipe(...)
       15.             └─recipes:::check_training_set(training, x, fresh)
       16.               └─recipes::recipes_ptype_validate(...)
       17.                 └─cli::cli_abort(msg, call = call)
       18.                   └─rlang::abort(...)
      
      [ FAIL 2 | WARN 0 | SKIP 3 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

