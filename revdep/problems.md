# butcher

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/butcher
* URL: https://tidymodels.github.io/butcher, https://github.com/tidymodels/butcher
* BugReports: https://github.com/tidymodels/butcher/issues
* Date/Publication: 2019-08-09 15:30:02 UTC
* Number of recursive dependencies: 185

Run `revdep_details(,"butcher")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ###   axe_env.step_naomit axe_env.step_nnmf axe_env.step_novel
    > ###   axe_env.step_num2factor axe_env.step_ns axe_env.step_nzv
    > ###   axe_env.step_ordinalscore axe_env.step_other axe_env.step_pca
    > ###   axe_env.step_pls axe_env.step_poly axe_env.step_range
    > ###   axe_env.step_ratio axe_env.step_regex axe_env.step_relu
    > ###   axe_env.step_rm axe_env.step_rollimpute axe_env.step_shuffle
    > ###   axe_env.step_slice axe_env.step_scale axe_env.step_string2factor
    > ###   axe_env.step_sqrt axe_env.step_spatialsign axe_env.step_unorder
    > ###   axe_env.step_upsample axe_env.step_window axe_env.step_YeoJohnson
    > ###   axe_env.step_zv axe_env.quosure
    > 
    > ### ** Examples
    > 
    > suppressWarnings(suppressMessages(library(recipes)))
    > 
    > data(biomass)
    Warning in data(biomass) : data set ‘biomass’ not found
    > 
    > biomass_tr <- biomass[biomass$dataset == "Training",]
    Error: object 'biomass' not found
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(butcher)
      > 
      > test_check("butcher")
      ── 1. Error: (unknown) (@test-recipe.R#12)  ────────────────────────────────────
      object 'biomass' not found
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 29 | SKIPPED: 33 | WARNINGS: 1 | FAILED: 1 ]
      1. Error: (unknown) (@test-recipe.R#12) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# embed

<details>

* Version: 0.0.4
* Source code: https://github.com/cran/embed
* URL: https://tidymodels.github.io/embed
* BugReports: https://github.com/tidymodels/embed/issues
* Date/Publication: 2019-09-15 15:10:09 UTC
* Number of recursive dependencies: 145

Run `revdep_details(,"embed")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘embed-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: step_embed
    > ### Title: Encoding Factors into Multiple Columns
    > ### Aliases: step_embed tidy.step_embed embed_control
    > ### Keywords: datagen
    > 
    > ### ** Examples
    > 
    > data(okc)
    Warning in data(okc) : data set ‘okc’ not found
    > 
    > rec <- recipe(Class ~ age + location, data = okc) %>%
    +   step_embed(location, outcome = vars(Class),
    +              options = embed_control(epochs = 10))
    Error in is_tibble(data) : object 'okc' not found
    Calls: %>% ... eval -> recipe -> recipe.formula -> form2args -> is_tibble
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      > 
      > test_check(package = "embed")
      ── 1. Error: (unknown) (@test_woe.R#9)  ────────────────────────────────────────
      object 'credit_data' not found
      Backtrace:
       1. base::sample(1:nrow(credit_data), 2000)
       2. base::nrow(credit_data)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 112 | SKIPPED: 10 | WARNINGS: 1 | FAILED: 1 ]
      1. Error: (unknown) (@test_woe.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# formulize

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/formulize
* Date/Publication: 2018-01-09 18:23:30 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"formulize")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(formulize)
      > 
      > test_check("formulize")
      ── 1. Error: (unknown) (@test_formulize.R#10)  ─────────────────────────────────
      `-` is not allowed in a recipe formula. Use `step_rm()` instead.
      Backtrace:
       1. recipes::recipe(form, data = mtcars)
       2. recipes:::recipe.formula(form, data = mtcars)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: (unknown) (@test_formulize.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# hardhat

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/hardhat
* URL: https://github.com/tidymodels/hardhat
* BugReports: https://github.com/tidymodels/hardhat/issues
* Date/Publication: 2019-12-16 10:00:09 UTC
* Number of recursive dependencies: 119

Run `revdep_details(,"hardhat")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Failure: Missing y value returns a 0 column tibble for `outcomes` (@test-m
      Expectation did not fail
      
      ℹ Writing skeleton files
      ✔ Setting active project to '/private/var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T/RtmphsiPwv/model'
      ✔ Writing 'R/random_forest-constructor.R'
      ✔ Writing 'R/random_forest-fit.R'
      ✔ Writing 'R/random_forest-predict.R'
      ● Run `devtools::document()`
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 400 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: Missing y value returns a 0 column tibble for `outcomes` (@test-mold-recipe.R#185) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

