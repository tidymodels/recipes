# butcher

<details>

* Version: 0.1.4
* GitHub: https://github.com/tidymodels/butcher
* Source code: https://github.com/cran/butcher
* Date/Publication: 2021-03-19 05:20:15 UTC
* Number of recursive dependencies: 186

Run `revdep_details(, "butcher")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-recipe.R:426:3): recipe + step_upsample + axe_env() works ───────
      Error: could not find function "step_upsample"
      Backtrace:
          █
       1. └─recipe(~., data = okc) %>% step_upsample(diet, ratio = 0.0121) test-recipe.R:426:2
      ── Error (test-recipe.R:521:3): recipe + step_downsample + axe_env() works ─────
      Error: could not find function "step_downsample"
      Backtrace:
          █
       1. └─recipe(~., data = okc) %>% step_downsample(diet) test-recipe.R:521:2
      
      [ FAIL 2 | WARN 0 | SKIP 35 | PASS 101 ]
      Error: Test failures
      Execution halted
    ```

# caret

<details>

* Version: 6.0-86
* GitHub: https://github.com/topepo/caret
* Source code: https://github.com/cran/caret
* Date/Publication: 2020-03-20 10:20:07 UTC
* Number of recursive dependencies: 179

Run `revdep_details(, "caret")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("caret")
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      ● On CRAN (31)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test_recipe_upsample.R:15:1): (code run outside of `test_that()`) ────
      Error: could not find function "step_upsample"
      Backtrace:
          █
       1. └─`%>%`(...) test_recipe_upsample.R:15:0
      
      [ FAIL 1 | WARN 0 | SKIP 31 | PASS 144 ]
      Error: Test failures
      Execution halted
    ```

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        R        2.0Mb
        data     1.5Mb
        models   2.4Mb
    ```

# MachineShop

<details>

* Version: 2.7.0
* GitHub: https://github.com/brian-j-smith/MachineShop
* Source code: https://github.com/cran/MachineShop
* Date/Publication: 2021-03-02 19:10:12 UTC
* Number of recursive dependencies: 195

Run `revdep_details(, "MachineShop")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MachineShop-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ModeledInput
    > ### Title: ModeledInput Classes
    > ### Aliases: ModeledInput ModeledFrame ModeledRecipe ModeledInput.formula
    > ###   ModeledInput.matrix ModeledInput.ModelFrame ModeledInput.recipe
    > ###   ModeledInput.MLModel ModeledInput.MLModelFunction
    > 
    > ### ** Examples
    ...
    > 
    > rec <- recipe(sale_amount ~ ., data = ICHomes)
    > mod_rec <- ModeledInput(rec, model = GLMModel)
    > fit(mod_rec)
    Warning in `[<-.data.frame`(`*tmp*`, names(extras), value = list(`(weights)` = 1)) :
      replacement element 1 has 1 row to replace 0 rows
    Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
      contrasts can be applied only to factors with 2 or more levels
    Calls: fit ... model.matrix -> model.matrix.default -> contrasts<-
    Execution halted
    ```

