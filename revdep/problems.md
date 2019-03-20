# caret

Version: 6.0-81

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        R        2.0Mb
        data     1.5Mb
        models   2.4Mb
    ```

# customsteps

Version: 0.7.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rlang’ ‘tidyselect’
      All declared Imports should be used.
    ```

# easyalluvial

Version: 0.1.8

## Newly broken

*   checking examples ... ERROR
    ```
    ...
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    > data = as_tibble(mtcars)
    > categoricals = c('cyl', 'vs', 'am', 'gear', 'carb')
    > numericals = c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec')
    > max_variables = 5
    > 
    > data = data %>%
    +   mutate_at( vars(categoricals), as.factor )
    > 
    > 
    > alluvial_wide( data = data
    +                 , max_variables = max_variables
    +                 , fill_by = 'first_variable' )
    Error: No role currently exists for column(s): 'easyalluvialid'. Please use `update_role()` instead.
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      14: add_role(., easyalluvialid, new_role = "id variable")
      15: stop(glue::glue("No role currently exists for column(s): {vars}. Please use ", "`update_role()` instead."), 
             call. = FALSE)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 6 SKIPPED: 9 FAILED: 6
      1. Error: alluvial_long (@test_alluvial_long.R#94) 
      2. Error: alluvial_wide (@test_alluvial_wide.R#17) 
      3. Error: manip_bin_numerics (@test_manip.R#28) 
      4. Error: manip_bin_numerics zero variance columns (@test_manip.R#84) 
      5. Error: manip_bin_numerics with vector (@test_manip.R#96) 
      6. Error: plot condensation (@test_plot_condensation.R#15) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# MachineShop

Version: 1.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘polspline’
      All declared Imports should be used.
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

# rbin

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# textrecipes

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stringr’
      All declared Imports should be used.
    ```

# tidymodels

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘dials’ ‘parsnip’
      All declared Imports should be used.
    ```

