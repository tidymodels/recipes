# shrunken centroids

    Code
      print(nsc_rec_half)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Operations 
      * Distance to shrunken centroids with: all_numeric_predictors()

---

    Code
      print(nsc_rec_half_prep)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Training information 
      Training data contained 300 data points and no incomplete rows.
      
      -- Operations 
      * Distance to shrunken centroids with: x, y | Trained

---

    Code
      print(tidy_spec)
    Output
      # A tibble: 1 x 6
        terms                    value class type  threshold id                      
        <chr>                    <dbl> <chr> <chr>     <dbl> <chr>                   
      1 all_numeric_predictors()    NA <NA>  <NA>         NA classdist_shrunken_512IF

---

    Code
      print(tidy_prep)
    Output
      # A tibble: 18 x 6
         terms class type       value threshold id                      
         <chr> <chr> <chr>      <dbl>     <dbl> <chr>                   
       1 x     a     global    2.70         0.5 classdist_shrunken_512IF
       2 x     a     by_class  8.11         0.5 classdist_shrunken_512IF
       3 x     a     shrunken  1.37         0.5 classdist_shrunken_512IF
       4 x     b     global    2.70         0.5 classdist_shrunken_512IF
       5 x     b     by_class -0.0378       0.5 classdist_shrunken_512IF
       6 x     b     shrunken -0.0171       0.5 classdist_shrunken_512IF
       7 x     c     global    2.70         0.5 classdist_shrunken_512IF
       8 x     c     by_class  0.0297       0.5 classdist_shrunken_512IF
       9 x     c     shrunken  0            0.5 classdist_shrunken_512IF
      10 y     a     global   -2.68         0.5 classdist_shrunken_512IF
      11 y     a     by_class  0.0516       0.5 classdist_shrunken_512IF
      12 y     a     shrunken  0            0.5 classdist_shrunken_512IF
      13 y     b     global   -2.68         0.5 classdist_shrunken_512IF
      14 y     b     by_class -8.04         0.5 classdist_shrunken_512IF
      15 y     b     shrunken -1.24         0.5 classdist_shrunken_512IF
      16 y     c     global   -2.68         0.5 classdist_shrunken_512IF
      17 y     c     by_class -0.0445       0.5 classdist_shrunken_512IF
      18 y     c     shrunken  0            0.5 classdist_shrunken_512IF

---

    Code
      recipe(class ~ x + y, data = nsc_test) %>% step_classdist_shrunken(
        all_numeric_predictors(), class = "class", threshold = -1) %>% prep()
    Condition
      Error in `step_classdist_shrunken()`:
      Caused by error in `prep.step_classdist_shrunken()`:
      ! all(threshold >= 0) & all(threshold <= 1) & length(threshold) ==  .... is not TRUE

---

    Code
      recipe(class ~ x + y, data = nsc_test) %>% step_classdist_shrunken(
        all_numeric_predictors(), class = "class", sd_offset = -1) %>% prep()
    Condition
      Error in `step_classdist_shrunken()`:
      Caused by error in `prep.step_classdist_shrunken()`:
      ! all(sd_offset >= 0) & all(sd_offset <= 1) & length(sd_offset) ==  .... is not TRUE

