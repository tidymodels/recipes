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
      * Distance to shrunken centroids with: x and y | Trained

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
         terms   value class type     threshold id                      
         <chr>   <dbl> <chr> <chr>        <dbl> <chr>                   
       1 x      2.70   a     global         0.5 classdist_shrunken_512IF
       2 x      8.11   a     by_class       0.5 classdist_shrunken_512IF
       3 x      1.37   a     shrunken       0.5 classdist_shrunken_512IF
       4 x      2.70   b     global         0.5 classdist_shrunken_512IF
       5 x     -0.0378 b     by_class       0.5 classdist_shrunken_512IF
       6 x     -0.0171 b     shrunken       0.5 classdist_shrunken_512IF
       7 x      2.70   c     global         0.5 classdist_shrunken_512IF
       8 x      0.0297 c     by_class       0.5 classdist_shrunken_512IF
       9 x      0      c     shrunken       0.5 classdist_shrunken_512IF
      10 y     -2.68   a     global         0.5 classdist_shrunken_512IF
      11 y      0.0516 a     by_class       0.5 classdist_shrunken_512IF
      12 y      0      a     shrunken       0.5 classdist_shrunken_512IF
      13 y     -2.68   b     global         0.5 classdist_shrunken_512IF
      14 y     -8.04   b     by_class       0.5 classdist_shrunken_512IF
      15 y     -1.24   b     shrunken       0.5 classdist_shrunken_512IF
      16 y     -2.68   c     global         0.5 classdist_shrunken_512IF
      17 y     -0.0445 c     by_class       0.5 classdist_shrunken_512IF
      18 y      0      c     shrunken       0.5 classdist_shrunken_512IF

---

    Code
      recipe(class ~ x + y, data = nsc_test) %>% step_classdist_shrunken(
        all_numeric_predictors(), class = "class", threshold = -1) %>% prep()
    Condition
      Error in `step_classdist_shrunken()`:
      Caused by error in `prep()`:
      ! `threshold` must be a number between 0 and 1, not the number -1.

---

    Code
      recipe(class ~ x + y, data = nsc_test) %>% step_classdist_shrunken(
        all_numeric_predictors(), class = "class", sd_offset = -1) %>% prep()
    Condition
      Error in `step_classdist_shrunken()`:
      Caused by error in `prep()`:
      ! `sd_offset` must be a number between 0 and 1, not the number -1.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Operations 
      * Distance to shrunken centroids with: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Distance to shrunken centroids with: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Operations 
      * Distance to shrunken centroids with: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Distance to shrunken centroids with: Sepal.Length, ... | Trained

