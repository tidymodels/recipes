# check_col works in the bake stage

    Code
      rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars[-1])
    Condition
      Error:
      ! The following cols are missing from `new_data`: mpg.

---

    Code
      rp2 %>% check_cols(cyl, mpg, drat) %>% prep() %>% bake(mtcars[, c(2, 5)])
    Condition
      Error:
      ! The following cols are missing from `new_data`: mpg.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Check if the following columns are present: everything()

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Check if the following columns are present: mpg, disp, hp, drat, wt, qsec, vs, am, gear, ca... [trained]

# empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Check if the following columns are present: <none>

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Check if the following columns are present: <none> [trained]

# non-standard roles during bake/predict

    The following cols are missing from `new_data`: date.
    i There are also non-standard recipe roles for the column(s).
    i See `?update_role` for more information on how use non-standard recipe roles during prediction.

---

    The following required columns are missing: 'date'.

---

    The following required columns are missing: 'date'.

---

    The following required columns are missing: 'date'.

