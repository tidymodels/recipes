# check_col works in the bake stage

    Code
      rp1 %>% check_cols(everything()) %>% prep() %>% bake(mtcars[-1])
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "mpg".
      i These columns have one of the following roles, which are required at `bake()` time: "predictor".

---

    Code
      rp2 %>% check_cols(cyl, mpg, drat) %>% prep() %>% bake(mtcars[, c(2, 5)])
    Condition
      Error in `bake()`:
      ! The following required columns are missing from `new_data`: "mpg".
      i These columns have one of the following roles, which are required at `bake()` time: "predictor".

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

    The following required columns are missing from `new_data`: "date".
    i These columns have one of the following roles, which are required at `bake()` time: "date".
    i If these roles are not required at `bake()` time, use `update_role_requirements(role = "your_role", bake = FALSE)`.

---

    The following required columns are missing: 'date'.

---

    The following required columns are missing: 'date'.

---

    The following required columns are missing: 'date'.

