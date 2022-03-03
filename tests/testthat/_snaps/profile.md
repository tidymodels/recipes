# bad values

    Code
      okc_rec %>% step_profile(everything(), profile = vars(age)) %>% prep(data = okc)
    Condition
      Error in `prep()`:
      ! The profiled variable cannot be in the list of variables to be fixed.

---

    Code
      okc_rec %>% step_profile(everything(), profile = age) %>% prep(data = okc)
    Condition
      Error in `structure()`:
      ! object 'age' not found

---

    Code
      okc_rec %>% step_profile(age, date, height, profile = vars(location, date)) %>%
        prep(data = okc)
    Condition
      Error in `prep()`:
      ! Only one variable should be profiled

---

    Code
      okc_rec %>% step_profile(diet, profile = vars(age), pct = -1) %>% prep(data = okc)
    Condition
      Error in `step_profile()`:
      ! `pct should be on [0, 1]`

---

    Code
      okc_rec %>% step_profile(diet, profile = vars(age), grid = 1:3) %>% prep(data = okc)
    Condition
      Error in `step_profile()`:
      ! `grid` should have two named elements. See ?step_profile

---

    Code
      okc_rec %>% step_profile(diet, profile = vars(age), grid = list(pctl = 1, len = 2)) %>%
        prep(data = okc)
    Condition
      Error in `step_profile()`:
      ! `grid$pctl should be logical.`

---

    Code
      fixed(rep(c(TRUE, FALSE), each = 5))
    Condition
      Error in `error_cnd()`:
      ! Conditions must have named data fields

# printing

    Code
      print(num_rec_1)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          7
      
      Operations:
      
      Profiling data set for age

---

    Code
      print(num_rec_2)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          7
      
      Training data contained 20 data points and 4 incomplete rows. 
      
      Operations:
      
      Profiling data set for age [trained]

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
      
      Profiling data set for mpg

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
      
      Profiling data set for mpg [trained]

