# bad args

    Code
      rec %>% step_string2factor(w, n) %>% prep(ex_dat)
    Condition
      Error in `step_string2factor()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be string, factor, or ordered.

---

    Code
      rec %>% step_string2factor(n, ordered = "yes") %>% prep(ex_dat)
    Condition
      Error in `step_string2factor()`:
      ! `ordered` should be a single logical variable

# printing

    Code
      print(ex_3)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 200 data points and no missing data.
      
      Operations:
      
      Factor variables from w, x [trained]

---

    Code
      prep(ex_3)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 200 data points and no missing data.
      
      Operations:
      
      Factor variables from w, x [trained]

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
      
      Factor variables from <none>

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
      
      Factor variables from <none> [trained]

