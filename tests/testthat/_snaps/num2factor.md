# bad args

    Code
      rec %>% step_num2factor(w, x, levels = c("one", "two")) %>% prep(ex_dat)
    Condition
      Error in `step_num2factor()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be double, or integer.

---

    Code
      rec %>% step_num2factor(w, x) %>% prep(ex_dat)
    Condition
      Error in `step_num2factor()`:
      ! Please provide a character vector of appropriate length for `levels`.

# printing

    Code
      print(ex_3)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Training data contained 200 data points and no missing data.
      
      Operations:
      
      Factor variables from z [trained]

---

    Code
      prep(ex_3)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Training data contained 200 data points and no missing data.
      
      Operations:
      
      Factor variables from z [trained]

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

