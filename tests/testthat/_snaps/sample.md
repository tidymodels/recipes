# bad input

    Code
      iris_rec %>% step_sample(size = -1)
    Condition
      Error in `step_sample()`:
      ! `size` should be a positive number or NULL.

---

    Code
      iris_rec %>% step_sample(size = "a")
    Condition
      Error in `step_sample()`:
      ! `size` should be a positive number or NULL.

---

    Code
      iris_rec %>% step_sample(replace = "a")
    Condition
      Error in `step_sample()`:
      ! `replace` should be a single logical.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          6
      
      Operations:
      
      Row sampling <none>

---

    Code
      prep(rec, training = iris2, verbose = TRUE)
    Output
      oper 1 step sample [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          6
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Row sampling <none> [trained]

# sample with case weights

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Row sampling <none> [weighted, trained]

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Row sampling <none> [weighted, trained]

