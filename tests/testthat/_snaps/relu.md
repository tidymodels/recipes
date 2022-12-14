# input checking

    Code
      recipe(~., data = df) %>% step_relu(val1, shift = TRUE) %>% prep(df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      ! Shift argument must be a numeric value.

---

    Code
      recipe(~., data = df) %>% step_relu(val1, reverse = 3) %>% prep(df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      ! Reverse argument must be a logical value.

---

    Code
      recipe(~., data = df) %>% step_relu(val1, smooth = "cat") %>% prep(df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      ! Smooth argument must be logical value.

---

    Code
      recipe(~., data = df) %>% step_relu(val2) %>% prep(df, verbose = FALSE)
    Condition
      Error in `step_relu()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be double, or integer.

# prints something

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Adding relu transform for val1

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 21 data points and no missing data.
      
      Operations:
      
      Adding relu transform for val1 [trained]

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
      
      Adding relu transform for <none>

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
      
      Adding relu transform for <none> [trained]

