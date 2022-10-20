# bad selector(s)

    Code
      rec %>% step_regex(description, rows, pattern = "(rock|stony)")
    Condition
      Error in `step_regex()`:
      ! For this step, at most a single selector can be used.

---

    Code
      prep(rec4, training = covers)
    Condition
      Error in `check_type()`:
      ! All columns selected for the step should be factor or character

# printing

    Code
      print(rec1)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Operations:
      
      Regular expression dummy variable using "(rock|stony)"

---

    Code
      prep(rec1)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Training data contained 40 data points and no missing data.
      
      Operations:
      
      Regular expression dummy variable using "(rock|stony)" [trained]

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
      
      Regular expression dummy variable using "."

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
      
      Regular expression dummy variable using "." [trained]

