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
      Error in `prep()`:
      ! The regular expression input should be character or factor

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
      prep(rec1, training = covers, verbose = TRUE)
    Output
      oper 1 step regex [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
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

