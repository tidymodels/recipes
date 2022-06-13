# bad selector(s)

    Code
      rec %>% step_count(description, rows, pattern = "(rock|stony)")
    Condition
      Error in `step_count()`:
      ! For this step, only a single selector can be used.

---

    Code
      prep(rec2, training = covers)
    Condition
      Error in `prep()`:
      ! The regular expression input should be character or factor

# printing

    Code
      print(rec5)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Operations:
      
      Regular expression counts using description

---

    Code
      prep(rec5)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Training data contained 40 data points and no missing data.
      
      Operations:
      
      Regular expression counts using description [trained]

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
      
      Regular expression counts using <none>

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
      
      Regular expression counts using <none> [trained]

