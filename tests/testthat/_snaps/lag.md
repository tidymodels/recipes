# default lag works on a single feature

    Code
      prepped_rec <- recipe(~., data = df) %>% step_lag(x, lag = 0.5) %>% prep(df)
    Condition
      Error in `bake()`:
      ! step_lag requires 'lag' argument to be integer valued.

# something prints

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Lagging t

---

    Code
      prep(rec, training = df, verbose = TRUE)
    Output
      oper 1 step lag [training] 
      The retained training set is ~ 0 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 10 data points and no missing data.
      
      Operations:
      
      Lagging t [trained]

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
      
      Lagging <none>

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
      
      Lagging <none> [trained]

