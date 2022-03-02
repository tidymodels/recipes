# basic functionality

    Code
      te_1 <- bake(rec_1, okc_te)
    Condition
      Warning:
      There are new levels in a factor: port costa, nicasio, livingston, granite bay, isla vista, hilarita, campbell, santa ana, santa rosa, north hollywood, nevada city, stockton, marin city, waterford, muir beach, pacheco, irvine, canyon, oceanview, napa, san luis obispo, modesto, costa mesa, oakley, chico, south lake tahoe, vacaville, long beach
      New levels will be coerced to `NA` by `step_unknown()`.
      Consider using `step_novel()` before `step_unknown()`.

# bad args

    Code
      recipe(~., data = okc_tr) %>% step_unknown(age) %>% prep()
    Condition
      Error in `prep()`:
      ! Columns must be character or factor: age

---

    Code
      recipe(~., data = okc_tr) %>% step_unknown(diet, new_level = "anything") %>%
        prep()
    Condition
      Error in `prep()`:
      ! Columns already contain a level 'anything': diet

# printing

    Code
      print(rec %>% step_unknown(diet, location))
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          6
      
      Operations:
      
      Unknown factor level assignment for diet, location

---

    Code
      print(rec %>% step_unknown(diet, location) %>% prep())
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          6
      
      Training data contained 30000 data points and 12077 incomplete rows. 
      
      Operations:
      
      Unknown factor level assignment for diet, location [trained]

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
      
      Unknown factor level assignment for <none>

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
      
      Unknown factor level assignment for <none> [trained]

