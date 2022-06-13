# basic functionality

    Code
      te_1 <- bake(rec_1, sacr_te)
    Condition
      Warning:
      There are new levels in a factor: WEST_SACRAMENTO
      New levels will be coerced to `NA` by `step_unknown()`.
      Consider using `step_novel()` before `step_unknown()`.
      Warning:
      There are new levels in a factor: z95691
      New levels will be coerced to `NA` by `step_unknown()`.
      Consider using `step_novel()` before `step_unknown()`.

# bad args

    Code
      recipe(~., data = sacr_tr) %>% step_unknown(sqft) %>% prep()
    Condition
      Error in `prep()`:
      ! Columns must be character or factor: sqft

---

    Code
      recipe(~., data = sacr_tr) %>% step_unknown(city, new_level = "FAIR_OAKS") %>%
        prep()
    Condition
      Error in `prep()`:
      ! Columns already contain a level 'FAIR_OAKS': city

# printing

    Code
      print(rec %>% step_unknown(city, zip))
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          9
      
      Operations:
      
      Unknown factor level assignment for city, zip

---

    Code
      print(rec %>% step_unknown(city, zip) %>% prep())
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          9
      
      Training data contained 800 data points and no missing data.
      
      Operations:
      
      Unknown factor level assignment for city, zip [trained]

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

