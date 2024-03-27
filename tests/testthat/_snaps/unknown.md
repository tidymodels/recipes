# basic functionality

    Code
      te_1 <- bake(rec_1, sacr_te)
    Condition
      Warning:
      ! There are new levels in a factor: `WEST_SACRAMENTO`.
      * New levels will be coerced to `NA` by `step_unknown()`.
      i Consider using ?step_novel (`?recipes::step_novel()`) before `step_unknown()`.
      Warning:
      ! There are new levels in a factor: `z95691`.
      * New levels will be coerced to `NA` by `step_unknown()`.
      i Consider using ?step_novel (`?recipes::step_novel()`) before `step_unknown()`.

# bad args

    Code
      recipe(~., data = sacr_tr) %>% step_unknown(sqft) %>% prep()
    Condition
      Error in `step_unknown()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be string, factor, or ordered.
      * 1 integer variable found: `sqft`

---

    Code
      recipe(~., data = sacr_tr) %>% step_unknown(city, new_level = "FAIR_OAKS") %>%
        prep()
    Condition
      Error in `step_unknown()`:
      Caused by error in `prep()`:
      ! Columns already contain the level "FAIR_OAKS": city.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Unknown factor level assignment for: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Unknown factor level assignment for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 9
      
      -- Operations 
      * Unknown factor level assignment for: city and zip

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 9
      
      -- Training information 
      Training data contained 800 data points and no incomplete rows.
      
      -- Operations 
      * Unknown factor level assignment for: city and zip | Trained

