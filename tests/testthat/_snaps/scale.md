# scale by factor of 1 or 2

    Code
      not_recommended_standardized_input <- rec %>% step_scale(carbon, id = "scale",
        factor = 3) %>% prep(training = biomass)
    Condition
      Warning:
      Scaling `factor` should take either a value of 1 or 2, not 3.

# na_rm argument works for step_scale

    Code
      rec_no_na_rm <- recipe(~., data = mtcars_na) %>% step_scale(all_predictors(),
      na_rm = FALSE) %>% prep()
    Condition
      Warning:
      Columns `mpg`, `cyl`, `disp`, and `hp` returned NaN, because variance cannot be calculated and scaling cannot be used. Consider avoiding `Inf` or `-Inf` values and/or setting `na_rm = TRUE` before normalizing.

# warns on zv

    Code
      prep(rec1)
    Condition
      Warning:
      !  The following column has zero variance so scaling cannot be used: zero_variance.
      i Consider using ?step_zv (`?recipes::step_zv()`) to remove those columns before normalizing.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 6
      
      -- Training information 
      Training data contained 536 data points and no incomplete rows.
      
      -- Operations 
      * Scaling for: carbon, hydrogen, oxygen, nitrogen, sulfur, ... | Trained

# warns when NaN is returned

    Code
      prep(rec1)
    Condition
      Warning:
      Column `sulfur` returned NaN, because variance cannot be calculated and scaling cannot be used. Consider avoiding `Inf` or `-Inf` values and/or setting `na_rm = TRUE` before normalizing.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 536 data points and no incomplete rows.
      
      -- Operations 
      * Log transformation on: sulfur | Trained
      * Scaling for: sulfur | Trained

# scaling with case weights

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:      1
      predictor:    9
      case_weights: 1
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Scaling for: disp, hp, drat, wt, qsec, vs, am, ... | Trained, weighted

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:      1
      predictor:    9
      case_weights: 1
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Scaling for: cyl, disp, hp, drat, qsec, vs, ... | Trained, ignored weights

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
      * Scaling for: <none>

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
      * Scaling for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Scaling for: disp and wt

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Scaling for: disp and wt | Trained

