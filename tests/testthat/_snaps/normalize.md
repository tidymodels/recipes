# na_rm argument works for step_normalize

    Code
      rec_no_na_rm <- prep(step_normalize(recipe(~., data = mtcars_na),
      all_predictors(), na_rm = FALSE))
    Condition
      Warning:
      Columns `mpg`, `cyl`, `disp`, and `hp` returned NaN, because variance cannot be calculated and scaling cannot be used. Consider avoiding `Inf` or `-Inf` values and/or setting `na_rm = TRUE` before normalizing.

---

    Code
      prep(step_normalize(recipe(~., data = mtcars_na), all_predictors(), na_rm = 2))
    Condition
      Error in `step_normalize()`:
      Caused by error in `prep()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the number 2.

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
      * Centering and scaling for: carbon, hydrogen, oxygen, ... | Trained

# normalizing with case weights

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
      * Centering and scaling for: disp, hp, drat, wt, ... | Trained, weighted

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
      * Centering and scaling for: cyl, disp, hp, ... | Trained, ignored weights

# warns when NaN is returned due to Inf or -Inf

    Code
      prep(rec)
    Condition
      Warning:
      Column `x` returned NaN, because variance cannot be calculated and scaling cannot be used. Consider avoiding `Inf` or `-Inf` values and/or setting `na_rm = TRUE` before normalizing.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Centering and scaling for: x | Trained

---

    Code
      prep(rec)
    Condition
      Warning:
      Column `x` returned NaN, because variance cannot be calculated and scaling cannot be used. Consider avoiding `Inf` or `-Inf` values and/or setting `na_rm = TRUE` before normalizing.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 4 data points and no incomplete rows.
      
      -- Operations 
      * Centering and scaling for: x | Trained

# bake method errors when needed non-standard role columns are missing

    Code
      bake(std_trained, new_data = biomass[, 1:2])
    Condition
      Error in `step_normalize()`:
      ! The following required columns are missing from `new_data`: carbon, hydrogen, oxygen, nitrogen, and sulfur.

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
      * Centering and scaling for: <none>

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
      * Centering and scaling for: <none> | Trained

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
      * Centering and scaling for: disp wt

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
      * Centering and scaling for: disp wt | Trained

