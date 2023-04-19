# scale by factor of 1 or 2

    Code
      not_recommended_standardized_input <- rec %>% step_scale(carbon, id = "scale",
        factor = 3) %>% prep(training = biomass)
    Condition
      Warning:
      Scaling `factor` should take either a value of 1 or 2

# printing

    Code
      print(standardized)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * Scaling for: hydrogen

---

    Code
      prep(standardized)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 536 data points and no incomplete rows.
      
      -- Operations 
      * Scaling for: hydrogen | Trained

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

# warns on zv

    Code
      prep(rec1)
    Condition
      Warning:
      Column(s) have zero variance so scaling cannot be used: `zero_variance`. Consider using `step_zv()` to remove those columns before normalizing
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
      * Scaling for: disp, hp, drat, wt, qsec, vs, am, gear, carb | Trained, weighted

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
      * Scaling for: cyl, disp, hp, drat, qsec, vs, am, ... | Trained, ignored
        weights

