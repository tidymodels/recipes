# outside argument

    Code
      recipe(~a, data = train_df) %>% step_percentile(a, outside = "left") %>% prep() %>%
        bake(new_data = new_df)
    Condition
      Error in `step_percentile()`:
      ! `outside` must be one of "none", "both", "upper", or "lower", not "left".

# case weights

    Code
      rec_trained
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    8
      case_weights: 1
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * Percentile transformation on: carbon and sulfur | Trained, weighted

---

    Code
      rec_trained
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    8
      case_weights: 1
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * Percentile transformation on: carbon and sulfur | Trained, ignored weights

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = biomass_tr[, c(-3, -7)])
    Condition
      Error in `step_percentile()`:
      ! The following required columns are missing from `new_data`: carbon and sulfur.

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
      * Percentile transformation on: <none>

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
      * Percentile transformation on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 8
      
      -- Operations 
      * Percentile transformation on: carbon and sulfur

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 8
      
      -- Training information 
      Training data contained 456 data points and no incomplete rows.
      
      -- Operations 
      * Percentile transformation on: carbon and sulfur | Trained

