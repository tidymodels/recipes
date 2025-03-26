# training in stages

    Code
      no_sulfur_trained <- prep(no_sulfur)

---

    Code
      sequentially <- prep(scale_last)

---

    Code
      in_stages_trained <- prep(in_stages)

---

    Code
      rec %>% step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) %>% prep(
        training = biomass) %>% step_rm(sulfur) %>% prep(training = biomass)
    Condition
      Warning in `prep()`:
      ! The previous data will be used by `prep()`.
      i The data passed using `training` will be ignored.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 536 data points and no incomplete rows.
      
      -- Operations 
      * Centering for: carbon, hydrogen, oxygen, nitrogen, sulfur | Trained
      * Variables removed: sulfur | Trained

