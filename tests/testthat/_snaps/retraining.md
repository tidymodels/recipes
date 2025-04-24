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
      prep(step_rm(prep(step_center(rec, carbon, hydrogen, oxygen, nitrogen, sulfur),
      training = biomass), sulfur), training = biomass)
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

