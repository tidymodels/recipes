# format_selectors handles a long expression (#1083)

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Variable mutation for: function_call(.x = vs, very_very_very_long_text =
        "long1", also_very_very_long_text = "long2")

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Variable mutation for: function_call(.x = vs, very_very_very_long_text =
        "long1", also_very_very_long_text = "long2")

