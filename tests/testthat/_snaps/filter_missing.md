# case weights

    Code
      filtering_trained
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    7
      case_weights: 1
      
      -- Training information 
      Training data contained 100 data points and 100 incomplete rows.
      
      -- Operations 
      * Missing value column filter removed: dbl2, dbl3, dbl4, ... | Trained,
        weighted

---

    Code
      filtering_trained
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    7
      case_weights: 1
      
      -- Training information 
      Training data contained 100 data points and 100 incomplete rows.
      
      -- Operations 
      * Missing value column filter removed: dbl2, ... | Trained, ignored weights

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
      * Missing value column filter on: <none>

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
      * Missing value column filter removed: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 7
      
      -- Operations 
      * Missing value column filter on: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 7
      
      -- Training information 
      Training data contained 100 data points and 100 incomplete rows.
      
      -- Operations 
      * Missing value column filter removed: dbl2, dbl3, dbl4, dbl5, chr2 | Trained

