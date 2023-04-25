# many missing values

    Code
      filtering_trained <- prep(filtering, training = dat2, verbose = FALSE)
    Condition
      Warning:
      The correlation matrix has missing values. 1 columns were excluded from the filter.

# occasional missing values

    Code
      filtering_trained <- prep(filtering, training = dat3, verbose = FALSE)
    Condition
      Warning:
      The correlation matrix has sporadic missing values. Some columns were excluded from the filter.

# case weights

    Code
      filtering_trained
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    8
      case_weights: 1
      
      -- Training information 
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Correlation filter on: V3_dup, V1, V2 | Trained, weighted

---

    Code
      filtering_trained
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor:    8
      case_weights: 1
      
      -- Training information 
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Correlation filter on: V6, V1, V3 | Trained, ignored weights

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
      * Correlation filter on: <none>

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
      * Correlation filter on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 7
      
      -- Operations 
      * Correlation filter on: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 7
      
      -- Training information 
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Correlation filter on: V6, V1 | Trained

