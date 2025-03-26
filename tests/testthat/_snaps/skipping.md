# simple skip

    Code
      prepped_2 <- prep(rec_1, training = iris, retain = FALSE)
    Condition
      Warning:
      Since some operations have `skip = TRUE`, using `retain = TRUE` will allow those step's results to be accessible.

---

    Code
      prep(rec_1, training = iris, retain = FALSE)
    Condition
      Warning:
      Since some operations have `skip = TRUE`, using `retain = TRUE` will allow those step's results to be accessible.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 4
      
      -- Training information 
      Training data contained 150 data points and no incomplete rows.
      
      -- Operations 
      * Log transformation on: Sepal.Length | Trained
      * Dummy variables from: Species | Trained
      * Centering for: Sepal.Width, Petal.Length, Petal.Width, ... | Trained

