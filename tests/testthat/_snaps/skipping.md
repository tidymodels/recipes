# simple skip

    Code
      prepped_2 <- prep(rec_1, training = iris, retain = FALSE)
    Condition
      Warning:
      Since some operations have `skip = TRUE`, using `retain = TRUE` will allow those steps results to be accessible.

---

    Code
      prep(rec_1, training = iris, retain = FALSE)
    Condition
      Warning:
      Since some operations have `skip = TRUE`, using `retain = TRUE` will allow those steps results to be accessible.
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Log transformation on Sepal.Length [trained]
      Dummy variables from Species [trained]
      Centering for Sepal.Width, Petal.Length, Petal.Width, Species... [trained]

