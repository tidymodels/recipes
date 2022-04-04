# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Operations:
      
      Distances to Species for all_predictors()

---

    Code
      prep(rec, training = iris, verbose = TRUE)
    Output
      oper 1 step classdist [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Distances to Species for Sepal.Length, Sepal.Width, Petal.Length, Petal.... [trained]

# empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Operations:
      
      Distances to Species for <none>

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          4
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Distances to Species for <none> [trained]

# case weights

    Code
      rec_prep
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor          4
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Distances to Species for Sepal.Length, Sepal.Width, Petal.Length, Petal.... [weighted, trained]

