# bad args

    Code
      recipe(~., data = iris) %>% step_novel(all_predictors()) %>% prep(iris)
    Condition
      Error in `prep()`:
      ! Columns must be character or factor: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width

---

    Code
      recipe(~., data = tr_bad) %>% step_novel(all_predictors()) %>% prep(tr_bad)
    Condition
      Error in `prep()`:
      ! Columns already contain the new level: x

# printing

    Code
      print(ex_3)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Operations:
      
      Novel factor level assignment for all_predictors()

---

    Code
      print(prep(ex_3))
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 200 data points and no missing data.
      
      Operations:
      
      Novel factor level assignment for v, w, x, y, z [trained]

# empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Novel factor level assignment for <none>

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Novel factor level assignment for <none> [trained]

