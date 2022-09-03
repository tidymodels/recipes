# printing

    Code
      print(filtering)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          7
      
      Operations:
      
      Missing value column filter on all_predictors()

---

    Code
      prep(filtering)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          7
      
      Training data contained 100 data points and 100 incomplete rows. 
      
      Operations:
      
      Missing value column filter removed dbl4, dbl5, chr2 [trained]

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
      
      Missing value column filter on <none>

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
      
      Missing value column filter removed <none> [trained]

# case weights

    Code
      filtering_trained
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          7
      
      Training data contained 100 data points and 100 incomplete rows. 
      
      Operations:
      
      Missing value column filter removed dbl2, dbl3, dbl4, dbl5, chr1, chr2 [weighted, trained]

---

    Code
      filtering_trained
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          7
      
      Training data contained 100 data points and 100 incomplete rows. 
      
      Operations:
      
      Missing value column filter removed dbl2, dbl3, dbl4, dbl5, chr2 [ignored weights, trained]

