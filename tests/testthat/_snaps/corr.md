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

# printing

    Code
      print(filtering)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          7
      
      Operations:
      
      Correlation filter on all_predictors()

---

    Code
      prep(filtering)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          7
      
      Training data contained 100 data points and no missing data.
      
      Operations:
      
      Correlation filter on V6, V1 [trained]

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
      
      Correlation filter on <none>

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
      
      Correlation filter on <none> [trained]

# case weights

    Code
      filtering_trained
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          8
      
      Training data contained 100 data points and no missing data.
      
      Operations:
      
      Correlation filter on V3_dup, V1, V2 [weighted, trained]

---

    Code
      filtering_trained
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          8
      
      Training data contained 100 data points and no missing data.
      
      Operations:
      
      Correlation filter on V6, V1, V3 [ignored weights, trained]

