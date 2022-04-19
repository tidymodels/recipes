# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          8
      
      Operations:
      
      Percentile transformation on carbon, sulfur

---

    Code
      prep(rec, training = biomass_tr, verbose = TRUE)
    Output
      oper 1 step percentile [training] 
      The retained training set is ~ 0.06 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          8
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      Percentile transformation on carbon, sulfur [trained]

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
      
      Percentile transformation on <none>

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
      
      Percentile transformation on <none> [trained]

