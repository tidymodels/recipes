# printing

    Code
      print(ica_extract)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Operations:
      
      Centering and scaling for all_predictors()
      ICA extraction with carbon, hydrogen

---

    Code
      prep(ica_extract)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      Centering and scaling for carbon, hydrogen, oxygen, nitrogen, sulfur [trained]
      ICA extraction with carbon, hydrogen [trained]

# No ICA comps

    Code
      print(ica_extract_trained)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      Centering and scaling for carbon, hydrogen, oxygen, nitrogen, sulfur [trained]
      No ICA components were extracted from carbon, hydrogen, oxygen, nitrogen, sulfur [trained]

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
      
      ICA extraction with <none>

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
      
      ICA extraction with <none> [trained]

