# scale by factor of 1 or 2

    Code
      not_recommended_standardized_input <- rec %>% step_scale(carbon, id = "scale",
        factor = 3) %>% prep(training = biomass)
    Condition
      Warning:
      Scaling `factor` should take either a value of 1 or 2

# printing

    Code
      print(standardized)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Operations:
      
      Centering for carbon
      Scaling for hydrogen

---

    Code
      prep(standardized, training = biomass, verbose = TRUE)
    Output
      oper 1 step center [training] 
      oper 2 step scale [training] 
      The retained training set is ~ 0.03 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 536 data points and no missing data.
      
      Operations:
      
      Centering for carbon [trained]
      Scaling for hydrogen [trained]

# center - empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Centering for <none>

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
      
      Centering for <none> [trained]

# scale - empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Scaling for <none>

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
      
      Scaling for <none> [trained]

# scale - warns on zv

    Code
      prep(rec1)
    Condition
      Warning:
      Column(s) have zero variance so scaling is not defined: `zero_variance`
      i Consider `step_zv()` to remove those columns before scaling
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          6
      
      Training data contained 536 data points and no missing data.
      
      Operations:
      
      Scaling for carbon, hydrogen, oxygen, nitrogen, sulfur, zer... [trained]

# normalize - empty printing

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Centering and scaling for <none>

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
      
      Centering and scaling for <none> [trained]

