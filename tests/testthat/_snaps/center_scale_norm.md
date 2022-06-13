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
      Centering and scaling for nitrogen, carbon

---

    Code
      prep(standardized)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 536 data points and no missing data.
      
      Operations:
      
      Centering for carbon [trained]
      Scaling for hydrogen [trained]
      Centering and scaling for nitrogen, carbon [trained]

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
      Column(s) have zero variance so scaling cannot be used: `zero_variance`. Consider using `step_zv()` to remove those columns before normalizing
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

# normalize - warns on zv

    Code
      prep(rec1)
    Condition
      Warning:
      Column(s) have zero variance so scaling cannot be used: `zero_variance`. Consider using `step_zv()` to remove those columns before normalizing
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          6
      
      Training data contained 536 data points and no missing data.
      
      Operations:
      
      Centering and scaling for carbon, hydrogen, oxygen, nitrogen, sulfur, zer... [trained]

# centering with case weights

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor          9
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Centering for disp, hp, drat, wt, qsec, vs, am, gear, carb [weighted, trained]

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor          9
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Centering for cyl, disp, hp, drat, qsec, vs, am, gear, carb [ignored weights, trained]

# scaling with case weights

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor          9
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Scaling for disp, hp, drat, wt, qsec, vs, am, gear, carb [weighted, trained]

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor          9
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Scaling for cyl, disp, hp, drat, qsec, vs, am, gear, carb [ignored weights, trained]

# normalizing with case weights

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor          9
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Centering and scaling for disp, hp, drat, wt, qsec, vs, am, gear, carb [weighted, trained]

---

    Code
      rec
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor          9
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Centering and scaling for cyl, disp, hp, drat, qsec, vs, am, gear, carb [ignored weights, trained]

