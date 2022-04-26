# printing

    Code
      print(sp_sign)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Operations:
      
      Centering for carbon, hydrogen
      Scaling for carbon, hydrogen
      Spatial sign on  carbon, hydrogen

---

    Code
      prep(sp_sign, training = biomass, verbose = TRUE)
    Output
      oper 1 step center [training] 
      oper 2 step scale [training] 
      oper 3 step spatialsign [training] 
      The retained training set is ~ 0.03 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          5
      
      Training data contained 536 data points and no missing data.
      
      Operations:
      
      Centering for carbon, hydrogen [trained]
      Scaling for carbon, hydrogen [trained]
      Spatial sign on  carbon, hydrogen [trained]

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
      
      Spatial sign on  <none>

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
      
      Spatial sign on  <none> [trained]

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
      
      Spatial sign on  cyl, disp, hp, drat, qsec, vs, am, gear, carb [trained]

