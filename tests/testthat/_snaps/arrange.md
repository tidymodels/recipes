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
      
      Row arrangement using <none>

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
      
      Row arrangement using <none> [trained]

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Operations:
      
      Row arrangement using Sepal.Length

---

    Code
      prep(rec, training = iris, verbose = TRUE)
    Output
      oper 1 step arrange [training] 
      The retained training set is ~ 0.01 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
       predictor          5
      
      Training data contained 150 data points and no missing data.
      
      Operations:
      
      Row arrangement using ~Sepal.Length [trained]

