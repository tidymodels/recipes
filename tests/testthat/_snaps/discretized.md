# bad values

    Code
      discretize(letters)
    Condition
      Error in `discretize()`:
      ! Only numeric `x` is accepted

# printing of discretize()

    Code
      discretize(1:100)
    Output
      Bins: 5 (includes missing category)
      Breaks: -Inf, 25.75, 50.5, 75.25, Inf

---

    Code
      discretize(1:100, cuts = 6)
    Output
      Bins: 7 (includes missing category)

---

    Code
      discretize(1:100, keep_na = FALSE)
    Output
      Bins: 4
      Breaks: -Inf, 25.75, 50.5, 75.25, Inf

---

    Code
      res <- discretize(1:2)
    Condition
      Warning:
      Data not binned; too few unique values per bin. Adjust 'min_unique' as needed

---

    Code
      res
    Output
      Too few unique data points. No binning.

# bad args

    Code
      recipe(~., data = ex_tr) %>% step_discretize(x1, num_breaks = 1) %>% prep()
    Condition
      Error:
      ! There should be at least 2 cuts

---

    Code
      recipe(~., data = ex_tr) %>% step_discretize(x1, num_breaks = 100) %>% prep()
    Condition
      Warning:
      Data not binned; too few unique values per bin. Adjust 'min_unique' as needed
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Training data contained 100 data points and no missing data.
      
      Operations:
      
      Discretize numeric variables from x1 [trained]

---

    Code
      recipe(~., data = ex_tr) %>% step_discretize(x1, options = list(prefix = "@$")) %>%
        prep()
    Condition
      Warning:
      The prefix '@$' is not a valid R name. It has been changed to 'X..'.
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Training data contained 100 data points and no missing data.
      
      Operations:
      
      Discretize numeric variables from x1 [trained]

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Operations:
      
      Discretize numeric variables from x1

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Training data contained 100 data points and no missing data.
      
      Operations:
      
      Discretize numeric variables from x1 [trained]

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
      
      Discretize numeric variables from <none>

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
      
      Discretize numeric variables from <none> [trained]

