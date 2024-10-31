# bad values

    Code
      discretize(letters)
    Condition
      Error in `discretize()`:
      x Only numeric `x` is accepted.
      i The `x` was passed a character vector.

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
      Data not binned; too few unique values per bin. Adjust `min_unique` as needed.

---

    Code
      res
    Output
      Too few unique data points. No binning was used.

# multiple column prefix

    Code
      recipe(~., data = example_data) %>% step_discretize(x1, x2, options = list(
        prefix = "hello")) %>% prep()
    Condition
      Warning:
      Note that the options `prefix` and `labels` will be applied to all variables.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 1000 data points and no incomplete rows.
      
      -- Operations 
      * Discretize numeric variables from: x1 and x2 | Trained

# bad args

    Code
      recipe(~., data = ex_tr) %>% step_discretize(x1, num_breaks = 1) %>% prep()
    Condition
      Error in `step_discretize()`:
      Caused by error in `recipes::discretize()`:
      ! There should be at least 2 `cuts` but 1 was supplied.

---

    Code
      recipe(~., data = ex_tr) %>% step_discretize(x1, num_breaks = 100) %>% prep()
    Condition
      Warning:
      Data not binned; too few unique values per bin. Adjust `min_unique` as needed.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Discretize numeric variables from: x1 | Trained

---

    Code
      recipe(~., data = ex_tr) %>% step_discretize(x1, options = list(prefix = "@$")) %>%
        prep()
    Condition
      Warning:
      The prefix "@$" is not a valid R name. It has been changed to "X..".
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Discretize numeric variables from: x1 | Trained

# war when less breaks are generated

    Code
      tmp <- discretize(c(rep(1, 50), 1:50), cuts = 5, min_unique = 1)
    Condition
      Warning:
      Not enough data for 5 breaks. Only 4 breaks were used.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec, new_data = mtcars[, 2:ncol(mtcars)])
    Condition
      Error in `step_discretize()`:
      ! The following required column is missing from `new_data`: mpg.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Discretize numeric variables from: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Discretize numeric variables from: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Operations 
      * Discretize numeric variables from: x1

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 3
      
      -- Training information 
      Training data contained 100 data points and no incomplete rows.
      
      -- Operations 
      * Discretize numeric variables from: x1 | Trained

