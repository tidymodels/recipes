# error when neither sep or pattern is specified

    Code
      recipe(~medium, data = tate_text) %>% step_dummy_extract(medium) %>% prep()
    Error <rlang_error>
      `sep` or `pattern` must be specified.

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
      
      Extract patterns from <none>

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
      
      Extract patterns from <none> [trained]

