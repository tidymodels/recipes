# outside argument

    Code
      recipe(~a, data = train_df) %>% step_percentile(a, outside = "left") %>% prep() %>%
        bake(new_data = new_df)
    Condition
      Error in `step_percentile()`:
      ! `outside` must be one of "none", "both", "upper", or "lower", not "left".

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
      prep(rec)
    Output
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

# case weights

    Code
      rec_trained
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          8
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      Percentile transformation on carbon, sulfur [weighted, trained]

---

    Code
      rec_trained
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
          predictor          8
      
      Training data contained 456 data points and no missing data.
      
      Operations:
      
      Percentile transformation on carbon, sulfur [ignored weights, trained]

