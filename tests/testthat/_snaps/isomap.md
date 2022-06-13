# printing

    Code
      print(im_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Operations:
      
      Isomap approximation with x1, x2, x3

---

    Code
      prep(im_rec)
    Message
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Training data contained 5 data points and no missing data.
      
      Operations:
      
      Isomap approximation with x1, x2, x3 [trained]

# No ISOmap

    Code
      print(im_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          3
      
      Training data contained 5 data points and no missing data.
      
      Operations:
      
      Isomap was not conducted for x1, x2, x3 [trained]

# ISOmap fails gracefully

    Code
      recipe(Sepal.Length ~ ., data = iris) %>% step_bs(Sepal.Width, deg_free = 1,
        degree = 1) %>% step_bs(Sepal.Length, deg_free = 1, degree = 1) %>%
        step_other(Species, threshold = 1e-09) %>% step_isomap(all_numeric_predictors(),
      num_terms = 1, neighbors = 1) %>% prep()
    Message
    Condition
      Error in `prep()`:
      ! `step_isomap` failed with error:
      Error in do.call(.Call, args = dot_call_args) : 
        TridiagEigen: eigen decomposition failed

# can prep recipes with no keep_original_cols

    Code
      im_trained <- prep(im_rec, training = dat1, verbose = FALSE)
    Message
    Condition
      Warning:
      'keep_original_cols' was added to `step_isomap()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

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
      
      Isomap approximation with <none>

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
      
      Isomap approximation with <none> [trained]

