# errors if degree > deg_free (#1170)

    Code
      recipe(~., data = mtcars) %>% step_spline_convex(mpg, degree = 3, deg_free = 3,
        complete_set = TRUE) %>% prep()
    Condition
      Error in `step_spline_convex()`:
      Caused by error in `prep()`:
      ! `degree` (3) must be less than to `deg_free` (3) when `complete_set = FALSE`.

---

    Code
      recipe(~., data = mtcars) %>% step_spline_convex(mpg, degree = 4, deg_free = 3,
        complete_set = FALSE) %>% prep()
    Condition
      Error in `step_spline_convex()`:
      Caused by error in `prep()`:
      ! `degree` (4) must be less than or equal to `deg_free` (3) when `complete_set = TRUE`.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_spline_convex()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `mpg_01`

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
      * Convex spline expansion: <none>

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
      * Convex spline expansion: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Operations 
      * Convex spline expansion: carbon and hydrogen

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 5
      
      -- Training information 
      Training data contained 536 data points and no incomplete rows.
      
      -- Operations 
      * Convex spline expansion: carbon and hydrogen | Trained

