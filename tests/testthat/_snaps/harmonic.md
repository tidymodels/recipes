# harmonic error

    Code
      recipe(osc ~ time_var, data = harmonic_dat) %>% step_harmonic(time_var,
        frequency = 1, cycle_size = NA)
    Condition
      Error in `step_harmonic()`:
      ! cycle_size must have at least one non-NA numeric value.

---

    Code
      recipe(osc ~ time_var, data = harmonic_dat) %>% step_harmonic(time_var,
        frequency = 1, starting_val = 0, cycle_size = NA)
    Condition
      Error in `step_harmonic()`:
      ! cycle_size must have at least one non-NA numeric value.

---

    Code
      recipe(osc ~ time_var, data = harmonic_dat) %>% step_harmonic(time_var,
        frequency = 1, starting_val = 0, cycle_size = "a")
    Condition
      Error in `step_harmonic()`:
      ! cycle_size must have at least one non-NA numeric value.

---

    Code
      recipe(osc ~ time_var, data = harmonic_dat) %>% step_harmonic(time_var,
        frequency = 1, starting_val = "a", cycle_size = 86400)
    Condition
      Error in `step_harmonic()`:
      ! starting_val must be NA, numeric, Date or POSIXt

---

    Code
      recipe(osc ~ time_var, data = harmonic_dat) %>% step_harmonic(time_var,
        frequency = 1, starting_val = factor("a"), cycle_size = 86400)
    Condition
      Error in `step_harmonic()`:
      ! starting_val must be NA, numeric, Date or POSIXt

# harmonic NA in term

    Code
      recipe(osc ~ time_var, data = harmonic_dat) %>% step_harmonic(time_var,
        frequency = 4, cycle_size = 86400) %>% prep() %>% bake(new_data = NULL)
    Condition
      Error in `step_harmonic()`:
      Caused by error in `bake()`:
      ! variable must have at least one non-NA value

# harmonic character in term

    Code
      recipe(osc ~ time_var, data = harmonic_dat) %>% step_harmonic(time_var,
        frequency = 4, cycle_size = 86400) %>% prep() %>% bake(new_data = NULL)
    Condition
      Error in `step_harmonic()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be date, datetime, or numeric.

# harmonic cycle_size length

    Code
      recipe(osc ~ time_var_1 + time_var_2 + time_var_3, data = harmonic_dat) %>%
        step_harmonic(time_var_1, time_var_2, time_var_3, frequency = 4, cycle_size = c(
          86400, 86400)) %>% prep()
    Condition
      Error in `step_harmonic()`:
      Caused by error in `prep()`:
      ! `cycle_size` must be length 1 or the same  length as the input columns

# harmonic starting_val length

    Code
      recipe(osc ~ time_var_1 + time_var_2 + time_var_3, data = harmonic_dat) %>%
        step_harmonic(time_var_1, time_var_2, time_var_3, frequency = 4,
          starting_val = c(86400, 86400), cycle_size = 86400) %>% prep()
    Condition
      Error in `step_harmonic()`:
      Caused by error in `prep()`:
      ! `starting_val` must be length 1 or the same  length as the input columns

# printing

    Code
      print(with_harmonic)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Operations:
      
      Harmonic numeric variables for hp

---

    Code
      prep(with_harmonic)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         10
      
      Training data contained 32 data points and no missing data.
      
      Operations:
      
      Harmonic numeric variables for hp [trained]

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
      
      Harmonic numeric variables for <none>

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
      
      Harmonic numeric variables for <none> [trained]

