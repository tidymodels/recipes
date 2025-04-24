# lat lon

    Code
      near_station <- prep(step_geodist(recipe(~., data = postal), lat = latitude,
      lon = longitude, log = FALSE, ref_lat = 100, ref_lon = 100, is_lat_lon = TRUE))
    Condition
      Error in `step_geodist()`:
      ! `ref_lat` must be a number between -90 and 90, not the number 100.

---

    Code
      near_station <- prep(step_geodist(recipe(~., data = postal), lat = latitude,
      lon = longitude, log = FALSE, ref_lat = 0, ref_lon = 190, is_lat_lon = TRUE))
    Condition
      Error in `step_geodist()`:
      ! `ref_lon` must be a number between -180 and 180, not the number 190.

---

    Code
      near_station <- prep(step_geodist(recipe(~., data = postal), lat = latitude,
      lon = longitude, log = FALSE, ref_lat = -100, ref_lon = 0, is_lat_lon = TRUE))
    Condition
      Error in `step_geodist()`:
      ! `ref_lat` must be a number between -90 and 90, not the number -100.

---

    Code
      near_station <- prep(step_geodist(recipe(~., data = postal), lat = latitude,
      lon = longitude, log = FALSE, ref_lat = 0, ref_lon = -190, is_lat_lon = TRUE))
    Condition
      Error in `step_geodist()`:
      ! `ref_lon` must be a number between -180 and 180, not the number -190.

---

    Code
      near_station <- prep(step_geodist(recipe(~., data = postal), lat = latitude,
      lon = longitude, log = FALSE, ref_lat = 38.8986312, ref_lon = -77.0062457,
      is_lat_lon = TRUE))
    Condition
      Error in `step_geodist()`:
      Caused by error in `bake()`:
      ! All `lat` values should be between -90 and 90.

---

    Code
      near_station <- prep(step_geodist(recipe(~., data = postal), lat = latitude,
      lon = longitude, log = FALSE, ref_lat = 38.8986312, ref_lon = -77.0062457,
      is_lat_lon = TRUE))
    Condition
      Error in `step_geodist()`:
      Caused by error in `bake()`:
      ! All `lon` values should be between -180 and 180.

---

    Code
      near_station <- prep(step_geodist(recipe(~., data = postal), lat = latitude,
      lon = longitude, log = FALSE, ref_lat = 38.8986312, ref_lon = -77.0062457,
      is_lat_lon = TRUE))
    Condition
      Error in `step_geodist()`:
      Caused by error in `bake()`:
      ! All `lat` values should be between -90 and 90.

---

    Code
      near_station <- prep(step_geodist(recipe(~., data = postal), lat = latitude,
      lon = longitude, log = FALSE, ref_lat = 38.8986312, ref_lon = -77.0062457,
      is_lat_lon = TRUE))
    Condition
      Error in `step_geodist()`:
      Caused by error in `bake()`:
      ! All `lon` values should be between -180 and 180.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_geodist()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `geo_dist`

# bad args

    Code
      prep(step_geodist(rec, starts_with("x"), y, ref_lat = 0.5, ref_lon = 0.25),
      training = rand_data_2)
    Condition
      Error in `step_geodist()`:
      Caused by error in `prep()`:
      x The `lat` selector should select at most a single variable.
      i The following 2 were selected: `x` and `x1`.

---

    Code
      prep(step_geodist(rec, x, starts_with("y"), ref_lat = 0.5, ref_lon = 0.25),
      training = rand_data_2)
    Condition
      Error in `step_geodist()`:
      Caused by error in `prep()`:
      x The `lon` selector should select at most a single variable.
      i The following 2 were selected: `y` and `y1`.

---

    Code
      prep(step_geodist(rec, x, y, ref_lat = letters[1:2], ref_lon = 0.25), training = rand_data_2)
    Condition
      Error in `step_geodist()`:
      ! `ref_lat` must be a number, not a character vector.

---

    Code
      prep(step_geodist(rec, x, y, ref_lon = letters[1:2], ref_lat = 0.25), training = rand_data_2)
    Condition
      Error in `step_geodist()`:
      ! `ref_lon` must be a number, not a character vector.

---

    Code
      prep(step_geodist(rec, x, y, ref_lon = 0.5, ref_lat = 0.25, name = 1),
      training = rand_data_2)
    Condition
      Error in `step_geodist()`:
      ! `name` must be a single string, not the number 1.

---

    Code
      prep(step_geodist(rec, x, y, ref_lon = 0.5, ref_lat = 0.25, log = exp(1)),
      training = rand_data_2)
    Condition
      Error in `step_geodist()`:
      ! `log` must be `TRUE` or `FALSE`, not the number 2.72.

---

    Code
      prep(step_geodist(recipe(~ x + y, data = rand_data), x, y, ref_lat = 0.5,
      ref_lon = 0.25, is_lat_lon = "no", log = FALSE), training = rand_data)
    Condition
      Error in `step_geodist()`:
      ! `is_lat_lon` must be `TRUE` or `FALSE`, not the string "no".

---

    Code
      prep(step_geodist(recipe(~ x + y, data = rand_data), x, y, ref_lat = 0.5,
      ref_lon = 0.25, is_lat_lon = c(TRUE, TRUE), log = FALSE), training = rand_data)
    Condition
      Error in `step_geodist()`:
      ! `is_lat_lon` must be `TRUE` or `FALSE`, not a logical vector.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = rand_data[, 2, drop = FALSE])
    Condition
      Error in `step_geodist()`:
      ! The following required column is missing from `new_data`: x.

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
      * Geographical distances from 0.5 x 0.25 using: <none>

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
      * Geographical distances from 0.5 x 0.25 using: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_geodist()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * Geographical distances from 0.5 x 0.25 using: x y

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 10 data points and 1 incomplete row.
      
      -- Operations 
      * Geographical distances from 0.5 x 0.25 using: x y | Trained

