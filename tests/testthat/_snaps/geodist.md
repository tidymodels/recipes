# lat lon

    Code
      near_station <- recipe(~., data = postal) %>% step_geodist(lat = latitude, lon = longitude,
        log = FALSE, ref_lat = 100, ref_lon = 100, is_lat_lon = TRUE) %>% prep()
    Condition
      Error in `step_geodist()`:
      ! `ref_lat` should be between -90 and 90

---

    Code
      near_station <- recipe(~., data = postal) %>% step_geodist(lat = latitude, lon = longitude,
        log = FALSE, ref_lat = 0, ref_lon = 190, is_lat_lon = TRUE) %>% prep()
    Condition
      Error in `step_geodist()`:
      ! `ref_lon` should be between -180 and 180

---

    Code
      near_station <- recipe(~., data = postal) %>% step_geodist(lat = latitude, lon = longitude,
        log = FALSE, ref_lat = -100, ref_lon = 0, is_lat_lon = TRUE) %>% prep()
    Condition
      Error in `step_geodist()`:
      ! `ref_lat` should be between -90 and 90

---

    Code
      near_station <- recipe(~., data = postal) %>% step_geodist(lat = latitude, lon = longitude,
        log = FALSE, ref_lat = 0, ref_lon = -190, is_lat_lon = TRUE) %>% prep()
    Condition
      Error in `step_geodist()`:
      ! `ref_lon` should be between -180 and 180

---

    Code
      near_station <- recipe(~., data = postal) %>% step_geodist(lat = latitude, lon = longitude,
        log = FALSE, ref_lat = 38.8986312, ref_lon = -77.0062457, is_lat_lon = TRUE) %>%
        prep()
    Condition
      Error in `geo_dist_calc_lat_lon()`:
      ! All `lat` values should be between -90 and 90

---

    Code
      near_station <- recipe(~., data = postal) %>% step_geodist(lat = latitude, lon = longitude,
        log = FALSE, ref_lat = 38.8986312, ref_lon = -77.0062457, is_lat_lon = TRUE) %>%
        prep()
    Condition
      Error in `geo_dist_calc_lat_lon()`:
      ! All `lon` values should be between -180 and 180

---

    Code
      near_station <- recipe(~., data = postal) %>% step_geodist(lat = latitude, lon = longitude,
        log = FALSE, ref_lat = 38.8986312, ref_lon = -77.0062457, is_lat_lon = TRUE) %>%
        prep()
    Condition
      Error in `geo_dist_calc_lat_lon()`:
      ! All `lat` values should be between -90 and 90

---

    Code
      near_station <- recipe(~., data = postal) %>% step_geodist(lat = latitude, lon = longitude,
        log = FALSE, ref_lat = 38.8986312, ref_lon = -77.0062457, is_lat_lon = TRUE) %>%
        prep()
    Condition
      Error in `geo_dist_calc_lat_lon()`:
      ! All `lon` values should be between -180 and 180

# bad args

    Code
      rec %>% step_geodist(starts_with("x"), y, ref_lat = 0.5, ref_lon = 0.25) %>%
        prep(training = rand_data_2)
    Condition
      Error in `prep()`:
      ! `lat` should resolve to a single column name.

---

    Code
      rec %>% step_geodist(x, starts_with("y"), ref_lat = 0.5, ref_lon = 0.25) %>%
        prep(training = rand_data_2)
    Condition
      Error in `prep()`:
      ! `lon` should resolve to a single column name.

---

    Code
      rec %>% step_geodist(x, y, ref_lat = letters[1:2], ref_lon = 0.25) %>% prep(
        training = rand_data_2)
    Condition
      Error in `step_geodist()`:
      ! `ref_lat` should be a single numeric value.

---

    Code
      rec %>% step_geodist(x, y, ref_lon = letters[1:2], ref_lat = 0.25) %>% prep(
        training = rand_data_2)
    Condition
      Error in `step_geodist()`:
      ! `ref_lon` should be a single numeric value.

---

    Code
      rec %>% step_geodist(x, y, ref_lon = 0.5, ref_lat = 0.25, name = 1) %>% prep(
        training = rand_data_2)
    Condition
      Error in `step_geodist()`:
      ! `name` should be a single character value.

---

    Code
      rec %>% step_geodist(x, y, ref_lon = 0.5, ref_lat = 0.25, log = exp(1)) %>%
        prep(training = rand_data_2)
    Condition
      Error in `step_geodist()`:
      ! `log` should be a single logical value.

# printing

    Code
      print(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Operations:
      
      Geographical distances from 0.5 x 0.25 using x, y

---

    Code
      prep(rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
       predictor          2
      
      Training data contained 10 data points and 1 incomplete row. 
      
      Operations:
      
      Geographical distances from 0.5 x 0.25 using x, y [trained]

