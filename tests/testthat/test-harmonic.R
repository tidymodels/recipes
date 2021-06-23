library(testthat)
library(recipes)

context("harmonic parameters")
x_year           <- seq(0, 86400*365*4, by = 31556926)
x_month_sidereal <- seq(0, 86400*7, by = 2360592)
x_month_synodic  <- seq(0, 86400*7, by = 2551443)
x_month_average  <- seq(0, 86400*365, by = 2629744)
x_week           <- seq(0, 86400*365, by = 86400*7)
x_day            <- seq(0, 86400*365, by = 86400)
x_hour           <- seq(0, 86400*7, by = 3600)
x_minute         <- seq(0, 86400, by = 60)
x_second         <- 0:86400



test_that("harmonic error", {
  harmonic_dat = tibble(osc = sin(2*pi*x_second/(3600*6)),
                        time_var = x_second)

  # non-numeric input
  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               period = "a",
                               cycle_unit = "second"))
  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               frequency = "a",
                               cycle_unit = "second"))


  # wrong units
  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               frequency = 1,
                               cycle_unit = "yeer"))

  # negative and zero values
  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               frequency = -1,
                               cycle_unit = "second"))
  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               frequency = 0,
                               cycle_unit = "second"))
  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               period = -1,
                               cycle_unit = "second"))
  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               period = 0,
                               cycle_unit = "second"))

  # infinite input
  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               period = Inf,
                               cycle_unit = "second"))
  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               frequency = Inf,
                               cycle_unit = "second"))

  # no period or frequency
  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               cycle_unit = "year"))

  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               frequency = 1,
                               cycle_unit = c("second", "minute")))


})

test_that("harmonic cycle units", {

  harmonic_dat = tibble(osc = sin(2*pi*x_second/(3600*6)),
                        time_var = x_second)

  rec_year <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 365.2422,
                  cycle_unit = "year") %>%
    prep() %>%
    bake(new_data = NULL)
  rec_month_synodic <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 29.53059028,
                  cycle_unit = "month_synodic") %>%
    prep() %>%
    bake(new_data = NULL)
  rec_month_sidereal <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 27.321666667,
                  cycle_unit = "month_sidereal") %>%
    prep() %>%
    bake(new_data = NULL)
  rec_month_average <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 30.43685185,
                  cycle_unit = "month_average") %>%
    prep() %>%
    bake(new_data = NULL)
  rec_week <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 7,
                  cycle_unit = "week") %>%
    prep() %>%
    bake(new_data = NULL)
  rec_day <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 1,
                  cycle_unit = "day") %>%
    prep() %>%
    bake(new_data = NULL)
  rec_hour <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 1/24,
                  cycle_unit = "hour") %>%
    prep() %>%
    bake(new_data = NULL)
  rec_minute <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 1/1440,
                  cycle_unit = "minute") %>%
    prep() %>%
    bake(new_data = NULL)
  rec_second <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 1/86400,
                  cycle_unit = "second") %>%
    prep() %>%
    bake(new_data = NULL)
  rec_sample <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 1/86400,
                  cycle_unit = "sample") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(rec_year[[3]],
               rec_month_synodic[[3]])
  expect_equal(rec_year[[3]],
               rec_month_sidereal[[3]])
  expect_equal(rec_year[[3]],
               rec_month_average[[3]])
  expect_equal(rec_year[[3]],
               rec_week[[3]])
  expect_equal(rec_year[[3]],
               rec_day[[3]])
  expect_equal(rec_year[[3]],
               rec_hour[[3]])
  expect_equal(rec_year[[3]],
               rec_minute[[3]])
  expect_equal(rec_year[[3]],
               rec_second[[3]])
  expect_equal(rec_year[[3]],
               rec_sample[[3]])

})

test_that("harmonic multiple variables", {
  harmonic_dat_mult = tibble(osc = sin(2*pi*x_second/(3600*6)),
                             time_var_1 = x_second,
                             time_var_2 = x_second*2)

  rec <- recipe(osc ~ time_var_1 + time_var_2, data = harmonic_dat_mult) %>%
    step_harmonic(time_var_1, time_var_2,
                  frequency = c(5, 10),
                  cycle_unit = "week") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(rec$time_var_1_sin_f_10, rec$time_var_2_sin_f_5)

})


test_that("harmonic frequencies", {
  harmonic_dat = tibble(osc = sin(2*pi*x_second/(3600*6)),
                        time_var = x_second)

  rec <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = c(1, 1.93, 2),
                  cycle_unit = "day") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(ncol(rec), 2+6)

})

test_that("harmonic periods", {
  harmonic_dat = tibble(osc = sin(2*pi*x_second/(3600*6)),
                        time_var = x_second)

  rec <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  period = c(1, 1.93, 2),
                  cycle_unit = "day") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(ncol(rec), 2+6)
})


test_that("harmonic frequencies and periods", {
  harmonic_dat = tibble(osc = sin(2*pi*x_second/(3600*6)),
                        time_var = x_second)
  expect_warning(rec <- recipe(osc ~ time_var, data = harmonic_dat) %>%
                   step_harmonic(time_var,
                                 frequency = 2,
                                 period = 0.5,
                                 cycle_unit = "week") %>%
                   prep() %>%
                   bake(new_data = NULL))
  expect_equal(ncol(rec), 2 * 2 + 2)
  expect_equal(rec$time_var_cos_f_2, rec$time_var_cos_p_0.5)

})


test_that("harmonic phase", {

  harmonic_dat_1 = tibble(osc = sin(2*pi*x_second / (3600*6)),
                          time_var = x_second)

  rec_1 <- recipe(osc ~ time_var, data = harmonic_dat_1) %>%
    step_harmonic(time_var,
                  frequency = 4,
                  cycle_unit = "day") %>%
    prep() %>%
    bake(new_data = NULL)

  # different starting point
  harmonic_dat_2 = tibble(osc = sin(2*pi*x_second[-(1:8000)] / (3600*6)),
                          time_var = x_second[-(1:8000)])
  rec_2 <- recipe(osc ~ time_var, data = harmonic_dat_2) %>%
    step_harmonic(time_var,
                  frequency = 4,
                  cycle_unit = "day") %>%
    prep() %>%
    bake(new_data = NULL)
  fit_1 <- lm(osc ~ time_var_sin_f_4 + time_var_cos_f_4 - 1, rec_1)
  fit_2 <- lm(osc ~ time_var_sin_f_4 + time_var_cos_f_4 - 1, rec_2)
  co_1 <- coefficients(fit_1)
  co_2 <- coefficients(fit_2)
  expect_equal(atan2(co_1[1], co_1[2]), atan2(co_2[1], co_2[2]))


  # capture half period shift
  harmonic_dat_3 = tibble(osc = sin(2*pi*(3600*3 + x_second) / (3600*6)),
                          time_var = x_second)
  rec_3 <- recipe(osc ~ time_var, data = harmonic_dat_3) %>%
    step_harmonic(time_var,
                  frequency = 4,
                  cycle_unit = "day") %>%
    prep() %>%
    bake(new_data = NULL)
  fit_3 <- lm(osc ~ time_var_sin_f_4 + time_var_cos_f_4 - 1, rec_3)
  co_3 <- coefficients(fit_3)
  expect_equal(atan2(co_1[1], co_1[2]), -atan2(co_3[1], co_3[2]))


  # set reference (starting_val) at half period
  rec_4 <- recipe(osc ~ time_var, data = harmonic_dat_1) %>%
    step_harmonic(time_var,
                  frequency = 4,
                  starting_val = 3600*3,
                  cycle_unit = "day") %>%
    prep() %>%
    bake(new_data = NULL)
  fit_4 <- lm(osc ~ time_var_sin_f_4 + time_var_cos_f_4 - 1, rec_4)
  co_4 <- coefficients(fit_4)
  expect_equal(atan2(co_1[1], co_1[2]), -atan2(co_4[1], co_4[2]))


})

test_that("harmonic model recovers amplitude", {

  set.seed(123)
  amplitude <- abs(rnorm(1))
  harmonic_dat = tibble(osc = amplitude * sin(2*pi*x_second/(3600*6)),
                        time_var = x_second)
  rec <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 4,
                  cycle_unit = "day") %>%
    prep() %>%
    bake(new_data = NULL)
  fit <- lm(osc ~ time_var_sin_f_4 + time_var_cos_f_4 - 1, rec)
  expect_equal(sqrt(sum(coefficients(fit)^2)), amplitude)

})



test_that("harmonic datetime, numeric and date columns", {
  x_datetime <- as.POSIXct(x_second,
                           origin = '1970-01-01',
                           tz = 'UTC')

  harmonic_dat = tibble(osc = sin(2 * pi * as.numeric(x_second) / (3600 * 6)),
                        time_var_posixt = x_datetime,
                        time_var_int = x_second)

  rec_datetime <- recipe(osc ~ time_var_posixt, data = harmonic_dat) %>%
    step_harmonic(time_var_posixt,
                  frequency = 4,
                  cycle_unit = "day") %>%
    prep() %>%
    bake(new_data = NULL)

  rec_numeric <- recipe(osc ~ time_var_int, data = harmonic_dat) %>%
    step_harmonic(time_var_int,
                  frequency = 4,
                  cycle_unit = "day") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(rec_datetime[[3]],
               rec_numeric[[3]],
               ignore_attr = TRUE)


  x_date <- as.Date(x_second[1:366], origin = '1970-01-01') # one year
  harmonic_dat = tibble(osc = sin(2 * pi * as.numeric(x_date)),
                        time_var_date = x_date)

  rec_date <- recipe(osc ~ time_var_date, data = harmonic_dat) %>%
    step_harmonic(time_var_date,
                  frequency = 12,
                  cycle_unit = "year") %>%
    prep() %>%
    bake(new_data = NULL)

  x_date_sec <- seq(0, 365, 1) * 86400 # one year
  harmonic_dat = tibble(osc = sin(2 * pi * x_date_sec),
                        time_var_date_s = x_date_sec)

  rec_date_s <- recipe(osc ~ time_var_date_s, data = harmonic_dat) %>%
    step_harmonic(time_var_date_s,
                  frequency = 12,
                  cycle_unit = "year") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(rec_date[[3]],
               rec_date_s[[3]],
               ignore_attr = TRUE)


})


test_that("harmonic NA in term", {

  harmonic_dat = tibble(osc = sin(2 * pi * as.numeric(x_second) / (3600 * 6)),
                        time_var = x_second)
  harmonic_dat[20, 'time_var'] <- NA
  harmonic_dat[3, 'time_var'] <- NA
  rec_na <- recipe(osc ~ time_var, data = harmonic_dat) %>%
    step_harmonic(time_var,
                  frequency = 12,
                  cycle_unit = "year") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(sum(is.na(rec_na)), 2*3)

  harmonic_dat = tibble(osc = sin(2 * pi * as.numeric(x_second) / (3600 * 6)),
                        time_var = NA_real_)

  expect_error(recipe(osc ~ time_var, data = harmonic_dat) %>%
                 step_harmonic(time_var,
                               frequency = 12,
                               cycle_unit = "year") %>%
                 prep() %>%
                 bake(new_data = NULL))


})



