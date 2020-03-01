context("Check new values")

x    <- rep(letters[1:3], 2)
x_na <- c(rep(letters[1:3], 2), NA)
allowed_values <- letters[1:3]

test_that("new_values_func passes when no new values", {
  expect_error(new_values_func(x, allowed_values), NA)
})

test_that("new_values_func breaks when x contains new values", {
  expect_error(new_values_func(x, allowed_values[-3], colname = "MacGyver"),
               "MacGyver contains the new value(s): c", fixed = TRUE)
})

test_that("new_values_func correctly prints multiple new values", {
  expect_error(new_values_func(x, allowed_values[-c(2:3)], colname = "MacGyver"),
               "MacGyver contains the new value(s): b,c", fixed = TRUE)
})

test_that("new_values_func by default ignores NA", {
  expect_error(new_values_func(x_na, allowed_values), NA)
})

test_that("new_values_func breaks when NA is new value and ignore_NA is FALSE", {
  expect_error(new_values_func(x_na, allowed_values,
                               ignore_NA = FALSE, colname = "MacGyver"),
               "MacGyver contains the new value(s): NA", fixed = TRUE)
})

test_that("new_values_func correctly prints multiple new values with NA", {
  expect_error(new_values_func(x_na, allowed_values[-3],
                               ignore_NA = FALSE, colname = "MacGyver"),
               "MacGyver contains the new value(s): c,NA", fixed = TRUE)
})

test_that("new_values_func correctly prints only non na-values when also NA as new value and ignore_NA is TRUE", {
  expect_error(new_values_func(x_na, allowed_values[-3],
                               ignore_NA = TRUE, colname = "MacGyver"),
               "MacGyver contains the new value(s): c", fixed = TRUE)
})

test_that("check_new_values does nothing when no new values", {
  expect_error(
    x <- recipe(credit_data) %>% check_new_values(Home) %>%
      prep() %>% bake(credit_data),
    NA
  )
  expect_equal(x, credit_data)
})

test_that("check_new_values breaks with new values", {
  x1 <- data.frame(a = letters[1:3])
  x2 <- data.frame(a = letters[1:5])

  expect_error(
    recipe(x1) %>% check_new_values(a) %>%
      prep() %>% bake(x2[1:4, , drop = FALSE]),
    "a contains the new value(s): d", fixed = TRUE
  )

  expect_error(
    recipe(x1) %>% check_new_values(a) %>%
      prep() %>% bake(x2),
    "a contains the new value(s): d,e", fixed = TRUE
  )
})

test_that("check_new_values ignores NA by default", {
  x1 <- data.frame(a = letters[1:3])
  x2 <- data.frame(a = letters[1:4] %>% c(NA))
  expect_error(
    recipe(x1) %>% check_new_values(a) %>%
      prep() %>% bake(x2[-4, , drop = FALSE]),
    NA
  )

  expect_error(
    recipe(x1) %>% check_new_values(a) %>%
      prep() %>% bake(x2),
    "a contains the new value(s): d", fixed = TRUE
  )
})

test_that("check_new_values not ignoring NA argument", {
  x1 <- data.frame(a = letters[1:3])
  x2 <- data.frame(a = letters[1:4] %>% c(NA))

  expect_error(
    recipe(x1) %>% check_new_values(a, ignore_NA = FALSE) %>%
      prep() %>% bake(x2[-4, , drop = FALSE]),
    "a contains the new value(s): NA", fixed = TRUE
  )

  expect_error(
    recipe(x1) %>% check_new_values(a, ignore_NA = FALSE) %>%
      prep() %>% bake(x2),
    "a contains the new value(s): d,NA", fixed = TRUE
  )
})

check_new_values_data_type_unit_tests <- function(x1, x2, saf = TRUE) {

  expect_error(
    res <- recipe(x1) %>% check_new_values(a) %>%
      prep(strings_as_factors = saf) %>% bake(x1),
    NA
  )

  expect_equal(res, x1)

  error_msg <- paste( "a contains the new value(s):", x2[3,])
  expect_error(
    recipe(x1) %>% check_new_values(a) %>%
      prep() %>% bake(x2),
    error_msg, fixed = TRUE
  )
}

test_that("check_new_values works on doubles", {
  x1 <- data.frame(a = c(1.1, 1.2))
  x2 <- data.frame(a = c(1.1, 1.2, 1.3))
  check_new_values_data_type_unit_tests(x1, x2)
})

test_that("check_new_values works on integers", {
  x1 <- data.frame(a = c(1L, 2L))
  x2 <- data.frame(a = c(1L, 2L, 3L))
  check_new_values_data_type_unit_tests(x1, x2)
})

test_that("check_new_values works on factors", {
  x1 <- data.frame(a = factor(letters[1:2]))
  x2 <- data.frame(a = factor(letters[1:3]))
  check_new_values_data_type_unit_tests(x1, x2)
})

test_that("check_new_values works on characters", {
  hell_no <- FALSE
  x1 <- data.frame(a = letters[1:2], stringsAsFactors = hell_no)
  x2 <- data.frame(a = letters[1:3], stringsAsFactors = hell_no)
  check_new_values_data_type_unit_tests(x1, x2, saf = FALSE)
})

test_that("check_new_values works on logicals", {
  x1 <- data.frame(a = c(TRUE, TRUE))
  x2 <- data.frame(a = c(TRUE, TRUE, FALSE))
  check_new_values_data_type_unit_tests(x1, x2)
})
