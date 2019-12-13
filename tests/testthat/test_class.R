library(testthat)
library(recipes)
library(dplyr)

library(modeldata)
data(okc)

x1 <- rnorm(3)
x2 <- as.POSIXct(1:3, origin = "1970-01-01", tz = "CET")
x <- data_frame(x1, x2)
x_newdata <- tibble(x1 = as.character(x1), x2)

x3 <- x2
class(x3) <- c(class(x3), "Julian")
x_newdata_2 <- data_frame(x1 = x1, x2 = x3)

err1 <- "x1 should have the class(es) character but has the class(es) numeric." #nolint
err2 <- "x2 should have the class(es) POSIXct, Julian but has the class(es) POSIXct, POSIXt." #nolint
err3 <- "x2 has the class(es) POSIXct, POSIXt, but only the following is/are asked POSIXct, allow_additional is FALSE." #nolint
err4 <- "x1 should have the class(es) numeric but has the class(es) character." #nolint
err5 <- "x2 has the class(es) POSIXct, POSIXt, Julian, but only the following is/are asked POSIXct, POSIXt, allow_additional is FALSE." #nolint
err6 <- "diet should have the class(es) factor but has the class(es) character."

test_that("bake_check_class helper function gives expected output", {

  expect_error(bake_check_class_core(x1, "numeric", "x1"), NA)
  expect_error(bake_check_class_core(x2, c("POSIXct", "POSIXt"), "x1"), NA)
  expect_error(bake_check_class_core(x1, "character", "x1"),
               err1, fixed = TRUE)
  expect_error(bake_check_class_core(x2, c("POSIXct", "Julian"), "x2"),
               err2, fixed = TRUE)
  expect_error(bake_check_class_core(x2, "POSIXct", "x2", TRUE), NA)
  expect_error(bake_check_class_core(x2, "POSIXct", "x2"),
               err3, fixed = TRUE)
})

test_that("check_class works when class is learned", {
  rec1 <- recipe(x) %>% check_class(everything()) %>% prep()

  expect_error(bake(rec1, x), NA)
  expect_equal(bake(rec1, x), x)
  expect_error(bake(rec1, x_newdata),
               err4, fixed = TRUE)
  expect_error(bake(rec1, x_newdata_2),
               err5, fixed = TRUE)
})

test_that("check_class works when class is provided", {
  rec2 <- recipe(x) %>% check_class(x1, class_nm = "numeric") %>% prep()

  expect_error(bake(rec2, x), NA)
  expect_equal(bake(rec2, x), x)
  expect_error(bake(rec2, x_newdata),
               err4, fixed = TRUE)

  rec3 <- recipe(x) %>%
    check_class(x2, class_nm = c("POSIXct", "POSIXt")) %>%
    prep()

  expect_error(bake(rec3, x), NA)
  expect_equal(bake(rec3, x), x)
  expect_error(bake(rec3, x_newdata_2),
               err5, fixed = TRUE)

  rec4 <- recipe(x) %>%
    check_class(x2,
                class_nm = c("POSIXct", "POSIXt"),
                allow_additional = TRUE) %>% prep()
  expect_error(bake(rec4, x_newdata_2), NA)
})

# recipes has internal coercion to character >> factor
test_that('characters are handled correctly' ,{
  rec5_NULL <- recipe(okc[1:10,], age ~ .) %>%
    check_class(everything()) %>%
    prep(okc[1:10, ], strings_as_factors = FALSE)

  expect_error(bake(rec5_NULL, okc[11:20, ]), NA)

  rec5_man <- recipe(okc[1:10,], age ~ .) %>%
    check_class(diet, location) %>%
    prep(okc[1:10,], strings_as_factors = FALSE)

  expect_error(bake(rec5_man, okc[11:20, ]), NA)

  rec6_NULL <- recipe(okc[1:10,], age ~ .) %>%
    check_class(everything()) %>%
    prep(okc[1:10, ], strings_as_factors = TRUE)

  expect_error(bake(rec6_NULL, okc[11:20, ]),
               err6, fixed = TRUE)

  rec6_man <- recipe(okc[1:10,], age ~ .) %>%
    check_class(diet) %>%
    prep(okc[1:10, ], strings_as_factors = TRUE)

  expect_error(bake(rec6_man, okc[11:20, ]),
               err6, fixed = TRUE)
})

test_that('printing', {
  rec7 <- recipe(x) %>% check_class(everything())
  expect_output(print(rec7))
  expect_output(prep(rec7, training = x, verbose = TRUE))
})
