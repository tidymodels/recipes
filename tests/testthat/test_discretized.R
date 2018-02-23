library(testthat)
library(recipes)


ex_tr <- data.frame(x1 = 1:100,
                    x2 = rep(1:5, each = 20),
                    x3 = factor(rep(letters[1:2], each = 50)))
ex_tr_mis <- ex_tr
ex_tr_mis$x1[2] <- NA
ex_tr_mis$x3[10] <- NA

ex_te <- data.frame(x1 = c(1, 50, 101, NA))

lvls_breaks_4 <- c('bin_missing', 'bin1', 'bin2', 'bin3', 'bin4')

test_that('default args', {
  bin_1 <- discretize(ex_tr$x1)
  pred_1 <- predict(bin_1, ex_te$x1)
  exp_1 <- factor(c("bin1", "bin2", "bin4", "bin_missing"), levels = lvls_breaks_4)
  expect_equal(pred_1, exp_1)
})

test_that('NA values', {
  bin_2 <- discretize(ex_tr$x1, keep_na = FALSE)
  pred_2 <- predict(bin_2, ex_te$x1)
  exp_2 <- factor(c("bin1", "bin2", "bin4", NA), levels = lvls_breaks_4[-1])
  expect_equal(pred_2, exp_2)
})

test_that('NA values from out of range', {
  bin_3 <- discretize(ex_tr$x1, keep_na = FALSE, infs = FALSE)
  pred_3 <- predict(bin_3, ex_te$x1)
  exp_3 <- factor(c("bin1", "bin2", NA, NA), levels = lvls_breaks_4[-1])
  expect_equal(pred_3, exp_3)
})


test_that('NA values with step_discretize (issue #127)', {
  iris_na <- iris
  iris_na$sepal_na <- iris_na$Sepal.Length
  iris_na$sepal_na[1:5] = NA

  disc_values <-
    discretize(
      iris_na$sepal_na,
      min.unique = 2,
      cuts = 2,
      keep_na = TRUE,
      na.rm = TRUE
    )

  opts <- list(min.unique = 2, cuts = 2, keep_na = TRUE, na.rm = TRUE)

  rec <- recipe( ~ ., data = iris_na) %>%
    step_discretize(sepal_na, options = opts) %>%
    prep(training = iris_na)

  expect_equal(rec$steps[[1]]$objects$sepal_na, disc_values)
})

test_that('printing and tidys', {
  rec <- recipe(~., data = ex_tr) %>%
    step_discretize(x1)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_tr, verbose = TRUE))

  tidy_exp_un <- tibble(
    terms = "x1",
    value = NA_real_
  )
  expect_equal(tidy(rec, 1), tidy_exp_un)

  rec_trained <- prep(rec, training = ex_tr)
  br <- rec_trained$steps[[1]]$objects$x1$breaks
  tidy_exp_tr <- tibble(
    terms = rep("x1", length(br)),
    value = br
  )
  expect_equal(tidy(rec_trained, 1), tidy_exp_tr)

})
