library(testthat)
library(recipes)

n <- 50
set.seed(424)
dat <- data.frame(x1 = rnorm(n),
                  x2 = rep(1:5, each = 10),
                  x3 = factor(rep(letters[1:3], c(2, 2, 46))),
                  x4 = 1,
                  y = runif(n))


ratios <- function(x) {
  tab <- sort(table(x), decreasing = TRUE)
  if(length(tab) > 1)
    tab[1]/tab[2] else Inf
}

pct_uni <- vapply(dat[, -5], function(x) length(unique(x)), c(val = 0))/nrow(dat)*100
f_ratio <- vapply(dat[, -5], ratios, c(val = 0))
vars <- names(pct_uni)

test_that('zv filtering', {
  rec <- recipe(y ~ ., data = dat)
  filtering <- rec %>%
    step_zv(x1, x2, x3, x4)

  filtering_trained <- prep(filtering, training = dat, verbose = FALSE)

  expect_equal(filtering_trained$steps[[1]]$removals, "x4")
})

test_that('printing', {
  rec <- recipe(y ~ ., data = dat) %>%
    step_zv(x1, x2, x3, x4)
  expect_output(print(rec))
  expect_output(prep(rec, training = dat, verbose = TRUE))
})


test_that('mssing values in zero-variance screen', {

  x <- rep(1, 5)
  y <- c(NA, x)
  z <- rep(NA, 5)

  expect_true(recipes:::one_unique(x))
  expect_true(recipes:::one_unique(y))
  expect_true(recipes:::one_unique(z))

})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_zv(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_zv(rec)

  expect <- tibble(
    terms = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_zv(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
