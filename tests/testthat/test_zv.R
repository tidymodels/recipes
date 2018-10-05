library(testthat)
library(recipes)


context("Zero variance filter")

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
