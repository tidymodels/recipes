library(testthat)
source(test_path("helper-case-weights.R"))

test_that('correct means', {
  exp_means <- colMeans(mtcars)
  calc_means <- averages(mtcars)
  expect_equal(exp_means, calc_means)

  exp_means <- cov.wt(mtcars, frac_wts)$center
  calc_means <- averages(mtcars, frac_wts)
  expect_equal(exp_means, calc_means)

  exp_means <- cov.wt(mtcars, freq_wts)$center
  calc_means <- averages(mtcars, freq_wts)
  expect_equal(exp_means, calc_means)

  # Missing case weight
  exp_means <- cov.wt(mtcars[-1,], freq_wts[-1])$center
  calc_means <- averages(mtcars, miss_wts)
  expect_equal(exp_means, calc_means)

  # Missing data in x
  exp_means <- cov.wt(mtcars, freq_wts)$center
  exp_means[2] <- weighted.mean(mtcar_mis[[2]], freq_wts, na.rm = TRUE)
  exp_means[3] <- weighted.mean(mtcar_mis[[3]], freq_wts, na.rm = TRUE)
  calc_means <- averages(mtcar_mis, freq_wts)
  expect_equal(exp_means, calc_means)

})


test_that('correct variances', {
  exp_vars <- diag(var(mtcars))
  calc_vars <- variances(mtcars)
  expect_equal(exp_vars, calc_vars)

  exp_vars <- diag(cov.wt(mtcars, frac_wts)$cov)
  calc_vars <- variances(mtcars, frac_wts)
  expect_equal(exp_vars, calc_vars)

  exp_vars <- diag(cov.wt(mtcars, freq_wts)$cov)
  calc_vars <- variances(mtcars, freq_wts)
  expect_equal(exp_vars, calc_vars)

  # Missing case weight
  exp_vars <- diag(cov.wt(mtcars[-1,], freq_wts[-1])$cov)
  calc_vars <- variances(mtcars, miss_wts)
  expect_equal(exp_vars, calc_vars)

  # Missing data in x
  exp_vars <- diag(cov.wt(mtcars, freq_wts)$cov)
  exp_vars[2] <- diag(cov.wt(mtcar_mis[-1,2,drop = FALSE], freq_wts[-1])$cov)
  exp_vars[3] <- diag(cov.wt(mtcar_mis[-2,3,drop = FALSE], freq_wts[-2])$cov)
  calc_vars <- variances(mtcar_mis, freq_wts)
  expect_equal(exp_vars, calc_vars)

})


test_that('correct correlations', {
  exp_cors <- cor(mtcars)
  calc_cors <- correlations(mtcars)
  expect_equal(exp_cors, calc_cors)

  exp_cors <- cov.wt(mtcars, frac_wts, cor = TRUE)$cor
  calc_cors <- correlations(mtcars, frac_wts)
  expect_equal(exp_cors, calc_cors)

  exp_cors <- cov.wt(mtcars, freq_wts, cor = TRUE)$cor
  calc_cors <- correlations(mtcars, freq_wts)
  expect_equal(exp_cors, calc_cors)

  # Missing case weight
  exp_cors <- cov.wt(mtcars[-1,], freq_wts[-1], cor = TRUE)$cor
  calc_cors <- correlations(mtcars, miss_wts)
  expect_equal(exp_cors, calc_cors)

  # Missing data in x
  exp_cors <- cov.wt(mtcars[-(1:2), 1:3], freq_wts[-(1:2)], cor = TRUE)$cor
  calc_cors <- correlations(mtcar_mis[, 1:3], freq_wts)
  expect_equal(exp_cors, calc_cors)

})


test_that('correct tables', {
  mtcars1 <- mtcars[c("cyl")]
  mtcars1$cyl <- factor(mtcars1$cyl)
  exp_table <- table(mtcars1)
  calc_table <- weighted_table(mtcars1)
  names(dimnames(exp_table)) <- "tmp"
  names(dimnames(calc_table)) <- "tmp"

  expect_equal(exp_table, calc_table)

  mtcars2 <- mtcars[c("cyl", "vs")]
  mtcars2$cyl <- factor(mtcars2$cyl)
  mtcars2$vs <- factor(mtcars2$vs)
  exp_table <- table(mtcars2)
  calc_table <- weighted_table(mtcars2)

  expect_equal(exp_table, calc_table)

  # Integer weights 1D
  exp_weigts <- mtcars %>% count(cyl, wt = carb)
  exp_table <- table(mtcars1)
  value_order <- match(names(exp_table), exp_weigts$cyl)
  exp_table[seq_along(exp_table)] <- exp_weigts$n[value_order]

  calc_table <- weighted_table(mtcars1, wts = mtcars$carb)
  names(dimnames(exp_table)) <- "tmp"
  names(dimnames(calc_table)) <- "tmp"

  expect_equal(exp_table, calc_table)

  # Irrational weights 1D
  exp_weigts <- mtcars %>% count(cyl, wt = qsec)
  exp_table <- table(mtcars1)
  exp_table[seq_along(exp_table)] <- exp_weigts$n

  calc_table <- weighted_table(mtcars1, wts = mtcars$qsec)
  names(dimnames(exp_table)) <- "tmp"
  names(dimnames(calc_table)) <- "tmp"

  expect_equal(exp_table, calc_table)

  # Irrational weights 2D
  exp_weigts <- mtcars %>%
    mutate(cyl = factor(cyl),
           vs = factor(vs)) %>%
    group_by(cyl, vs, .drop = FALSE) %>%
    summarise(n = sum(carb), .groups = "drop") %>%
    ungroup() %>%
    arrange(vs, cyl)
  exp_table <- table(mtcars2)
  exp_table[seq_along(exp_table)] <- exp_weigts$n

  calc_table <- weighted_table(mtcars2, wts = mtcars$carb)

  expect_equal(exp_table, calc_table)

  # Irrational weights 2D
  exp_weigts <- mtcars %>%
    mutate(cyl = factor(cyl),
           vs = factor(vs)) %>%
    group_by(cyl, vs, .drop = FALSE) %>%
    summarise(n = sum(qsec), .groups = "drop") %>%
    ungroup() %>%
    arrange(vs, cyl)
  exp_table <- table(mtcars2)
  exp_table[seq_along(exp_table)] <- exp_weigts$n

  calc_table <- weighted_table(mtcars2, wts = mtcars$qsec)

  expect_equal(exp_table, calc_table)

  expect_snapshot(weighted_table(mtcars["vs"]), error = TRUE)

  # Deal with empty levels
  f1 <- factor(c("A", "B"), levels = c("A", "B", "C"))
  exp_table <- table(f1)
  calc_table <- weighted_table(f1)
  names(dimnames(exp_table)) <- "tmp"
  names(dimnames(calc_table)) <- "tmp"

  expect_equal(exp_table, calc_table)
})

test_that("is_unsupervised_weights works", {
  expect_true(is_unsupervised_weights(frequency_weights(5)))
  expect_false(is_unsupervised_weights(importance_weights(5)))

  expect_snapshot(error = TRUE, is_unsupervised_weights(3))
})
