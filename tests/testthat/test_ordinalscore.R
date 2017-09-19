library(testthat)
library(recipes)

n <- 20

set.seed(752)
ex_dat <- data.frame(
  numbers = rnorm(n),
  fact = factor(sample(letters[1:3], n, replace = TRUE)),
  ord1 = factor(sample(LETTERS[1:3], n, replace = TRUE),
               ordered = TRUE),
  ord2 = factor(sample(LETTERS[4:8], n, replace = TRUE),
                ordered = TRUE),
  ord3 = factor(sample(LETTERS[10:20], n, replace = TRUE),
                ordered = TRUE)
)

ex_miss <- ex_dat
ex_miss$ord1[c(1, 5, 9)] <- NA
ex_miss$ord3[2] <- NA

score <- function(x) as.numeric(x)^2

test_that('linear scores', {
  rec1 <- recipe(~ ., data = ex_dat) %>%
    step_ordinalscore(starts_with("ord"))
  rec1 <- prep(rec1, training = ex_dat, retain = TRUE,
                  stringsAsFactors = FALSE, verbose = FALSE)
  rec1_scores <- bake(rec1, newdata = ex_dat)
  rec1_scores_NA <- bake(rec1, newdata = ex_miss)

  expect_equal(as.numeric(ex_dat$ord1), rec1_scores$ord1)
  expect_equal(as.numeric(ex_dat$ord2), rec1_scores$ord2)
  expect_equal(as.numeric(ex_dat$ord3), rec1_scores$ord3)

  expect_equal(as.numeric(ex_miss$ord1), rec1_scores_NA$ord1)
  expect_equal(as.numeric(ex_miss$ord3), rec1_scores_NA$ord3)
})

test_that('nonlinear scores', {
  rec2 <- recipe(~ ., data = ex_dat) %>%
    step_ordinalscore(starts_with("ord"),
                      convert = score)
  rec2 <- prep(rec2, training = ex_dat, retain = TRUE,
                  stringsAsFactors = FALSE, verbose = FALSE)
  rec2_scores <- bake(rec2, newdata = ex_dat)
  rec2_scores_NA <- bake(rec2, newdata = ex_miss)

  expect_equal(as.numeric(ex_dat$ord1)^2, rec2_scores$ord1)
  expect_equal(as.numeric(ex_dat$ord2)^2, rec2_scores$ord2)
  expect_equal(as.numeric(ex_dat$ord3)^2, rec2_scores$ord3)

  expect_equal(as.numeric(ex_miss$ord1)^2, rec2_scores_NA$ord1)
  expect_equal(as.numeric(ex_miss$ord3)^2, rec2_scores_NA$ord3)
})

test_that('bad spec', {
  rec3 <- recipe(~ ., data = ex_dat) %>%
    step_ordinalscore(everything())
  expect_error(prep(rec3, training = ex_dat, verbose = FALSE))
  rec4 <- recipe(~ ., data = ex_dat)
  expect_error(rec4 %>% step_ordinalscore())
})


test_that('printing', {
  rec5 <- recipe(~ ., data = ex_dat) %>%
    step_ordinalscore(starts_with("ord"))
  expect_output(print(rec5))
  expect_output(prep(rec5, training = ex_dat, verbose = TRUE))
})

