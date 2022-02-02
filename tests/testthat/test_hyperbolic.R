library(testthat)
library(recipes)
library(tibble)


n <- 20
ex_dat <- data.frame(x1 = seq(0, 1, length = n),
                     x2 = seq(1, 0, length = n))

get_exp <- function(x, f)
  as_tibble(lapply(x, f))


test_that('simple hyperbolic trans', {

  for(func in c("sin", "cos", "tan")) {
    for(invf in c(TRUE, FALSE)) {
      rec <- recipe(~., data = ex_dat) %>%
        step_hyperbolic(x1, x2, func = func, inverse = invf)

      rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
      rec_trans <- bake(rec_trained, new_data = ex_dat)

      if(invf) {
        foo <- get(paste0("a", func))
      } else {
        foo <- get(func)
      }

      exp_res <- get_exp(ex_dat, foo)

      expect_equal(rec_trans, exp_res)
    }
  }

})


test_that('printing', {
  rec <- recipe(~., data = ex_dat) %>%
    step_hyperbolic(x1, x2, func = "sin", inverse = TRUE)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat, verbose = TRUE))
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_hyperbolic(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_hyperbolic(rec)

  expect <- tibble(
    terms = character(),
    inverse = logical(),
    func = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_hyperbolic(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
