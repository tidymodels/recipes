test_that("works with integer64", {
  df <- data.frame(x = bit64::as.integer64(1:5))

  rec <- recipe(~ x, df)
  rec <- step_scale(rec, x)

  prepped <- prep(rec, df)

  expect_equal(
    prepped$steps[[1]]$sds,
    c(x = sd(as.numeric(df$x)))
  )

  expect <- tibble::tibble(x = as.double(df$x) / sd(as.double(df$x)))

  expect_equal(
    juice(prepped),
    expect
  )
})
