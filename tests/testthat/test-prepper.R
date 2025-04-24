test_that("prepper uses fresh = TRUE", {
  skip_if_not_installed("rsample")
  set.seed(123)

  train1 <- mtcars[1:20, ]
  train2 <- mtcars[21:32, ]

  rec <- recipe(cyl ~ mpg, train1) |>
    step_center(mpg)

  prepped_rec <- prep(rec, train1)

  split2 <- rsample::initial_split(train2)
  prepped_rec2 <- prepper(split2, prepped_rec)

  expect_equal(
    prepped_rec2$steps[[1]]$means,
    c(mpg = mean(rsample::training(split2)$mpg))
  )
})
