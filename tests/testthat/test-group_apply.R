test_that("step_group", {

  df <- tibble::tibble(
    g = sample(letters[1:3], size = 100, replace = TRUE),
    x = runif(100),
    y = rnorm(100)
  )

  grouped_rec <- recipes::recipe(y ~ ., data = df) %>%
    recipes::step_center(x, y)

  rec <- recipes::recipe(y ~ ., df) %>%
    step_group_apply(g, grouped_rec)

  rec <- recipes::prep(rec, df)

  out <- bake(rec, df)
  expect_equal(
    out,
    df %>%
      dplyr::group_by(g) %>%
      dplyr::summarise(x = x - mean(x), y = y - mean(y), .groups = "drop") %>%
      dplyr::mutate(g = as.factor(g))
  )

})
