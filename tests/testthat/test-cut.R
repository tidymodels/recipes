test_that("step_cut throws error on non-numerics", {
  x <- tibble(num_var = 1:3, cat_var = c("1", "2", "3"))
  expect_no_error(recipe(x) |> step_cut(num_var, breaks = 2) |> prep())
  expect_snapshot(
    error = TRUE,
    recipe(x) |> step_cut(cat_var, breaks = 2) |> prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., x) |> step_cut(all_predictors(), breaks = 2) |> prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., x) |>
      step_cut(num_var, breaks = 2, include_outside_range = 2) |>
      prep()
  )
})

test_that("create_full_breaks helper function", {
  var1 <- 1:100
  var2 <- 75:150
  var3 <- 1:25
  breaks1 <- 50
  breaks2 <- c(50, 70)
  var4 <- 50:71
  var5 <- 49:70
  expect_equal(create_full_breaks(var1, breaks1), c(1, 50, 100))
  expect_equal(create_full_breaks(var1, breaks2), c(1, 50, 70, 100))
  expect_equal(create_full_breaks(var2, breaks1), c(50, 150))
  expect_equal(create_full_breaks(var2, breaks2), c(50, 70, 150))
  expect_equal(create_full_breaks(var3, breaks1), c(1, 50))
  expect_equal(create_full_breaks(var3, breaks2), c(1, 50, 70))
  expect_equal(create_full_breaks(var4, breaks2), c(50, 70, 71))
  expect_equal(create_full_breaks(var5, breaks2), c(49, 50, 70))
})

test_that("full_breaks_check will give warnings", {
  expect_snapshot(error = TRUE, full_breaks_check(10))
  expect_snapshot(
    full_breaks_check(c(10, 20))
  )
  expect_no_error(full_breaks_check(c(10, 20, 30)))
  expect_no_warning(full_breaks_check(c(10, 20, 30)))
})

test_that("cut_var gives correct output", {
  var1 <- 1:10
  var2 <- 2:11
  breaks <- c(1, 5, 10)

  expect_equal(
    cut_var(var1, breaks, TRUE),
    factor(
      rep(c("[min,5]", "(5,max]"), each = 5),
      levels = c("[min,5]", "(5,max]")
    )
  )

  expect_equal(
    cut_var(var2, breaks, TRUE),
    factor(
      rep(c("[min,5]", "(5,max]"), c(4, 6)),
      levels = c("[min,5]", "(5,max]")
    )
  )

  expect_equal(
    cut_var(var2, breaks, FALSE),
    factor(
      c(rep(c("[1,5]", "(5,10]"), c(4, 5)), NA),
      levels = c("[1,5]", "(5,10]")
    )
  )
})

test_that("adjust_levels_min_max gives correct output", {
  f1 <- cut(c(3, 5, 9), c(1, 4, 7, 10), include.lowest = TRUE)
  f2 <- cut(c(3, 5, 9), c(1, 7, 10), include.lowest = TRUE)
  f3 <- cut(c(3, 5, 9), c(1, 10), include.lowest = TRUE)
  f1_res <- factor(
    c("[min,4]", "(4,7]", "(7,max]"),
    levels = c("[min,4]", "(4,7]", "(7,max]")
  )
  f2_res <- factor(
    c("[min,7]", "[min,7]", "(7,max]"),
    levels = c("[min,7]", "(7,max]")
  )
  f3_res <- factor(rep("[min,max]", 3))
  expect_equal(adjust_levels_min_max(f1), f1_res)
  expect_equal(adjust_levels_min_max(f2), f2_res)
  expect_equal(adjust_levels_min_max(f3), f3_res)
})

test_that("step_cut integration test", {
  tb <- tibble(x = c(2, 5, 7), y = c(1, 8, 12))
  tb2 <- tibble(x = c(5, 9), y = c(1, 1))
  tb3 <- tibble(x = c(-1, 6), y = c(1, 1))

  expect_equal(
    recipe(tb) |>
      step_cut(x, breaks = 5) |>
      prep() |>
      bake(tb) |>
      pull(x),
    factor(c("[2,5]", "[2,5]", "(5,7]"), levels = c("[2,5]", "(5,7]"))
  )

  expect_equal(
    recipe(tb) |>
      step_cut(y, breaks = c(3, 9)) |>
      prep() |>
      bake(tb) |>
      pull(y),
    factor(
      c("[1,3]", "(3,9]", "(9,12]"),
      levels = c("[1,3]", "(3,9]", "(9,12]")
    )
  )

  result <- tibble(
    x = factor(
      c("[2,4]", "(4,6]", "(6,7]"),
      levels = c("[2,4]", "(4,6]", "(6,7]")
    ),
    y = factor(
      c("[1,4]", "(6,12]", "(6,12]"),
      levels = c("[1,4]", "(4,6]", "(6,12]")
    )
  )

  expect_equal(
    recipe(tb) |>
      step_cut(x, y, breaks = c(4, 6)) |>
      prep() |>
      bake(tb),
    result
  )

  expect_equal(
    recipe(tb) |>
      step_cut(x, breaks = 5, include_outside_range = TRUE) |>
      prep() |>
      bake(tb2) |>
      pull(x),
    factor(c("[min,5]", "(5,max]"), levels = c("[min,5]", "(5,max]"))
  )

  expect_equal(
    recipe(tb) |>
      step_cut(x, breaks = 5, include_outside_range = TRUE) |>
      prep() |>
      bake(tb3) |>
      pull(x),
    factor(c("[min,5]", "(5,max]"), levels = c("[min,5]", "(5,max]"))
  )
})

test_that("tidy method works", {
  rec <- recipe(~., data = mtcars) |>
    step_cut(disp, hp, breaks = 200) |>
    prep()

  res <- tidy(rec, 1)

  expect_identical(
    rep(c("disp", "hp"), each = 3),
    res$terms
  )

  expect_identical(
    c(
      min(mtcars$disp),
      200,
      max(mtcars$disp),
      min(mtcars$hp),
      200,
      max(mtcars$hp)
    ),
    res$value
  )
})

test_that("step_cut() provides informative warning on missing values", {
  # Single missing value
  mtcars_with_na <- mtcars
  mtcars_with_na[1, "mpg"] <- NA

  expect_snapshot(
    recipe(~., data = mtcars_with_na) |>
      step_cut(mpg, breaks = 20) |>
      prep()
  )

  # Multiple missing values
  mtcars_with_nas <- mtcars
  mtcars_with_nas[c(1, 3, 5), "mpg"] <- NA

  expect_snapshot(
    recipe(~., data = mtcars_with_nas) |>
      step_cut(mpg, breaks = 20) |>
      prep()
  )
})

test_that("step_cut() can handle missig missing values in bake() (#1304)", {
  mtcars_with_na <- mtcars
  mtcars_with_na[c(1, 10, 20), "mpg"] <- NA

  suppressWarnings(
    rec <- recipe(~., data = mtcars_with_na) |>
      step_cut(mpg, breaks = 20) |>
      prep()
  )

  exp <- bake(rec, mtcars)$mpg
  exp[c(1, 10, 20)] <- NA

  expect_identical(
    bake(rec, mtcars_with_na)$mpg,
    exp
  )
})

test_that("breaks argument are type checked", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_cut(disp, hp, breaks = TRUE) |>
      prep()
  )

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_cut(disp, hp, breaks = c("100", "200")) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  df <- data.frame(x = 1:10, y = 5:14)
  rec <- recipe(df)

  # The min and max of the variable are used as boundaries
  # if they exceed the breaks
  prepped <- rec |>
    step_cut(x, breaks = 5) |>
    update_role(x, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE) |>
    prep()

  expect_snapshot(error = TRUE, bake(prepped, df[, 2, drop = FALSE]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_cut(rec, breaks = 5)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_cut(rec1, breaks = 5)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_cut(rec, breaks = 5)

  expect <- tibble(terms = character(), value = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(mpg ~ ., mtcars) |>
    step_cut(disp, breaks = 100)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(~., data) |>
    step_cut(disp, breaks = 100) |>
    prep()

  expect_identical(
    nrow(bake(rec, slice(data, 1))),
    1L
  )
  expect_identical(
    nrow(bake(rec, slice(data, 0))),
    0L
  )
})
