test_that("check_new_data works", {
  set.seed(313)
  examples <- matrix(exp(rnorm(40)), ncol = 4)
  examples <- as.data.frame(examples)

  log_trans <- recipe(~ V1 + V2 + V3 + V4, data = examples) |>
    step_log(V1, V2, V3) |>
    update_role(V1, V2, V3, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  log_obj <- prep(log_trans, training = examples)

  expect_snapshot(bake(log_obj, examples[, 2:4, drop = FALSE]), error = TRUE)
  expect_snapshot(bake(log_obj, examples[, 3:4, drop = FALSE]), error = TRUE)
  expect_snapshot(bake(log_obj, examples[, 4, drop = FALSE]), error = TRUE)
})

test_that("conditionMessage method for recipes errors works", {
  res <-
    try(
      {
        recipe(~., data = mtcars) |>
          step_dummy(all_numeric_predictors()) |>
          prep()
      },
      silent = TRUE
    )

  expect_s3_class(attr(res, "condition"), "recipes_error")

  expect_snapshot(conditionMessage(attr(res, "condition")))
})

test_that("validate_training_data errors are thrown", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |> prep(fresh = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |> prep(mtcars[, 1:2], fresh = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_center(disp) |>
      prep(retain = FALSE) |>
      prep(mtcars, fresh = FALSE)
  )

  expect_snapshot(
    tmp <- recipe(~., data = mtcars) |>
      step_center(disp) |>
      prep() |>
      prep(mtcars)
  )
})

test_that("vars without role in predictor/outcome avoid string processing", {
  x <- tibble(
    real_pred = 1:5,
    chr_pred_and_lime = letters[1:5],
    chr_outcome = letters[1:5],
    chr_only_lemon = letters[1:5],
    chr_only_lime = letters[1:5]
  )
  var_info <- tibble(variable = names(x), source = "original")
  var_info <- full_join(get_types(x), var_info, by = "variable")
  var_info$role <- c("predictor", "predictor", "outcome", "lemon", "lime")
  additional_row <- var_info[2, ]
  additional_row$role <- "lime"
  var_info <- var_info |> add_row(additional_row)

  orig_lvls <- lapply(x, get_levels)
  training <- strings2factors(x, orig_lvls)
  original_expectation <- c(FALSE, rep(TRUE, 4))
  names(original_expectation) <- names(x)
  expect_identical(has_lvls(orig_lvls), original_expectation)
  expect_identical(orig_lvls$real_pred, list(values = NA, ordered = NA))
  expect_identical(
    orig_lvls$chr_pred_and_lime,
    list(values = letters[1:5], ordered = FALSE, factor = FALSE)
  )
  expect_identical(
    orig_lvls$chr_outcome,
    list(values = letters[1:5], ordered = FALSE, factor = FALSE)
  )
  expect_identical(
    orig_lvls$chr_only_lemon, # gets converted to fctr
    list(values = letters[1:5], ordered = FALSE, factor = FALSE)
  )
  expect_identical(
    orig_lvls$chr_only_lime, # gets converted to fctr
    list(values = letters[1:5], ordered = FALSE, factor = FALSE)
  )

  new_lvls <- kill_levels(orig_lvls, var_info)
  new_expect <- original_expectation
  new_expect[4:5] <- FALSE
  expect_identical(has_lvls(new_lvls), new_expect)
  expect_identical(new_lvls$real_pred, orig_lvls$real_pred)
  # chr predictor gets converted, despite also having another role
  expect_identical(new_lvls$chr_pred_and_lime, orig_lvls$chr_pred_and_lime)
  expect_identical(new_lvls$chr_outcome, orig_lvls$chr_outcome)
  # non-predictor / non-outcome var remains chr, we don't log the levels
  expect_identical(new_lvls$chr_only_lemon, list(values = NA, ordered = NA))
  expect_identical(new_lvls$chr_only_lime, list(values = NA, ordered = NA))
})

test_that("spline error messages", {
  skip_if_not_installed("splines2")

  local_mocked_bindings(
    .package = "splines2",
    cSpline = function(...) {
      cli::cli_abort("mocked error")
    }
  )

  expect_snapshot(
    error = TRUE,
    recipe(. ~ disp, data = mtcars) |>
      step_spline_convex(disp) |>
      prep()
  )
})

test_that("names0() error on non-positive number", {
  expect_snapshot(
    error = TRUE,
    names0(0)
  )
})

test_that("ellipse_check() errors on empty selection", {
  expect_snapshot(
    error = TRUE,
    ellipse_check()
  )
})

test_that("ellipse_check() errors on empty selection", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  x <- 2
  class(x) <- "dimRedResult"
  expect_snapshot(
    error = TRUE,
    uses_dim_red(x)
  )
})

test_that("check_options() works", {
  expect_no_error(
    check_options(NULL)
  )
  expect_no_error(
    check_options(list())
  )
  expect_snapshot(
    error = TRUE,
    check_options(c("unname", "arguments"))
  )
  expect_snapshot(
    error = TRUE,
    check_options(list("unname", "arguments"))
  )
  expect_snapshot(
    error = TRUE,
    check_options(list(a = 1, b = 2), exclude = "b")
  )
  expect_snapshot(
    error = TRUE,
    check_options(list(a = 1, b = 2), include = "b")
  )
})

test_that("recipes_argument_select() works with single selection", {
  rec <- recipe(~., data = mtcars)
  info <- rec$var_info
  helper <- function(x) {
    recipes_argument_select(enquos(x), mtcars, info)
  }

  expect_identical(
    helper(drat),
    "drat"
  )
  expect_identical(
    helper("drat"),
    "drat"
  )
  expect_identical(
    helper(vars(drat)),
    "drat"
  )
  expect_identical(
    helper(imp_vars(drat)),
    "drat"
  )
  expect_identical(
    helper(starts_with("dra")),
    "drat"
  )

  expect_snapshot(
    error = TRUE,
    helper(NULL)
  )
  expect_snapshot(
    error = TRUE,
    helper(not_mpg)
  )

  expect_snapshot(
    error = TRUE,
    helper(c())
  )
  expect_snapshot(
    error = TRUE,
    helper(vars())
  )
  expect_snapshot(
    error = TRUE,
    helper(imp_vars())
  )

  expect_snapshot(
    error = TRUE,
    helper(c(mpg, disp))
  )
  expect_snapshot(
    error = TRUE,
    helper(c("mpg", "disp"))
  )
  expect_snapshot(
    error = TRUE,
    helper(vars(mpg, disp))
  )
  expect_snapshot(
    error = TRUE,
    helper(imp_vars(mpg, disp))
  )
})

test_that("recipes_argument_select() works with multiple selections", {
  rec <- recipe(mpg ~ ., data = mtcars)
  info <- rec$var_info
  helper <- function(x) {
    recipes_argument_select(enquos(x), mtcars, info, single = FALSE)
  }

  expect_identical(
    helper(drat),
    "drat"
  )
  expect_identical(
    helper("drat"),
    "drat"
  )
  expect_identical(
    helper(vars(drat)),
    "drat"
  )
  expect_identical(
    helper(imp_vars(drat)),
    "drat"
  )
  expect_identical(
    helper(starts_with("dra")),
    "drat"
  )

  expect_identical(
    helper(c(mpg, disp)),
    c("mpg", "disp")
  )
  expect_identical(
    helper(c("mpg", "disp")),
    c("mpg", "disp")
  )
  expect_identical(
    helper(vars(mpg, disp)),
    c("mpg", "disp")
  )
  expect_identical(
    helper(imp_vars(mpg, disp)),
    c("mpg", "disp")
  )
  expect_identical(
    helper(all_predictors()),
    setdiff(names(mtcars), "mpg")
  )

  expect_snapshot(
    error = TRUE,
    helper(NULL)
  )
  expect_snapshot(
    error = TRUE,
    helper(not_mpg)
  )

  expect_snapshot(
    error = TRUE,
    helper(c())
  )
  expect_snapshot(
    error = TRUE,
    helper(vars())
  )
  expect_snapshot(
    error = TRUE,
    helper(imp_vars())
  )
})

test_that("recipes_argument_select() errors on case_weights", {
  mtcars$gear <- hardhat::importance_weights(mtcars$gear)
  rec <- recipe(mpg ~ ., data = mtcars)
  info <- rec$var_info
  helper <- function(x) {
    recipes_argument_select(enquos(x), mtcars, info, single = FALSE)
  }

  expect_snapshot(
    error = TRUE,
    helper(gear)
  )
  expect_snapshot(
    error = TRUE,
    helper(gear)
  )
  expect_snapshot(
    error = TRUE,
    helper(vars(gear))
  )
  expect_snapshot(
    error = TRUE,
    helper(imp_vars(gear))
  )
  expect_snapshot(
    error = TRUE,
    helper(starts_with("gea"))
  )

  expect_snapshot(
    error = TRUE,
    helper(c(mpg, gear))
  )
  expect_snapshot(
    error = TRUE,
    helper(c("mpg", "gear"))
  )
  expect_snapshot(
    error = TRUE,
    helper(vars(mpg, gear))
  )
  expect_snapshot(
    error = TRUE,
    helper(imp_vars(mpg, gear))
  )
})
