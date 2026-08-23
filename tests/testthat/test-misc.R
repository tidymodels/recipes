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

test_that("recipes_map_cols() transforms the selected columns", {
  data <- tibble(a = 1:3, b = 4:6, c = 7:9)
  offsets <- c(10L, 20L)

  expect_identical(
    recipes_map_cols(data, c("a", "c"), \(x, i, col_name) x + offsets[[i]]),
    tibble(a = 11:13, b = 4:6, c = 27:29)
  )
})

test_that("recipes_map_cols() passes position and name to `fn`", {
  data <- tibble(a = 1, b = 2, c = 3)
  seen <- list()

  recipes_map_cols(data, c("c", "a"), function(x, i, col_name) {
    seen[[length(seen) + 1]] <<- list(i = i, col_name = col_name)
    x
  })

  expect_identical(
    seen,
    list(list(i = 1L, col_name = "c"), list(i = 2L, col_name = "a"))
  )
})

test_that("recipes_map_cols() only passes the arguments `fn` accepts", {
  data <- tibble(a = 1, b = 2)

  expect_identical(
    recipes_map_cols(data, c("a", "b"), \(x) x * 10),
    tibble(a = 10, b = 20)
  )
  expect_identical(
    recipes_map_cols(data, c("a", "b"), \(x, i) x + i),
    tibble(a = 2, b = 4)
  )
  expect_identical(
    recipes_map_cols(data, c("a", "b"), \(x, i, col_name) x + nchar(col_name)),
    tibble(a = 2, b = 3)
  )
  # dots absorb the optional arguments
  expect_identical(
    recipes_map_cols(data, c("a", "b"), \(x, ...) x * 10),
    tibble(a = 10, b = 20)
  )
  # primitives have no formals but still take the column
  expect_identical(
    recipes_map_cols(tibble(a = 4, b = 9), c("a", "b"), sqrt),
    tibble(a = 2, b = 3)
  )
})

test_that("recipes_map_cols() is a no-op for empty selections", {
  data <- tibble(a = 1:3, b = 4:6)

  expect_identical(recipes_map_cols(data, character(), \(x, i, nm) x + 1), data)
  expect_identical(recipes_map_cols(data, NULL, \(x, i, nm) x + 1), data)
})

test_that("recipes_map_cols() preserves the class of `new_data`", {
  double_it <- \(x, i, col_name) x * 2

  expect_s3_class(
    recipes_map_cols(tibble(a = 1:3), "a", double_it),
    class(tibble(a = 1:3)),
    exact = TRUE
  )
  expect_s3_class(
    recipes_map_cols(data.frame(a = 1:3), "a", double_it),
    "data.frame",
    exact = TRUE
  )
})

test_that("recipes_map_cols() leaves column types alone", {
  data <- tibble(fct = factor(c("a", "b")), chr = c("x", "y"))

  expect_identical(
    recipes_map_cols(data, "chr", \(x, i, col_name) toupper(x)),
    tibble(fct = factor(c("a", "b")), chr = c("X", "Y"))
  )
})

test_that("recipes_map_cols() works with sparse vectors", {
  data <- tibble(x = sparsevctrs::as_sparse_double(c(0, 0, 4)))

  res <- recipes_map_cols(data, "x", function(x, i, col_name) {
    sparsevctrs::sparse_division_scalar(x, 2)
  })

  expect_true(sparsevctrs::is_sparse_double(res$x))
  expect_identical(as.double(res$x), c(0, 0, 2))
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
