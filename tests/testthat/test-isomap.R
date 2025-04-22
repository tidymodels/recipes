library(testthat)
library(recipes)

## expected results form the `dimRed` package

exp_res <- structure(
  list(
    Isomap1 = c(
      0.312570873898531,
      0.371885353599467,
      2.23124009833741,
      0.248271457498181,
      -0.420128801874122
    ),
    Isomap2 = c(
      -0.443724171391742,
      -0.407721529759647,
      0.245721022395862,
      3.112001672258,
      0.0292770508011519
    ),
    Isomap3 = c(
      0.761529345514676,
      0.595015565588918,
      1.59943072269788,
      0.566884409484389,
      1.53770327701819
    )
  ),
  .Names = c("Isomap1", "Isomap2", "Isomap3"),
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(NA, -5L)
)

set.seed(1)
dat1 <- matrix(rnorm(15), ncol = 3)
dat2 <- matrix(rnorm(15), ncol = 3)
colnames(dat1) <- paste0("x", 1:3)
colnames(dat2) <- paste0("x", 1:3)

rec <- recipe(~., data = dat1)

scrub_timestamp <- function(x) {
  if (grepl("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", x[1])) {
    return(NULL)
  }
  x
}

test_that("correct Isomap values", {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  im_rec <- rec |>
    step_isomap(x1, x2, x3, neighbors = 3, num_terms = 3, id = "")

  im_trained <- prep(im_rec, training = dat1, verbose = FALSE)

  im_pred <- bake(im_trained, new_data = dat2)

  # unique up to sign
  all.equal(abs(as.matrix(im_pred)), abs(as.matrix(exp_res)))

  im_tibble <-
    tibble(terms = c("x1", "x2", "x3"), id = "")

  expect_equal(tidy(im_rec, 1), im_tibble)
  expect_equal(tidy(im_trained, 1), im_tibble)
})

test_that("No ISOmap", {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  im_rec <- rec |>
    step_isomap(x1, x2, x3, neighbors = 3, num_terms = 0, id = "") |>
    prep()

  expect_equal(
    names(bake(im_rec, new_data = NULL)),
    colnames(dat1)
  )
  expect_null(im_rec$steps[[1]]$res)
  expect_snapshot(print(im_rec))
  expect_equal(
    tidy(im_rec, 1),
    tibble::tibble(terms = unname(im_rec$steps[[1]]$columns), id = "")
  )
})

test_that("ISOmap fails gracefully", {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  expect_snapshot(
    error = TRUE,
    recipe(Sepal.Length ~ ., data = iris) |>
      step_bs(Sepal.Width, deg_free = 1, degree = 1) |>
      step_bs(Sepal.Length, deg_free = 1, degree = 1) |>
      step_other(Species, threshold = .000000001) |>
      step_isomap(all_numeric_predictors(), num_terms = 1, neighbors = 1) |>
      prep(),
    transform = scrub_timestamp
  )
})

test_that("check_name() is used", {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")
  dat <- dplyr::as_tibble(dat1)
  dat$Isomap1 <- dat$x1

  rec <- recipe(~., data = dat) |>
    step_isomap(x1, x2, x3, neighbors = 3, num_terms = 3)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat),
    transform = scrub_timestamp
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = iris) |>
    step_isomap(all_predictors())
  rec_param <- tunable.step_isomap(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_terms", "neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("check_options() is used", {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  expect_snapshot(
    error = TRUE,
    recipe(~mpg, data = mtcars) |>
      step_isomap(mpg, options = TRUE) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  im_rec <- rec |>
    step_isomap(x1, x2, x3, neighbors = 3, num_terms = 3) |>
    update_role(x1, x2, x3, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  im_trained <- prep(im_rec, training = dat1, verbose = FALSE)

  expect_snapshot(error = TRUE, bake(im_trained, new_data = dat2[, 1:2]))
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_isomap(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_isomap(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_isomap(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  new_names <- c("Isomap1", "Isomap2", "Isomap3")

  rec <- recipe(~., data = dat1) |>
    step_isomap(
      x1,
      x2,
      x3,
      neighbors = 3,
      num_terms = 3,
      keep_original_cols = FALSE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~., data = dat1) |>
    step_isomap(
      x1,
      x2,
      x3,
      neighbors = 3,
      num_terms = 3,
      keep_original_cols = TRUE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("x1", "x2", "x3", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  rec <- recipe(~., data = dat1) |>
    step_isomap(x1, x2, x3, neighbors = 3, num_terms = 3)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec),
    transform = scrub_timestamp
  )

  expect_no_error(
    bake(rec, new_data = dat1)
  )
})

test_that("printing", {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  rec <- recipe(~., data = dat1) |>
    step_isomap(x1, x2, x3, neighbors = 3, num_terms = 3)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec), transform = scrub_timestamp)
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_isomap(
      all_predictors(),
      num_terms = hardhat::tune(),
      neighbors = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})

test_that("bad args", {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")

  expect_snapshot(
    recipe(~., data = mtcars) |>
      step_isomap(all_predictors(), num_terms = 2, neighbors = -1 / 3) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(~., data = mtcars) |>
      step_isomap(all_predictors(), prefix = NULL) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  data <- as_tibble(dat1)
  rec <- recipe(~., data) |>
    step_isomap(all_predictors(), neighbors = 3, num_terms = 3) |>
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
