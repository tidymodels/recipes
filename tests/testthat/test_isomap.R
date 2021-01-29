library(testthat)
library(recipes)

context("ISOmap")

## expected results form the `dimRed` package

exp_res <- structure(list(Isomap1 = c(0.312570873898531, 0.371885353599467, 2.23124009833741,
                                      0.248271457498181, -0.420128801874122),
                          Isomap2 = c(-0.443724171391742, -0.407721529759647, 0.245721022395862,
                                      3.112001672258, 0.0292770508011519),
                          Isomap3 = c(0.761529345514676, 0.595015565588918, 1.59943072269788,
                                      0.566884409484389, 1.53770327701819)),
                     .Names = c("Isomap1","Isomap2", "Isomap3"),
                     class = c("tbl_df", "tbl", "data.frame"),
                     row.names = c(NA, -5L))

set.seed(1)
dat1 <- matrix(rnorm(15), ncol = 3)
dat2 <- matrix(rnorm(15), ncol = 3)
colnames(dat1) <- paste0("x", 1:3)
colnames(dat2) <- paste0("x", 1:3)

rec <- recipe( ~ ., data = dat1)

test_that('correct Isomap values', {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  im_rec <- rec %>%
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


test_that('printing', {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  im_rec <- rec %>%
    step_isomap(x1, x2, x3, neighbors = 3, num_terms = 3)
  expect_output(print(im_rec))
  expect_output(prep(im_rec, training = dat1, verbose = TRUE))
})


test_that('No ISOmap', {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  im_rec <- rec %>%
    step_isomap(x1, x2, x3, neighbors = 3, num_terms = 0, id = "") %>%
    prep()

  expect_equal(
    names(juice(im_rec)),
    colnames(dat1)
  )
  expect_true(inherits(im_rec$steps[[1]]$res, "list"))
  expect_output(print(im_rec),
                regexp = "Isomap was not conducted")
  expect_equal(
    tidy(im_rec, 1),
    tibble::tibble(terms = im_rec$steps[[1]]$res$x_vars, id = "")
  )
})


test_that('ISOmap fails gracefully', {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  expect_error(
    recipe(Sepal.Length ~ ., data = iris) %>%
      step_bs(Sepal.Width, deg_free = 1, degree = 1) %>%
      step_bs(Sepal.Length, deg_free = 1, degree = 1) %>%
      step_other(Species, threshold = .000000001) %>%
      step_isomap(all_numeric_predictors(), num_terms = 1, neighbors = 1) %>%
      prep(),
    "eigen decomposition failed"
  )
})


test_that('tunable', {
  rec <-
    recipe(~ ., data = iris) %>%
    step_isomap(all_predictors())
  rec_param <- tunable.step_isomap(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_terms", "neighbors"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})

test_that('keep_original_cols works', {

  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  im_rec <- rec %>%
    step_isomap(x1, x2, x3, neighbors = 3, num_terms = 3, id = "", keep_original_cols = TRUE)

  im_trained <- prep(im_rec, training = dat1, verbose = FALSE)

  im_pred <- bake(im_trained, new_data = dat2)

  expect_equal(
    colnames(im_pred),
    c("x1", "x2", "x3",
      "Isomap1", "Isomap2", "Isomap3")
  )
})

test_that('can prep recipes with no keep_original_cols', {
  skip_on_cran()
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("igraph")
  skip_if_not_installed("RANN")
  skip_if_not_installed("dimRed")
  skip_if(getRversion() <= "3.4.4")

  im_rec <- rec %>%
    step_isomap(x1, x2, x3, neighbors = 3, num_terms = 3)

  im_rec$steps[[1]]$keep_original_cols <- NULL

  expect_warning(
    im_trained <- prep(im_rec, training = dat1, verbose = FALSE),
    "'keep_original_cols' was added to"
  )

  expect_error(
    im_pred <- bake(im_trained, new_data = dat2, all_predictors()),
    NA
  )

})
