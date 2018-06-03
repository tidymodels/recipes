library(testthat)
library(recipes)
library(dplyr)

example_data <- npk
example_data$block <- NULL
example_data$y2 <- 1:nrow(example_data)
example_data$N <- as.numeric(example_data$N) - 1
example_data$P <- as.numeric(example_data$P) - 1
example_data$K <- as.numeric(example_data$K) - 1
tr_data <- example_data[1:20,]
te_data <- example_data[-(1:20),]

test_that('default values - multivariate', {
  default_mult_rec <- recipe(yield + y2 ~ ., data = tr_data) %>%
    step_pls(all_predictors(), outcome = vars(starts_with("y")))

  default_mult_ty_un <- tibble(
    terms = "all_predictors()",
    value = NA_real_, component = NA_character_)
  expect_equal(default_mult_ty_un, tidy(default_mult_rec, number = 1))

  default_mult_rec <- default_mult_rec %>%
    prep(training = tr_data, retain = TRUE)

  # Expected results based on
  # library(pls)
  # basic_mod <- plsr(cbind(yield, y2) ~ N + P + K, data = tr_data, ncomp = 2)
  # basic_mod_proj <- basic_mod$projection
  # basic_mod_xsc <- basic_mod$Xmeans
  # basic_mod_tr_sc <- unclass(basic_mod$scores)
  # basic_mod_tr_sc <- as_tibble(head(basic_mod_tr_sc))
  # colnames(basic_mod_tr_sc) <- c("PLS1", "PLS2")

  default_mult_proj <-
    structure(
      c(
        -0.801863006303567, 0.166086063360367, 0.573960920864184,-0.111851622567151,
        -0.985334606374407, 0.128860110236209
      ),
      .Dim = c(3L, 2L),
      .Dimnames = list(c("N", "P", "K"), c("Comp 1", "Comp 2"))
    )
  default_mult_xmn <- rep(.5, 3)

  default_mult_ty_tr <- default_mult_proj %>%
    as.data.frame() %>%
    stack() %>%
    mutate(
      ind = gsub("Comp ", "PLS", ind),
      terms = rep(rownames(default_mult_proj), 2)

    ) %>%
    setNames(c("value", "component", "terms")) %>%
    select(terms, value, component)

  default_mult_scores <-
    structure(
      list(
        PLS1 = c(
          0.770954995264059, -0.604868931903692, 0.0309080110395078,
          -0.196994074399875, -0.770954995264059, -0.0309080110395078
        ),
        PLS2 = c(
          -0.372311436785524, -0.613023169588883, 0.484163059352674,
          0.501171547021733, 0.372311436785524, -0.484163059352674
        )
      ),
      .Names = c("PLS1", "PLS2"),
      row.names = c(NA,-6L),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_equal(default_mult_rec$steps[[1]]$res$projection, default_mult_proj)
  expect_equal(default_mult_rec$steps[[1]]$res$Xmeans, default_mult_xmn)
  expect_equal(as.data.frame(head(juice(default_mult_rec, all_predictors()))),
               as.data.frame(default_mult_scores))
  expect_true(is.na(default_mult_rec$steps[[1]]$res$scale))
})

test_that('non-default values - multivariate', {
  nondefault_mult_rec <- recipe(yield + y2 ~ ., data = tr_data) %>%
    step_pls(
      all_predictors(),
      outcome = vars(starts_with("y")),
      options = list(method = "simpls", scale = TRUE)
    )

  nondefault_mult_rec <- nondefault_mult_rec %>%
    prep(training = tr_data, retain = TRUE)

  # Expected results based on
  # library(pls)
  # simpls_mod <- plsr(cbind(yield, y2) ~ N + P + K, data = tr_data, ncomp = 2, method = "simpls", scale = TRUE)
  # simpls_mod_proj <- simpls_mod$projection
  # simpls_mod_xsc <- simpls_mod$Xmeans
  # simpls_mod_ysc <- simpls_mod$scale
  # simpls_mod_tr_sc <- unclass(simpls_mod$scores)
  # simpls_mod_tr_sc <- as_tibble(head(simpls_mod_tr_sc))
  # colnames(simpls_mod_tr_sc) <- c("PLS1", "PLS2")

  nondefault_mult_proj <-
    structure(
      c(
        -0.183959990054788, 0.0381027561114912, 0.13167566587308,-0.025660522075856,
        -0.226051261829446, 0.0295625367564814
      ),
      .Dim = c(3L, 2L),
      .Dimnames = list(c("N", "P", "K"), c("Comp 1", "Comp 2"))
    )
  nondefault_mult_xmn <- c(0.974679434480897, 0.974679434480897, 0.974679434480897)
  nondefault_mult_ysc <- c(N = 0.512989176042577, P = 0.512989176042577, K = 0.512989176042577)

  nondefault_mult_ty_tr <- nondefault_mult_proj %>%
    as.data.frame() %>%
    stack() %>%
    mutate(
      ind = gsub("Comp ", "PLS", ind),
      terms = rep(rownames(nondefault_mult_proj), 2)

    ) %>%
    setNames(c("value", "component", "terms")) %>%
    select(terms, value, component)

  nondefault_mult_scores <-
    structure(
      list(
        PLS1 = c(
          0.344781555400693, -0.270505609842869,  0.0138224827467307,
          -0.0880984283045543, -0.344781555400693,-0.0138224827467307
        ),
        PLS2 = c(
          -0.166502736290609, -0.274152295796625, 0.216524302581369,
          0.224130729505865, 0.166502736290609, -0.216524302581369
        )
      ),
      .Names = c("PLS1", "PLS2"),
      row.names = c(NA,-6L),
      class = c("tbl_df", "tbl", "data.frame")
    )

  expect_equal(nondefault_mult_rec$steps[[1]]$res$projection, nondefault_mult_proj)
  expect_equal(nondefault_mult_rec$steps[[1]]$res$Xmeans, nondefault_mult_xmn)
  expect_equal(nondefault_mult_rec$steps[[1]]$res$scale, nondefault_mult_ysc)
  expect_equal(as.data.frame(head(juice(nondefault_mult_rec, all_predictors()))),
               as.data.frame(nondefault_mult_scores))
})



test_that('non-default values - univariate', {
  nondefault_uni_rec <- recipe(yield ~ N + P + K, data = tr_data) %>%
    step_pls(
      all_predictors(),
      outcome = "yield",
      options = list(method = "oscorespls", scale = TRUE)
    )

  nondefault_uni_rec <- nondefault_uni_rec %>%
    prep(training = tr_data, retain = TRUE)

  # Expected results based on
  # library(pls)
  # oscorespls_mod <- plsr(yield ~ N + P + K, data = tr_data, ncomp = 2, method = "oscorespls", scale = TRUE)
  # oscorespls_mod_proj <- oscorespls_mod$projection
  # oscorespls_mod_xsc <- oscorespls_mod$Xmeans
  # oscorespls_mod_ysc <- oscorespls_mod$scale
  # oscorespls_mod_tr_sc <- unclass(oscorespls_mod$scores)
  # oscorespls_mod_tr_sc <- as_tibble(head(oscorespls_mod_tr_sc))
  # colnames(oscorespls_mod_tr_sc) <- c("PLS1", "PLS2")

  nondefault_uni_proj <-
    structure(
      c(
        0.800815947734227, -0.175012756789599, -0.572769022220507,-0.376793063517125,
        -0.790419784069304, -0.285295367748138
      ),
      .Dim = c(3L, 2L),
      .Dimnames = list(c("N", "P", "K"), c("Comp 1", "Comp 2"))
    )
  nondefault_uni_xmn <- c(0.974679434480897, 0.974679434480897, 0.974679434480897)
  nondefault_uni_ysc <- c(N = 0.512989176042577, P = 0.512989176042577, K = 0.512989176042577)

  nondefault_uni_ty_tr <- nondefault_uni_proj %>%
    as.data.frame() %>%
    stack() %>%
    mutate(
      ind = gsub("Comp ", "PLS", ind),
      terms = rep(rownames(nondefault_uni_proj), 2)

    ) %>%
    setNames(c("value", "component", "terms")) %>%
    select(terms, value, component)

  nondefault_uni_scores <-
    structure(
      list(
        PLS1 = c(
          -1.50938635654157, 1.16822368691231, -0.0516913135801907,
          0.392853983209449, 1.50938635654157, 0.0516913135801907
        ),
        PLS2 = c(
          -0.68122498577076,-0.859586830507603, 1.41572988590115,
          0.125081930377211, 0.68122498577076,-1.41572988590115
        )
      ),
      .Names = c("PLS1", "PLS2"),
      row.names = c(NA, -6L),
      class = c("tbl_df", "tbl", "data.frame")
    )


  expect_equal(nondefault_uni_rec$steps[[1]]$res$projection, nondefault_uni_proj)
  expect_equal(nondefault_uni_rec$steps[[1]]$res$Xmeans, nondefault_uni_xmn)
  expect_equal(nondefault_uni_rec$steps[[1]]$res$scale, nondefault_uni_ysc)
  expect_equal(as.data.frame(head(juice(nondefault_uni_rec, all_predictors()))),
               as.data.frame(nondefault_uni_scores))
})

test_that('bad args', {
  expect_error(
    recipe(yield ~ N + P + K, data = tr_data) %>%
      step_pls(all_predictors())
  )
  expect_error(
    recipe(Species ~ ., data = iris) %>%
      step_pls(all_predictors(), outcome = "Species") %>%
      prep(training = iris)
  )
  expect_error(
    recipe(Sepal.Width ~ ., data = iris) %>%
      step_pls(all_predictors(), outcome = "Sepal.Width") %>%
      prep(training = iris)
  )
})

test_that('printing', {
  nondefault_uni_rec <- recipe(yield ~ N + P + K, data = tr_data) %>%
    step_pls(
      all_predictors(),
      outcome = "yield",
      options = list(method = "oscorespls", scale = TRUE)
    )
  expect_output(print(nondefault_uni_rec))
  expect_output(prep(nondefault_uni_rec, training = tr_data, verbose = TRUE))
})

