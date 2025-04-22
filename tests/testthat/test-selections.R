r_version <- function() paste0("R", getRversion()[, 1:2])

skip_if_not_installed("modeldata")

data("Sacramento", package = "modeldata")
Sacramento$lgl1 <- Sacramento$beds >= 1
Sacramento$lgl2 <- Sacramento$beds >= 2
Sacramento$ord1 <- as.ordered(Sacramento$type)
Sacramento$ord2 <- as.ordered(Sacramento$type)
Sacramento$date1 <- as.Date(Sacramento$sqft, "2000-01-01")
Sacramento$date2 <- as.Date(Sacramento$sqft, "2000-01-01")
Sacramento$datetime1 <- as.POSIXct(
  Sacramento$sqft,
  origin = "2000-01-01 00:00:00"
)
Sacramento$datetime2 <- as.POSIXct(
  Sacramento$sqft,
  origin = "2000-01-01 00:00:00"
)

rec_sac <- recipe(~., data = Sacramento)
info_sac <- summary(rec_sac)

data("biomass", package = "modeldata")
rec_bio <- recipe(biomass) |>
  update_role(
    carbon,
    hydrogen,
    oxygen,
    nitrogen,
    sulfur,
    new_role = "predictor"
  ) |>
  update_role(HHV, new_role = "outcome") |>
  update_role(sample, new_role = "id variable") |>
  update_role(dataset, new_role = "splitting indicator")
info_bio <- summary(rec_bio)

test_that("simple role selections", {
  expect_equal(
    recipes_eval_select(
      quos = quos(all_predictors()),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = info_sac$variable)
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_outcomes()),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = character())
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_outcomes()),
      data = biomass,
      info = info_bio
    ),
    setNames(nm = "HHV")
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(has_role("splitting indicator")),
      data = biomass,
      info = info_bio
    ),
    setNames(nm = "dataset")
  )
})

test_that("simple type selections", {
  expect_equal(
    recipes_eval_select(
      quos = quos(all_numeric()),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("beds", "baths", "sqft", "price", "latitude", "longitude"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(has_type("numeric")),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("beds", "baths", "sqft", "price", "latitude", "longitude"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_nominal()),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("city", "zip", "type", "ord1", "ord2"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_logical()),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("lgl1", "lgl2"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_ordered()),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("ord1", "ord2"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_integer()),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("beds", "sqft", "price"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_double()),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("baths", "latitude", "longitude"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_unordered()),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("city", "zip", "type"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_date()),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("date1", "date2"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_datetime()),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("datetime1", "datetime2"))
  )
})

test_that("simple name selections", {
  expect_equal(
    recipes_eval_select(
      quos = quos(matches("s$")),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("beds", "baths"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(contains("gen")),
      data = biomass,
      info = info_bio
    ),
    setNames(nm = c("hydrogen", "oxygen", "nitrogen"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(contains("gen"), -nitrogen),
      data = biomass,
      info = info_bio
    ),
    setNames(nm = c("hydrogen", "oxygen"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(beds, sqft),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("beds", "sqft"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(-sqft, beds),
      data = Sacramento,
      info = info_sac
    ),
    setNames(
      nm = c(
        "city",
        "zip",
        "beds",
        "baths",
        "type",
        "price",
        "latitude",
        "longitude",
        "lgl1",
        "lgl2",
        "ord1",
        "ord2",
        "date1",
        "date2",
        "datetime1",
        "datetime2"
      )
    )
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(beds, -sqft),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = "beds")
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(beds:sqft),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = c("beds", "baths", "sqft"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(matches("blahblahblah")),
      data = Sacramento,
      info = info_sac
    ),
    setNames(nm = character())
  )

  expect_snapshot(
    recipes_eval_select(
      quos = quos(log(beds)),
      data = Sacramento,
      info = info_sac
    ),
    error = TRUE
  )
  expect_snapshot(
    recipes_eval_select(
      quos = quos(I(beds:sqft)),
      data = Sacramento,
      info = info_sac
    ),
    error = TRUE,
    variant = r_version()
  )
  expect_snapshot(
    recipes_eval_select(data = Sacramento, info = info_sac),
    error = TRUE
  )
})

test_that("combinations", {
  expect_equal(
    recipes_eval_select(
      quos = quos(matches("[hH]"), -all_outcomes()),
      data = biomass,
      info = info_bio
    ),
    setNames(nm = "hydrogen")
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_numeric(), -all_predictors()),
      data = biomass,
      info = info_bio
    ),
    setNames(nm = "HHV")
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_numeric(), -all_predictors(), dataset),
      data = biomass,
      info = info_bio
    ),
    setNames(nm = c("HHV", "dataset"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_numeric(), -all_predictors(), dataset, -dataset),
      data = biomass,
      info = info_bio
    ),
    setNames(nm = "HHV")
  )
})

test_that("namespaced selectors", {
  expect_equal(
    recipes_eval_select(
      quos = quos(tidyselect::matches("e$")),
      data = Sacramento,
      info = info_sac
    ),
    recipes_eval_select(
      quos = quos(matches("e$")),
      data = Sacramento,
      info = info_sac
    )
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(dplyr::matches("e$")),
      data = Sacramento,
      info = info_sac
    ),
    recipes_eval_select(quos(matches("e$")), data = Sacramento, info = info_sac)
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(recipes::all_predictors()),
      data = Sacramento,
      info = info_sac
    ),
    recipes_eval_select(
      quos = quos(all_predictors()),
      data = Sacramento,
      info = info_sac
    )
  )
})

test_that("new dplyr selectors", {
  vnames <- c("hydrogen", "carbon")
  expect_no_error(
    rec_1 <-
      recipe(HHV ~ ., data = biomass) |>
      step_normalize(all_of(c("hydrogen", "carbon"))) |>
      prep()
  )
  expect_equal(names(rec_1$steps[[1]]$means), c("hydrogen", "carbon"))

  expect_no_error(
    rec_2 <-
      recipe(HHV ~ ., data = biomass) |>
      step_normalize(all_of(!!vnames)) |>
      prep()
  )
  expect_equal(names(rec_2$steps[[1]]$means), c("hydrogen", "carbon"))

  expect_no_error(
    rec_3 <-
      recipe(HHV ~ ., data = biomass) |>
      step_normalize(any_of(c("hydrogen", "carbon"))) |>
      prep()
  )
  expect_equal(names(rec_3$steps[[1]]$means), c("hydrogen", "carbon"))

  expect_no_error(
    rec_4 <-
      recipe(HHV ~ ., data = biomass) |>
      step_normalize(any_of(c("hydrogen", "carbon", "bourbon"))) |>
      prep()
  )
  expect_equal(names(rec_4$steps[[1]]$means), c("hydrogen", "carbon"))
})

test_that("predictor specific role selections", {
  rec <- recipe(sqft ~ ., data = Sacramento)
  info <- summary(rec)
  expect_equal(
    recipes_eval_select(
      quos = quos(all_numeric_predictors()),
      data = Sacramento,
      info = info
    ),
    setNames(nm = c("beds", "baths", "price", "latitude", "longitude"))
  )

  rec <- recipe(city ~ ., data = Sacramento)
  info <- summary(rec)
  expect_equal(
    recipes_eval_select(
      quos = quos(all_nominal_predictors()),
      data = Sacramento,
      info = info
    ),
    setNames(nm = c("zip", "type", "ord1", "ord2"))
  )

  rec <- recipe(lgl1 ~ ., data = Sacramento)
  info <- summary(rec)
  expect_equal(
    recipes_eval_select(
      quos = quos(all_logical_predictors()),
      data = Sacramento,
      info = info
    ),
    setNames(nm = c("lgl2"))
  )

  rec <- recipe(ord1 ~ ., data = Sacramento)
  info <- summary(rec)
  expect_equal(
    recipes_eval_select(
      quos = quos(all_ordered_predictors()),
      data = Sacramento,
      info = info
    ),
    setNames(nm = c("ord2"))
  )

  rec <- recipe(price ~ ., data = Sacramento)
  info <- summary(rec)
  expect_equal(
    recipes_eval_select(
      quos = quos(all_integer_predictors()),
      data = Sacramento,
      info = info
    ),
    setNames(nm = c("beds", "sqft"))
  )

  rec <- recipe(baths ~ ., data = Sacramento)
  info <- summary(rec)
  expect_equal(
    recipes_eval_select(
      quos = quos(all_double_predictors()),
      data = Sacramento,
      info = info
    ),
    setNames(nm = c("latitude", "longitude"))
  )

  rec <- recipe(city ~ ., data = Sacramento)
  info <- summary(rec)
  expect_equal(
    recipes_eval_select(
      quos = quos(all_unordered_predictors()),
      data = Sacramento,
      info = info
    ),
    setNames(nm = c("zip", "type"))
  )

  rec <- recipe(date1 ~ ., data = Sacramento)
  info <- summary(rec)
  expect_equal(
    recipes_eval_select(
      quos = quos(all_date_predictors()),
      data = Sacramento,
      info = info
    ),
    setNames(nm = c("date2"))
  )

  rec <- recipe(datetime1 ~ ., data = Sacramento)
  info <- summary(rec)
  expect_equal(
    recipes_eval_select(
      quos = quos(all_datetime_predictors()),
      data = Sacramento,
      info = info
    ),
    setNames(nm = c("datetime2"))
  )
})

test_that("old recipes from 1.0.1 work with new get_types", {
  load(test_path("old-get_types.RData"))

  expect_identical(
    lapply(summary(old_rec_sac), class),
    list(
      variable = "character",
      type = "character",
      role = "character",
      source = 'character'
    )
  )

  expect_false(identical(old_rec_sac, rec_sac))

  # Avoid issue with new ptype field in 1.1.0
  rec_sac$ptype <- NULL

  # Avoid issue with new strings_as_factors field in 1.3.0
  rec_sac$strings_as_factors <- NULL

  expect_identical(
    prep(old_rec_sac),
    prep(rec_sac)
  )

  expect_equal(
    old_pca_rec_sac |>
      bake(new_data = Sacramento),
    rec_sac |>
      step_normalize(all_numeric_predictors()) |>
      step_pca(beds, baths, sqft) |>
      prep() |>
      bake(new_data = Sacramento)
  )
})

test_that("error when selecting case weights", {
  mtcars$hp <- hardhat::importance_weights(mtcars$hp)

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_normalize(hp) |>
      prep()
  )
})
