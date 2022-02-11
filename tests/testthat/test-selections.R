r_version <- function() paste0("R", getRversion()[, 1:2])

data("okc", package = "modeldata")
rec1 <- recipe(~ ., data = okc)
info1 <- summary(rec1)

rec2 <- recipe(age ~ ., data = okc)
info2 <- summary(rec2)

rec3 <- recipe(diet ~ ., data = okc)
info3 <- summary(rec3)

data("biomass", package = "modeldata")
rec4 <- recipe(biomass) %>%
  update_role(carbon, hydrogen, oxygen, nitrogen, sulfur,
              new_role = "predictor") %>%
  update_role(HHV, new_role = "outcome") %>%
  update_role(sample, new_role = "id variable") %>%
  update_role(dataset, new_role = "splitting indicator")
info4 <- summary(rec4)

test_that('simple role selections', {
  expect_equal(
    recipes_eval_select(quos = quos(all_predictors()), data = okc, info = info1),
    setNames(nm = info1$variable)
  )
  expect_equal(
    recipes_eval_select(quos = quos(all_outcomes()), data = okc, info = info1),
    setNames(nm = character())
  )
  expect_equal(
    recipes_eval_select(quos = quos(all_outcomes()), data = biomass, info = info4),
    setNames(nm = "HHV")
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(has_role("splitting indicator")),
      data = biomass,
      info = info4
    ),
    setNames(nm = "dataset")
  )
})

test_that('simple type selections', {
  expect_equal(
    recipes_eval_select(quos = quos(all_numeric()), data = okc, info = info1),
    setNames(nm = c("age", "height"))
  )
  expect_equal(
    recipes_eval_select(quos = quos(has_type("date")), data = okc, info = info1),
    setNames(nm = "date")
  )
  expect_equal(
    recipes_eval_select(quos = quos(all_nominal()), data = okc, info = info1),
    setNames(nm = c("diet", "location", "Class"))
  )
})

test_that('simple name selections', {
  expect_equal(
    recipes_eval_select(quos = quos(matches("e$")), data = okc, info = info1),
    setNames(nm = c("age", "date"))
  )
  expect_equal(
    recipes_eval_select(quos = quos(contains("gen")), data = biomass, info = info4),
    setNames(nm = c("hydrogen", "oxygen", "nitrogen"))
  )
  expect_equal(
    recipes_eval_select(quos = quos(contains("gen"), -nitrogen), data = biomass, info = info4),
    setNames(nm = c("hydrogen", "oxygen"))
  )
  expect_equal(
    recipes_eval_select(quos = quos(date, age), data = okc, info = info1),
    setNames(nm = c("date", "age"))
  )
  expect_equal(
    recipes_eval_select(quos = quos(-age, date), data = okc, info = info1),
    setNames(nm = c("diet", "height", "location", "date", "Class"))
  )
  expect_equal(
    recipes_eval_select(quos = quos(date, -age), data = okc, info = info1),
    setNames(nm = "date")
  )
  expect_equal(
    recipes_eval_select(quos = quos(date:age), data = okc, info = info1),
    setNames(nm = c("date", "location", "height", "diet", "age"))
  )
  expect_equal(
    recipes_eval_select(quos = quos(matches("blahblahblah")), data = okc, info = info1),
    setNames(nm = character())
  )

  expect_snapshot(
    recipes_eval_select(quos = quos(log(date)), data = okc, info = info1),
    error = TRUE
  )
  expect_snapshot(
    recipes_eval_select(quos = quos(I(date:age)), data = okc, info = info1),
    error = TRUE,
    variant = r_version()
  )
  expect_snapshot(
    recipes_eval_select(data = okc, info = info1),
    error = TRUE
  )
})

test_that('combinations', {
  expect_equal(
    recipes_eval_select(
      quos = quos(matches("[hH]"), -all_outcomes()),
      data = biomass,
      info = info4
    ),
    setNames(nm = "hydrogen")
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_numeric(), -all_predictors()),
      data = biomass,
      info = info4
    ),
    setNames(nm = "HHV")
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_numeric(), -all_predictors(), dataset),
      data = biomass,
      info = info4
    ),
    setNames(nm = c("HHV", "dataset"))
  )
  expect_equal(
    recipes_eval_select(
      quos = quos(all_numeric(), -all_predictors(), dataset, -dataset),
      data = biomass,
      info = info4
    ),
    setNames(nm = "HHV")
  )
})

test_that('namespaced selectors', {
  expect_equal(
    recipes_eval_select(quos = quos(tidyselect::matches("e$")), data = okc, info = info1),
    recipes_eval_select(quos = quos(matches("e$")), data = okc, info = info1)
  )
  expect_equal(
    recipes_eval_select(quos = quos(dplyr::matches("e$")), data = okc, info = info1),
    recipes_eval_select(quos(matches("e$")), data = okc, info = info1)
  )
  expect_equal(
    recipes_eval_select(quos = quos(recipes::all_predictors()), data = okc, info = info1),
    recipes_eval_select(quos = quos(all_predictors()), data = okc, info = info1)
  )
})

test_that('new dplyr selectors', {
  vnames <- c("hydrogen", "carbon")
  expect_error(
    rec_1 <-
      recipe(HHV ~ ., data = biomass) %>%
      step_normalize(all_of(c("hydrogen", "carbon"))) %>%
      prep(),
    regex = NA
  )
  expect_equal(names(rec_1$steps[[1]]$means), c("hydrogen", "carbon"))

  expect_error(
    rec_2 <-
      recipe(HHV ~ ., data = biomass) %>%
      step_normalize(all_of(!!vnames)) %>%
      prep(),
    regex = NA
  )
  expect_equal(names(rec_2$steps[[1]]$means), c("hydrogen", "carbon"))

  expect_error(
    rec_3 <-
      recipe(HHV ~ ., data = biomass) %>%
      step_normalize(any_of(c("hydrogen", "carbon"))) %>%
      prep(),
    regex = NA
  )
  expect_equal(names(rec_3$steps[[1]]$means), c("hydrogen", "carbon"))

  expect_error(
    rec_4 <-
      recipe(HHV ~ ., data = biomass) %>%
      step_normalize(any_of(c("hydrogen", "carbon", "bourbon"))) %>%
      prep(),
    regex = NA
  )
  expect_equal(names(rec_4$steps[[1]]$means), c("hydrogen", "carbon"))
})

test_that('predictor specific role selections', {
  expect_equal(
    recipes_eval_select(quos = quos(all_numeric_predictors()), data = okc, info = info2),
    setNames(nm = "height")
  )

  expect_equal(
    recipes_eval_select(quos = quos(all_nominal_predictors()), data = okc, info = info3),
    setNames(nm = c("location", "Class"))
  )
})
