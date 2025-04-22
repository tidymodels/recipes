library(testthat)
library(recipes)

skip_if_not_installed("modeldata")
data(biomass, package = "modeldata")

rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass
)

test_that("training in stages", {
  whole_recipe <- rec |>
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
    step_rm(sulfur) |>
    step_scale(carbon, hydrogen, oxygen, nitrogen)

  at_same_time <- prep(whole_recipe, training = biomass)

  ## not train in stages
  center_first <- rec |>
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur)
  center_first_trained <-
    prep(center_first, training = biomass)

  no_sulfur <- center_first_trained |>
    step_rm(sulfur)

  expect_snapshot(
    no_sulfur_trained <- prep(no_sulfur)
  )

  scale_last <- no_sulfur_trained |>
    step_scale(carbon, hydrogen, oxygen, nitrogen)
  expect_snapshot(
    sequentially <- prep(scale_last)
  )

  in_stages <- center_first_trained |>
    step_rm(sulfur) |>
    step_scale(carbon, hydrogen, oxygen, nitrogen)
  expect_snapshot(
    in_stages_trained <- prep(in_stages)
  )
  in_stages_retrained <-
    prep(in_stages, training = biomass, fresh = TRUE)

  # check baked values

  expect_equal(
    bake(at_same_time, head(biomass)),
    bake(sequentially, head(biomass))
  )
  expect_equal(
    bake(at_same_time, head(biomass)),
    bake(in_stages_trained, head(biomass))
  )
  expect_equal(
    bake(at_same_time, head(biomass)),
    bake(in_stages_retrained, head(biomass))
  )

  # variable lists
  expect_equal(
    summary(at_same_time),
    summary(sequentially)
  )
  expect_equal(
    summary(at_same_time),
    summary(in_stages_trained)
  )
  expect_equal(
    summary(at_same_time),
    summary(in_stages_retrained)
  )

  expect_snapshot(
    rec |>
      step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) |>
      prep(training = biomass) |>
      step_rm(sulfur) |>
      prep(training = biomass)
  )
})
