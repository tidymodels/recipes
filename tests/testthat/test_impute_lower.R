library(testthat)
library(recipes)
library(dplyr)

library(modeldata)
data(biomass)

biomass$carbon <- ifelse(biomass$carbon > 40, biomass$carbon, 40)
biomass$hydrogen <- ifelse(biomass$hydrogen > 5, biomass$carbon, 5)
biomass$has_neg <- runif(nrow(biomass), min = -2)

rec <- recipe(HHV ~ carbon + hydrogen + has_neg,
              data = biomass)

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]


test_that('basic usage', {
  rec1 <- rec %>%
    step_impute_lower(carbon, hydrogen, id = "")

  untrained <- tibble(
    terms = c("carbon", "hydrogen"),
    value = rep(NA_real_, 2),
    id = ""
  )

  expect_equal(untrained, tidy(rec1, number = 1))

  rec1 <- prep(rec1, biomass_tr)

  trained <- tibble(
    terms = c("carbon", "hydrogen"),
    value = c(40, 5),
    id = ""
  )

  expect_equal(trained, tidy(rec1, number = 1))

  expect_equal(c(carbon = 40, hydrogen = 5),
               rec1$steps[[1]]$threshold)

  processed <- juice(rec1)
  for(i in names(rec1$steps[[1]]$threshold)) {
    affected <- biomass_tr[[i]] <= rec1$steps[[1]]$threshold[[i]]
    is_less <- processed[affected, i] < biomass_tr[affected, i]
    is_pos <- processed[affected, i] > 0
    expect_true(all(is_less))
    expect_true(all(is_pos))
  }

})

test_that('bad data', {
  expect_error(
    rec %>%
      step_impute_lower(carbon, hydrogen, has_neg) %>%
      prep
  )
})

test_that('printing', {
  rec2 <- rec %>%
    step_impute_lower(carbon, hydrogen)

  expect_output(print(rec))
  expect_output(prep(rec2, training = biomass_tr, verbose = TRUE))
})

