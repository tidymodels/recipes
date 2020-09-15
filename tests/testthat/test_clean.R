library(testthat)
library(recipes)

context("Clean levels")

library(modeldata)
data("Smithsonian")
smith_tr <- Smithsonian[1:15,]
smith_te <- Smithsonian[16:20,]

rec <- recipe(~ ., data = smith_tr)

test_that('default inputs', {
  cleaned <- rec %>% step_clean_levels(name, id = "")

  tidy_exp_un <- tibble(
    terms = c("name"),
    id = ""
  )
  expect_equal(tidy_exp_un, tidy(cleaned, number = 1))

  cleaned <- prep(cleaned, training = smith_tr[1:2,])
  cleaned_tr <-  bake(cleaned, new_data = NULL)
  cleaned_te <- bake(cleaned, new_data = smith_te)

  expect_equal(sum(grepl(" ", cleaned_tr$name)), 0)
  expect_equal(sum(levels(cleaned_tr$name) %in% smith_tr$name), 0)

  tidy_exp_tr <- tibble(
    terms = rep(c("name"), c(2)),
    original = c("Anacostia Community Museum",
                 "Arthur M. Sackler Gallery"),
    value = c("anacostia_community_museum",
              "arthur_m_sackler_gallery"),
    id = ""
  )
  expect_equal(tidy_exp_tr, tidy(cleaned, number = 1))
  expect_equal(cleaned_tr$name,
               factor(c("anacostia_community_museum",
                        "arthur_m_sackler_gallery")))
  expect_equal(sum(is.na(cleaned_te$name)), 5)

})


test_that('factor inputs', {

  smith_tr <- Smithsonian[1:15,]
  smith_tr$name <- as.factor(smith_tr$name)
  smith_te <- Smithsonian[16:20,]
  smith_te$name <- as.factor(smith_te$name)

  rec <- recipe(~ ., data = smith_tr)

  cleaned <- rec %>% step_clean_levels(name)
  cleaned <- prep(cleaned, training = smith_tr)
  cleaned_tr <- bake(cleaned, new_data = smith_tr)
  cleaned_te <- bake(cleaned, new_data = smith_te)

  expect_equal(sum(grepl(" ", cleaned_tr$name)), 0)
  expect_equal(sum(levels(cleaned_tr$name) %in% smith_tr$name), 0)
  expect_equal(sum(is.na(cleaned_te$name)), 5)
})

test_that('printing', {
  rec <- rec %>% step_clean_levels(name)
  expect_output(print(rec))
  expect_output(prep(rec, training = smith_tr, verbose = TRUE))
})

