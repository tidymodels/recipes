library(recipes)
library(testthat)

library(modeldata)
data(okc)

okc_tr <- okc[ (1:30000), ]
okc_te <- okc[-(1:30000), ]

rec <- recipe(~., data = okc_tr)

test_that("basic functionality", {
  rec_1 <- rec %>%
    step_relevel(location, ref_level = "oakland") %>%
    prep()

  tr_1 <- juice(rec_1)
  expect_equal(levels(tr_1$location)[[1]], "oakland")

  te_1 <- bake(rec_1, okc_te)
  expect_equal(levels(te_1$location)[[1]], "oakland")
})

test_that("bad args", {
  expect_error(
    rec %>%
      step_relevel(age, ref_level = 23) %>%
      prep()
  )
  expect_error(
    rec %>%
      step_relevel(diet, ref_level = "missing_level") %>%
      prep()
  )
})

test_that("printing", {
  expect_output(print(rec %>% step_relevel(location, ref_level = "oakland")))
  expect_output(print(rec %>% step_relevel(location, ref_level = "oakland") %>% prep()))
})


test_that("tidy methods", {
  rec_raw <- rec %>% step_relevel(location, ref_level = "oakland", id = "city")
  expect_equal(
    tidy(rec_raw, 1),
    tibble(terms = "location", value = "oakland", id = "city")
  )
  expect_equal(
    tidy(prep(rec_raw), 1),
    tibble(terms = "location", value = "oakland", id = "city")
  )
})
