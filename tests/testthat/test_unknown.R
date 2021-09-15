library(recipes)
library(testthat)


library(modeldata)
data(okc)

okc_tr <- okc[ (1:30000), ]
okc_te <- okc[-(1:30000), ]

rec <- recipe(~ ., data = okc_tr)

test_that('basic functionality', {
  rec_1 <- rec %>% step_unknown(diet, location) %>% prep()

  tr_1 <- juice(rec_1)
  tr_diet <- tr_1$diet[is.na(okc_tr$diet)]
  tr_diet <- unique(as.character(tr_diet))
  expect_true(all(tr_diet == "unknown"))
  diet_lvl <- c(sort(unique(okc_tr$diet)), "unknown")
  expect_equal(diet_lvl, levels(tr_1$diet))

  tr_loc <- tr_1$diet[is.na(okc_tr$location)]
  tr_loc <- unique(as.character(tr_loc))
  expect_true(all(tr_loc == "unknown"))
  expect_equal(diet_lvl, levels(tr_1$diet))
  loc_lvl <- c(sort(unique(okc_tr$location)), "unknown")
  expect_equal(loc_lvl, levels(tr_1$location))


  expect_warning(
    te_1 <- bake(rec_1, okc_te),
    "There are new levels in a factor: port costa"
  )
  te_diet <- te_1$diet[is.na(okc_te$diet)]
  te_diet <- unique(as.character(te_diet))
  expect_true(all(te_diet == "unknown"))
  expect_equal(diet_lvl, levels(te_1$diet))

  te_loc <- tr_1$diet[is.na(okc_te$location)]
  te_loc <- unique(as.character(te_loc))
  expect_true(all(te_loc == "unknown"))
  expect_equal(loc_lvl, levels(te_1$location))

  rec_2 <- rec %>% step_unknown(diet, new_level = "potato-based") %>% prep()
  tr_2 <- juice(rec_2)
  tr_diet <- tr_2$diet[is.na(okc_tr$diet)]
  tr_diet <- unique(as.character(tr_diet))
  expect_true(all(tr_diet == "potato-based"))
  diet_lvl <- c(sort(unique(okc_tr$diet)), "potato-based")
  expect_equal(diet_lvl, levels(tr_2$diet))

})

test_that('bad args', {
  expect_error(
    recipe(~., data = okc_tr) %>%
      step_unknown(age) %>%
      prep()
  )
  expect_error(
    recipe(~., data = okc_tr) %>%
      step_unknown(diet, new_level = "anything") %>%
      prep()
  )
})


test_that('printing', {
  expect_output(print(rec %>% step_unknown(diet, location)))
  expect_output(print(rec %>% step_unknown(diet, location) %>% prep()))
})

test_that('tidy methods', {
  rec_raw <- rec %>% step_unknown(all_nominal(), new_level = "cake", id = "cheese")

  expect_equal(
    tidy(rec_raw, 1),
    tibble(terms = "all_nominal()", value = "cake", id = "cheese")
  )
  expect_equal(
    tidy(prep(rec_raw), 1),
    tibble(terms = c("diet", "location", "Class"), value = "cake", id = "cheese")
  )
})


