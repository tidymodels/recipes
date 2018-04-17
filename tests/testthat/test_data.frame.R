library(testthat)
library(recipes)

###################################################################

data(okc)

okc$diet <- as.factor(okc$diet)
okc$date <- as.Date(okc$date)
okc$location <- as.factor(okc$location)

okc_tr <- okc[1:400, ]
okc_te <- okc[(401:800), ]

###################################################################

rec <- recipe( ~ ., data = okc_tr) %>%
  step_modeimpute(all_nominal()) %>%
  step_meanimpute(all_numeric()) %>%
  step_dummy(location, diet) %>%
  prep(training = okc_tr, retain = TRUE)

###################################################################

test_that('correct types', {
  bake_default <- bake(rec, newdata = okc_te, all_numeric())
  bake_df <-
    bake(rec,
         newdata = okc_te,
         all_numeric(),
         composition = "data.frame")
  bake_df_1d <-
    bake(rec,
         newdata = okc_te,
         age,
         composition = "data.frame")
  juice_default <- juice(rec, all_numeric())
  juice_df <-
    juice(rec, all_numeric(), composition = "data.frame")
  juice_df_1d <-
    juice(rec, age, composition = "data.frame")

  expect_equal(class(bake_default), class(tibble()))
  expect_equal(class(juice_default), class(tibble()))

  expect_equal(as.vector(class(bake_df)), "data.frame")
  expect_equal(as.vector(class(juice_df)), "data.frame")

  expect_equal(as.vector(class(bake_df_1d)), "data.frame")
  expect_equal(as.vector(class(juice_df_1d)), "data.frame")

})
