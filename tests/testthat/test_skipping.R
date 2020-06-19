library(recipes)
library(testthat)
library(modeldata)
data(scat)
scat <- na.omit(scat)

context("Skipping steps")

## -----------------------------------------------------------------------------

test_that('simple skip', {
  rec_1 <- recipe(Age ~ Species + Length, data = scat) %>%
    step_log(Age, skip = TRUE) %>%
    step_dummy(Species) %>%
    step_center(all_predictors())

  prepped_1 <- prep(rec_1, training = scat)

  juiced_1 <- juice(prepped_1)
  baked_1  <- bake(prepped_1, new_data = scat)

  expect_equal(baked_1$Age, scat$Age)
  expect_equal(juiced_1$Age, log(scat$Age))

  expect_warning(
    prepped_2 <- prep(rec_1, training = scat, retain = FALSE)
  )

  baked_2  <- bake(prepped_2, new_data = scat[, c("Species", "Length")])
  baked_3  <- bake(prepped_2, new_data = scat)
  expect_false(
    isTRUE(
      all.equal(juiced_1$Age, baked_3$Age)
    )
  )
  expect_equal(log(baked_1$Age), juiced_1$Age)
  expect_equal(setdiff(names(baked_1), names(baked_2)), "Age")
  expect_equal(setdiff(names(baked_2), names(baked_3)), character(0))

  expect_warning(prep(rec_1, training = scat, retain = FALSE))
})


test_that('check existing steps for `skip` arg', {
  step_check <- ls("package:recipes", pattern = "(^step_)|(^check_)")
  # These ones are not operations
  step_check <- step_check[step_check != "check_type"]
  step_check <- step_check[step_check != "check_nominal_type"]
  step_check <- step_check[step_check != "check_name"]
  step_check <- step_check[step_check != "step_type"]
  step_check <- step_check[step_check != "check_training_set"]
  has_skip_arg <- function(x) {
    x_code <- getFromNamespace(x, "recipes")
    x_args <- names(formals(x_code))
    "skip" %in% x_args
  }
  has_skip <- vapply(step_check, has_skip_arg, logical(1))
  if(any(!has_skip))
    print(names(has_skip)[!has_skip])

  for(i in names(has_skip))
    expect_true(has_skip[i])
})


test_that('skips for steps that remove columns (#239)', {
  simple_ex <-
    recipe(Species ~ Age + Mass + Length + Taper,
           data = scat %>% dplyr::select(Species, Age, Mass, Length, Taper)) %>%
    step_interact(terms = ~ Age:Mass) %>%
    step_rm(Age, skip = TRUE)

  prep_simple <- prep(simple_ex, scat)
  simple_juiced <- juice(prep_simple)
  simple_baked <- bake(prep_simple, new_data = scat)
  expect_equal(
    names(simple_juiced),
    c("Mass", "Length", "Taper", "Species", "Age_x_Mass")
  )
  expect_equal(
    names(simple_baked),
    c("Mass", "Length", "Taper", "Species", "Age_x_Mass", "Age")
  )

  complex_ex <-
    recipe(Species ~ ., data = scat %>% dplyr::select(Species, Age, Mass, Length, Taper)) %>%
    step_interact(terms = ~ Age:Mass) %>%
    step_rm(Age) %>%
    step_pca(Taper, Length) %>%
    step_rm(PC1, skip = TRUE) %>%
    prep()

  complex_juiced <- juice(complex_ex)
  complex_baked <- bake(complex_ex, new_data = scat)

  expect_equal(
    names(complex_juiced),
    c("Mass", "Species", "Age_x_Mass", "PC2")
  )
  expect_equal(
    names(complex_baked),
    c("Mass", "Species", "Age_x_Mass", "PC2", "PC1")
  )

  scat_dups <-
    scat %>%
    dplyr::select(Species, Age, Mass, Length) %>%
    mutate(
      dup_1 = Mass,
      dup_2 = Mass
    )

  corr_example <-
    recipe(Species ~ Age + Length + Mass + dup_1 + dup_2, data = scat_dups) %>%
    step_corr(all_predictors(), skip = TRUE) %>%
    prep()

  corr_juiced <- juice(corr_example)
  corr_baked <- bake(corr_example, new_data = scat_dups)

  expect_equal(
    names(corr_juiced),
    c('Age', 'Length', 'dup_2', 'Species')
  )

  expect_equal(
    sort(names(corr_baked)),
    sort(names(scat_dups))
  )
})

