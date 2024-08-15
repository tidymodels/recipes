test_that("vars without role in predictor/outcome avoid string processing", {
  a <- letters[1:3]
  b <- letters[c(2, 4:6)]
  expect_equal(a %nin% b, !(a %in% b))

  x <- tibble(
    real_pred = 1:5,
    chr_pred_and_lime = letters[1:5],
    chr_outcome = letters[1:5],
    chr_only_lemon = letters[1:5]
  )
  var_info <- tibble(variable = names(x), source = "original")
  var_info <- full_join(get_types(x), var_info, by = "variable")
  var_info$role <- c("predictor", "predictor", "outcome", "lemon")
  additional_row <- var_info[2, ]
  additional_row$role <- "lime"
  var_info <- var_info %>% add_row(additional_row)

  orig_lvls <- lapply(x, get_levels)
  training <- strings2factors(x, orig_lvls)
  original_expectation <- c(FALSE, rep(TRUE, 3))
  names(original_expectation) <- names(x)
  expect_identical(has_lvls(orig_lvls), original_expectation)
  expect_identical(orig_lvls$real_pred, list(values = NA, ordered = NA))
  expect_identical(
    orig_lvls$chr_pred_and_lime,
    list(values = letters[1:5], ordered = FALSE, factor = FALSE)
  )
  expect_identical(
    orig_lvls$chr_outcome,
    list(values = letters[1:5], ordered = FALSE, factor = FALSE)
  )
  expect_identical(
    orig_lvls$chr_only_lemon, # gets converted to fctr
    list(values = letters[1:5], ordered = FALSE, factor = FALSE)
  )

  new_lvls <- kill_levels(orig_lvls, var_info)
  new_expect <- original_expectation
  new_expect[4] <- FALSE
  expect_identical(has_lvls(new_lvls), new_expect)
  expect_identical(new_lvls$real_pred, orig_lvls$real_pred)
  # chr predictor gets converted, despite also having another role
  expect_identical(new_lvls$chr_pred_and_lime, orig_lvls$chr_pred_and_lime)
  expect_identical(new_lvls$chr_outcome, orig_lvls$chr_outcome)
  # non-predictor / non-outcome var remains chr, we don't log the levels
  expect_identical(new_lvls$chr_only_lemon, list(values = NA, ordered = NA))
})
