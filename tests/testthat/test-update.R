test_that("can update a step", {

  stp_4 <- recipes::step("stp", x = 4, trained = FALSE)
  stp_5 <- recipes::step("stp", x = 5, trained = FALSE)

  update(stp_4, x = 5)

  expect_equal(update(stp_4, x = 5), stp_5)
})

test_that("cannot create new fields for a step", {

  stp <- recipes::step("stp", x = 4, trained = FALSE)

  expect_error(
    update(stp, y = 5),
    "The step you are trying to update, 'step_stp', does not have the 'y' field."
  )

})

test_that("cannot update trained steps", {

  stp <- recipes::step("stp", x = 4, trained = TRUE)

  expect_error(
    update(stp, x = 5),
    "To update 'step_stp', it must not be trained."
  )

})
