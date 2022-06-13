test_that("can update a step", {
  stp_4 <- recipes::step("stp", x = 4, trained = FALSE)
  stp_5 <- recipes::step("stp", x = 5, trained = FALSE)

  update(stp_4, x = 5)

  expect_equal(update(stp_4, x = 5), stp_5)
})

test_that("cannot create new fields for a step", {
  stp <- recipes::step("stp", x = 4, trained = FALSE)

  expect_snapshot(error = TRUE,
    update(stp, y = 5)
  )
})

test_that("cannot update trained steps", {
  stp <- recipes::step("stp", x = 4, trained = TRUE)

  expect_snapshot(error = TRUE,
    update(stp, x = 5)
  )
})
