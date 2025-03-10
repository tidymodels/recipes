test_that("can update a step", {
  stp_4 <- recipes::step(
    "stp",
    x = 4,
    trained = FALSE,
    id = "a",
    skip = TRUE,
    role = "a"
  )
  stp_5 <- recipes::step(
    "stp",
    x = 5,
    trained = FALSE,
    id = "a",
    skip = TRUE,
    role = "a"
  )

  expect_equal(update(stp_4, x = 5), stp_5)
})

test_that("cannot create new fields for a step", {
  stp <- recipes::step(
    "stp",
    x = 4,
    trained = FALSE,
    id = "a",
    skip = TRUE,
    role = "a"
  )

  expect_snapshot(
    error = TRUE,
    update(stp, y = 5)
  )
})

test_that("cannot update trained steps", {
  stp <- recipes::step(
    "stp",
    x = 4,
    trained = TRUE,
    id = "a",
    skip = TRUE,
    role = "a"
  )

  expect_snapshot(error = TRUE, update(stp, x = 5))
})

test_that("update() errors on duplicate assignments", {
  step <- recipes::step(
    "stp",
    x = 4,
    trained = TRUE,
    id = "a",
    skip = TRUE,
    role = "a"
  )

  expect_snapshot(
    error = TRUE,
    update(step, x = 5, x = 6)
  )
})
