test_that("recipes_extension_check", {
  expect_snapshot(
    recipes_extension_check(
      pkg = "recipes",
      exclude_steps = "step_testthat_helper",
      exclude_methods = c("required_pkgs")
    )
  )
})
