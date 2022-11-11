test_that("recipes_pkg_check() works", {
  expect_snapshot(
    recipes_pkg_check(pkg = "missing_pkg", dependencies = NA)
  )
  expect_snapshot(
    recipes_pkg_check(pkg = c("missing_pkg_1", "missing_pkg_2"))
  )
  expect_snapshot(
    recipes_pkg_check(pkg = NULL)
  )
})
