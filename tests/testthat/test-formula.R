library(testthat)
library(recipes)

test_that("is trained?", {
  rec1 <- recipe(~., data = iris)
  expect_false(fully_trained(rec1))
  expect_true(fully_trained(rec1 |> prep()))

  rec2 <- rec1 |>
    step_sqrt(all_numeric()) |>
    step_center(all_numeric())
  expect_false(fully_trained(rec2))

  rec3 <- prep(rec2, training = iris)
  expect_true(fully_trained(rec3))

  rec4 <- rec3 |> step_scale(all_numeric())
  expect_false(fully_trained(rec4))

  expect_snapshot(
    rec5 <- prep(rec4, training = iris)
  )
  expect_true(fully_trained(rec5))
})

test_that("formulas", {
  rec6 <- recipe(Species ~ ., data = iris) |> prep(iris)
  expect_equal(
    formula(rec6),
    as.formula(
      Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
    ),
    ignore_formula_env = TRUE
  )

  rec7 <- rec6 |>
    step_rm(starts_with("Sepal")) |>
    prep(iris)
  expect_equal(
    formula(rec7),
    as.formula(Species ~ Petal.Length + Petal.Width),
    ignore_formula_env = TRUE
  )

  rec8 <- recipe(~., data = iris) |> prep(iris)
  expect_equal(
    formula(rec8),
    as.formula(
      ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width + Species
    ),
    ignore_formula_env = TRUE
  )

  rec9 <- recipe(Species + Sepal.Length ~ ., data = iris) |> prep(iris)
  expect_equal(
    formula(rec9),
    as.formula(
      Species + Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
    ),
    ignore_formula_env = TRUE
  )

  rec10 <- recipe(mtcars) |> prep()
  expect_equal(
    formula(rec10),
    as.formula(
      ~1
    ),
    ignore_formula_env = TRUE
  )

  rec11 <- recipe(mtcars) |>
    update_role(mpg, new_role = "outcome") |>
    prep()
  expect_equal(
    formula(rec11),
    as.formula(
      mpg ~ 1
    ),
    ignore_formula_env = TRUE
  )
  rec12 <- recipe(mtcars) |>
    update_role(-mpg, new_role = "predictor") |>
    prep()
  expect_equal(
    formula(rec12),
    as.formula(
      ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb
    ),
    ignore_formula_env = TRUE
  )
  rec13 <- recipe(mtcars) |>
    update_role(mpg, new_role = "outcome") |>
    update_role(-mpg, new_role = "predictor") |>
    prep()
  expect_equal(
    formula(rec13),
    as.formula(
      mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb
    ),
    ignore_formula_env = TRUE
  )
})

test_that("bad args", {
  rec10 <- recipe(Species ~ ., data = iris) |>
    step_center(all_numeric())
  expect_snapshot(error = TRUE, formula(rec10))
})
