
sparse_x <- Matrix::sparseMatrix(
  i = 1:4, j = 1:4,
  dimnames = list(NULL, c("x_a", "x_b", "x_c", "x_d")),
  x = 1)

sparse_y <- Matrix::sparseMatrix(
  i = 1:4, j = 4:1,
  dimnames = list(NULL, c("y_a", "y_b", "y_c", "y_d")),
  x = 2)

test_that("RsparseList works as expected", {

  expect_length(CsparseMatrix_to_RsparseList(sparse_x), 4)
  expect_true(attr(CsparseMatrix_to_RsparseList(sparse_x), "RsparseList"))

  expect_true(is_vector(CsparseMatrix_to_RsparseList(sparse_x)))
  expect_true(is_RsparseList(CsparseMatrix_to_RsparseList(sparse_x)))
  expect_false(is_RsparseList(sparse_x))

  expect_equal(sparse_x, RsparseList_to_CsparseMatrix(CsparseMatrix_to_RsparseList(sparse_x)))
})

ex_dat <- tibble(x = CsparseMatrix_to_RsparseList(sparse_x),
                 y = CsparseMatrix_to_RsparseList(sparse_y),
                 z = 1:4,
                 w = 1:4)

res_matrix <- matrix(c(1, 1, 1, 0, 0, 0, 0, 0, 0, 2,
                       2, 2, 0, 1, 0, 0, 0, 0, 2, 0,
                       3, 3, 0, 0, 1, 0, 0, 2, 0, 0,
                       4, 4, 0, 0, 0, 1, 2, 0, 0, 0),
                     nrow = 4, ncol = 10, byrow = TRUE,
                     dimnames = list(NULL, c("z", "w", "x_a", "x_b", "x_c", "x_d", "y_a", "y_b", "y_c", "y_d")))

test_that("convert_matrix works as expected", {
  expect_equal(
    convert_matrix(ex_dat, sparse = FALSE),
    res_matrix
  )

  expect_equal(
    as(res_matrix, "CsparseMatrix"),
    convert_matrix(ex_dat, sparse = TRUE)
  )
})

test_that("juice composition works with sparse columns", {
  recipe(~., data = tibble(z = 1:4,
                           w = 1:4)) %>%
    step_mutate(x = CsparseMatrix_to_RsparseList(sparse_x),
                y = CsparseMatrix_to_RsparseList(sparse_y)) %>%
    prep() %>%
    juice(composition = "tibble")

  recipe(~., data = tibble(z = 1:4,
                           w = 1:4)) %>%
    step_mutate(x = CsparseMatrix_to_RsparseList(sparse_x),
                y = CsparseMatrix_to_RsparseList(sparse_y)) %>%
    prep() %>%
    juice(composition = "data.frame")



  expect_equal(
    recipe(~., data = tibble(z = 1:4,
                             w = 1:4)) %>%
      step_mutate(x = CsparseMatrix_to_RsparseList(sparse_x),
                  y = CsparseMatrix_to_RsparseList(sparse_y)) %>%
      prep() %>%
      juice(composition = "matrix"),
    res_matrix
  )

  expect_equal(
    recipe(~., data = tibble(z = 1:4,
                             w = 1:4)) %>%
      step_mutate(x = CsparseMatrix_to_RsparseList(sparse_x),
                  y = CsparseMatrix_to_RsparseList(sparse_y)) %>%
      prep() %>%
      juice(composition = "dgCMatrix"),
  as(res_matrix, "CsparseMatrix")
  )

})
