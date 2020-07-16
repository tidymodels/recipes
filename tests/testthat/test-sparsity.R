
create_diag_matrix <- function(x, value, prefix) {
  Matrix::sparseMatrix(
    i = x, j = x,
    dimnames = list(NULL, paste0(prefix, seq_along(x))),
    x = value)
}

sparse_v <- create_diag_matrix(1:4, 1, "v_")

sparse_x <- create_diag_matrix(1:4, 2, "x_")

sparse_z <- create_diag_matrix(1:4, 3, "z_")

ex_dat <- tibble(
  v = CsparseMatrix_to_RsparseList(sparse_v),
  w = 1:4,
  x = CsparseMatrix_to_RsparseList(sparse_x),
  y = 1:4,
  z = CsparseMatrix_to_RsparseList(sparse_z)
  )

ref_df <- data.frame(v_1 = c(1, 0, 0, 0),
                     v_2 = c(0, 1, 0, 0),
                     v_3 = c(0, 0, 1, 0),
                     v_4 = c(0, 0, 0, 1),
                     w = 1:4,
                     x_1 = c(2, 0, 0, 0),
                     x_2 = c(0, 2, 0, 0),
                     x_3 = c(0, 0, 2, 0),
                     x_4 = c(0, 0, 0, 2),
                     y = 1:4,
                     z_1 = c(3, 0, 0, 0),
                     z_2 = c(0, 3, 0, 0),
                     z_3 = c(0, 0, 3, 0),
                     z_4 = c(0, 0, 0, 3))

test_that("RsparseList works as expected", {

  expect_length(CsparseMatrix_to_RsparseList(sparse_x), 4)
  expect_true(attr(CsparseMatrix_to_RsparseList(sparse_x), "RsparseList"))

  expect_true(is_vector(CsparseMatrix_to_RsparseList(sparse_x)))
  expect_true(is_RsparseList(CsparseMatrix_to_RsparseList(sparse_x)))
  expect_false(is_RsparseList(sparse_x))

  expect_equal(sparse_x, RsparseList_to_CsparseMatrix(CsparseMatrix_to_RsparseList(sparse_x)))
})

test_that("convert_matrix works as expected", {
  expect_equal(
    convert_matrix(ex_dat, sparse = FALSE),
    as.matrix(ref_df)
  )

  expect_equal(
    convert_matrix(ex_dat, sparse = TRUE),
    as( as.matrix(ref_df), "CsparseMatrix")
  )
})

test_that("juice composition works with sparse columns", {

  ex_rec <- recipe(~., data = tibble(temp = 1:4)) %>%
    step_mutate(v = CsparseMatrix_to_RsparseList(create_diag_matrix(temp, 1, "v_")),
                w = temp,
                x = CsparseMatrix_to_RsparseList(create_diag_matrix(temp, 2, "x_")),
                y = temp,
                z = CsparseMatrix_to_RsparseList(create_diag_matrix(temp, 3, "z_"))) %>%
    prep()

  expect_equal(
    ex_rec %>%
      juice(composition = "tibble") %>%
      select(-temp),
    as_tibble(ref_df)
  )

  expect_equal(
    ex_rec %>%
      juice(composition = "data.frame") %>%
      select(-temp),
    ref_df
  )

  expect_equal(
    ex_rec %>%
      juice(composition = "matrix") %>%
      .[, -1],
    as.matrix(ref_df)
  )

  expect_equal(
    ex_rec %>%
        juice(composition = "dgCMatrix") %>%
      .[, -1],
    as(as.matrix(ref_df), "CsparseMatrix")
  )
})
