test_that("format_ch_vec handles short vectors", {
  expect_equal(format_ch_vec(as.character(1:3)), "1, 2, 3")
})

test_that("format_ch_vec handles long vectors with short items", {
  expect_equal(format_ch_vec(as.character(1:1000)),
               "1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, ...")
})

test_that("format_ch_vec handles a vector with long items", {
  # create string with >100 characters
  long_vec <- ""
  for (i in 1:50){
    long_vec <- paste(long_vec, as.character(i))
  }

  expect_equal(format_ch_vec(c("1", long_vec)), "2 items")
})
