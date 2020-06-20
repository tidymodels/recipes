max_width <- options()$width - 9

test_that("format_ch_vec handles short vectors", {
  expect_equal(format_ch_vec(as.character(1:3)), "1, 2, 3")
})

test_that("format_ch_vec handles long vectors with short items", {
  # should fit the screen
  expect_true(length(format_ch_vec(as.character(1:1000))) <= max_width)
  #  should list as many items as possible, not just return "n items"
  expect_true(format_ch_vec(as.character(1:1000)) != "1000 items")

})

test_that("format_ch_vec handles a vector with long items", {
  # create string with >100 characters
  long_vec <- ""
  for (i in 1:50){
    long_vec <- paste(long_vec, as.character(i))
  }

  expect_equal(format_ch_vec(c("1", long_vec)), "2 items")
})

test_that("format_selectors handles small numbers of selectors", {
  test_exprs_small <- lapply(1:6, quo)
  expect_equal(format_selectors(test_exprs_small), "1L, 2L, 3L, 4L, 5L, 6L")
})

test_that("format_ch_vec handles lots of small selectors", {
  test_exprs_small_many <- lapply(1:100, quo)
  # should fit the screen
  expect_true(length(format_selectors(test_exprs_small_many)) <=  max_width)
  #  should list as many items as possible, not just return "n items"
  expect_true(format_selectors(test_exprs_small_many) != "1000 items")
})

test_that("format_ch_vec handles a long expression", {
  # create string with >100 characters
  long_vec <- ""
  for (i in 1:50){
    long_vec <- paste(long_vec, as.character(i))
  }

  expect_equal(format_ch_vec(c(expr(1), expr(!!long_vec))), "2 items")
})
