library(testthat)
library(recipes)

cmplt_ex_1 <-
  tibble(
    x = letters[1:10],
    y = lapply(as.list(1:10), function(x) 1:100),
    z = lapply(as.list(1:10), function(x) TRUE)
  )

cmplt_ex_2 <- cmplt_ex_1
cmplt_ex_2$x[1] <- NA
cmplt_ex_2$y[[2]][2] <- NA

cmplt_ex_3 <- cmplt_ex_1
cmplt_ex_3$y[[2]] <- NA_real_

test_that("number of complete records", {
  expect_equal(n_complete_rows(cmplt_ex_1), 10)
  expect_equal(n_complete_rows(cmplt_ex_2), 9)
  expect_equal(n_complete_rows(cmplt_ex_3), 9)
})
