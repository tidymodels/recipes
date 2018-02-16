library(recipes)
library(testthat)

n <- 200

set.seed(8575)
tr_dat <- data.frame(
  v = sample(letters[1:3], size = n, replace = TRUE),
  w = sample(LETTERS[1:2], size = n, replace = TRUE),
  x = factor(rep_len(month.abb, n)),
  y = factor(rep_len(month.name[-1], n), ordered = TRUE),
  z = factor(rep_len(month.name[-1], n), ordered = TRUE, levels = month.name),  
  stringsAsFactors = FALSE
)

tr_bad <- tr_dat
levels(tr_bad$x) <- c(levels(tr_bad$x), "new")

te_dat <- data.frame(
  v = letters[1:5],
  w = LETTERS[1:5],
  x = factor(month.abb[1:5]),
  y = factor(month.name[1:5], ordered = TRUE),
  z = factor(month.name[1:5], ordered = TRUE, levels = month.name),
  stringsAsFactors = FALSE
)

te_miss <- te_dat
te_miss$y[1] <- NA
te_miss$z[1] <- NA

rec <- recipe(~ ., data = tr_dat)

test_that('basic functionality', {
  ex_1 <- rec %>%
    step_novel(all_predictors()) %>%
    prep(tr_dat, stringsAsFactors = FALSE, retain = TRUE) 
  
  ex_1_tr <- bake(ex_1, newdata = tr_dat)
  ex_1_te <- bake(ex_1, newdata = te_dat)
  
  all(ex_1_te$v[!(ex_1_te$v %in% letters[1:3])] == "new")
  
  
  expect_true(all(vapply(ex_1_tr, is.factor, logical(1))))
  expect_true(all(vapply(ex_1_te, is.factor, logical(1))))
  
  for(i in names(ex_1_tr)) 
    expect_true(
      all.equal(as.character(tr_dat[[i]]), 
                as.character(ex_1_tr[[i]])
      )
    )
  expect_true(
    all(ex_1_te$v[!(ex_1_te$v %in% letters[1:3])] == "new")
  )
  expect_true(
    all(ex_1_te$w[!(ex_1_te$w %in% LETTERS[1:2])] == "new")
  )  
  expect_true(
    all(as.character(te_dat$x) == as.character(ex_1_te$x))
  )    
  expect_true(ex_1_te$y[1] == "new")
  expect_true(
    all(as.character(te_dat$z[-1]) == as.character(ex_1_te$z[-1]))
  )  
  expect_true(
    all(as.character(te_dat$z) == as.character(ex_1_te$z))
  )      
  
  expect_true(is.ordered(ex_1_te$y))
  expect_true(is.ordered(ex_1_te$z))
})

test_that('bad args', {
  expect_error(
    recipe(~., data = iris) %>%
      step_novel(all_predictors()) %>%
      prep(iris)
  )
  expect_error(
    recipe(~., data = tr_bad) %>%
      step_novel(all_predictors()) %>%
      prep(tr_bad)
  )
})

test_that('missing values', {
  ex_2 <- rec %>%
    step_novel(all_predictors()) %>%
    prep(training  = tr_dat)
  ex_2_te <- bake(ex_2, newdata = te_miss)
  expect_equal(which(is.na(te_miss$y)), which(is.na(ex_2_te$y)))
  expect_equal(which(is.na(te_miss$z)), which(is.na(ex_2_te$z)))
})


test_that('printing', {
  ex_3 <- rec %>%
    step_novel(all_predictors()) 
  expect_output(print(ex_3))
  expect_output(print(prep(ex_3, training = tr_dat, verbose = TRUE)))
})


