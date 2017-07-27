library(testthat)
library(recipes)
library(tibble)

n <- 20
ex_dat <- data.frame(x1 = seq(0, 1, length = n),
                     x2 = seq(1, 0, length = n))

get_exp <- function(x, f) 
  as_tibble(lapply(x, f))


test_that('simple hyperbolic trans', {
  
  for(func in c("sin", "cos", "tan")) {
    for(invf in c(TRUE, FALSE)) {
      rec <- recipe(~., data = ex_dat) %>% 
        step_hyperbolic(x1, x2, func = func, inverse = invf)
      
      rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
      rec_trans <- bake(rec_trained, newdata = ex_dat)
      
      if(invf) {
        foo <- get(paste0("a", func))
      } else {
        foo <- get(func)
      }
      
      exp_res <- get_exp(ex_dat, foo)
      
      expect_equal(rec_trans, exp_res)
    }
  }
  
})


test_that('printing', {
  rec <- recipe(~., data = ex_dat) %>% 
    step_hyperbolic(x1, x2, func = "sin", inverse = TRUE)
  expect_output(print(rec))
  expect_output(prep(rec, training = ex_dat))
})


