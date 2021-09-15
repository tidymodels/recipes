library(dplyr)

test_that("step_cut throws error on non-numerics", {
  x <- tibble(num_var = 1:3, cat_var = c("1", "2" ,"3"))
  expect_error(recipe(x) %>% step_cut(num_var, breaks = 2) %>% prep(), NA)
  expect_error(recipe(x) %>% step_cut(cat_var, breaks = 2) %>% prep(),
               "All columns selected for the step should be numeric")
  expect_error(recipe(x) %>% step_cut(everything(), breaks = 2) %>% prep(),
               "All columns selected for the step should be numeric")
})

test_that("create_full_breaks helper function", {
  var1 <- 1:100; var2 <- 75:150; var3 <- 1:25; breaks1 <- 50
  breaks2 <- c(50, 70); var4 <- 50:71; var5 <- 49:70
  expect_equal(create_full_breaks(var1, breaks1), c(1, 50, 100))
  expect_equal(create_full_breaks(var1, breaks2), c(1, 50, 70, 100))
  expect_equal(create_full_breaks(var2, breaks1), c(50, 150))
  expect_equal(create_full_breaks(var2, breaks2), c(50, 70, 150))
  expect_equal(create_full_breaks(var3, breaks1), c(1, 50))
  expect_equal(create_full_breaks(var3, breaks2), c(1, 50, 70))
  expect_equal(create_full_breaks(var4, breaks2), c(50, 70, 71))
  expect_equal(create_full_breaks(var5, breaks2), c(49, 50, 70))
})

test_that("full_breaks_check will give warnings", {
  expect_error(full_breaks_check(10),
               "In step_cut: variable is invariant and equal to break point")
  expect_warning(full_breaks_check(c(10, 20)),
                "In step_cut: this will create a factor with one value only")
  expect_error(full_breaks_check(c(10, 20, 30)), NA)
  expect_warning(full_breaks_check(c(10, 20, 30)), NA)
})

test_that("cut_var gives correct output", {
  var1    <- 1:10
  var2    <- 2:11
  breaks  <- c(1, 5, 10)

  expect_equal(cut_var(var1, breaks, TRUE),
               factor(rep(c("[min,5]", "(5,max]"), each = 5),
                      levels = c("[min,5]", "(5,max]")))

  expect_equal(cut_var(var2, breaks, TRUE),
               factor(rep(c("[min,5]", "(5,max]"), c(4, 6)),
                      levels = c("[min,5]", "(5,max]")))

  expect_equal(cut_var(var2, breaks, FALSE),
               factor(c(rep(c("[1,5]", "(5,10]"), c(4, 5)), NA),
                      levels = c("[1,5]", "(5,10]")))
})

test_that("adjust_levels_min_max gives correct output", {
  f1 <- cut(c(3, 5, 9), c(1, 4, 7, 10), include.lowest = TRUE)
  f2 <- cut(c(3, 5, 9), c(1, 7, 10), include.lowest = TRUE)
  f3 <- cut(c(3, 5, 9), c(1, 10), include.lowest = TRUE)
  f1_res <- factor(c("[min,4]", "(4,7]", "(7,max]"),
                   levels = c("[min,4]", "(4,7]", "(7,max]"))
  f2_res <- factor(c("[min,7]", "[min,7]", "(7,max]"),
                   levels = c("[min,7]", "(7,max]"))
  f3_res <- factor(rep("[min,max]", 3))
  expect_equal(adjust_levels_min_max(f1), f1_res)
  expect_equal(adjust_levels_min_max(f2), f2_res)
  expect_equal(adjust_levels_min_max(f3), f3_res)
})

test_that("step_cut integration test", {

  tb <- tibble(x = c(2, 5, 7), y = c(1, 8, 12))
  tb2 <- tibble(x = c(5, 9))
  tb3 <- tibble(x = c(-1, 6))

  expect_equal(recipe(tb) %>%
                 step_cut(x, breaks = 5) %>%
                 prep() %>%
                 bake(tb) %>%
                 pull(x),
               factor(c("[2,5]", "[2,5]", "(5,7]"),
                      levels = c("[2,5]", "(5,7]")))

  expect_equal(recipe(tb) %>%
                 step_cut(y, breaks = c(3, 9)) %>%
                 prep() %>%
                 bake(tb) %>%
                 pull(y),
               factor(c("[1,3]", "(3,9]", "(9,12]"),
                      levels = c("[1,3]", "(3,9]", "(9,12]")))

  result <- tibble(x = factor(c("[2,4]", "(4,6]",  "(6,7]"),
                              levels = c("[2,4]", "(4,6]",  "(6,7]")),
                   y = factor(c("[1,4]", "(6,12]", "(6,12]"),
                              levels = c("[1,4]", "(4,6]", "(6,12]")))

  expect_equal(recipe(tb) %>%
                 step_cut(x, y, breaks = c(4, 6)) %>%
                 prep() %>%
                 bake(tb),
               result)

  expect_equal(recipe(tb) %>%
                 step_cut(x, breaks = 5, include_outside_range = TRUE) %>%
                 prep() %>%
                 bake(tb2) %>%
                 pull(x),
               factor(c("[min,5]", "(5,max]"),
                      levels = c("[min,5]", "(5,max]")))

  expect_equal(recipe(tb) %>%
                 step_cut(x, breaks = 5, include_outside_range = TRUE) %>%
                 prep() %>%
                 bake(tb3) %>%
                 pull(x),
               factor(c("[min,5]", "(5,max]"),
                      levels = c("[min,5]", "(5,max]")))
})
