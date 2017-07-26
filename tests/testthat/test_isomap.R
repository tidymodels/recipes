library(testthat)
library(recipes)

## expected results form the `dimRed` package

exp_res <- structure(list(Isomap1 = c(0.312570873898531, 0.371885353599467, 2.23124009833741,
                                      0.248271457498181, -0.420128801874122),
                          Isomap2 = c(-0.443724171391742, -0.407721529759647, 0.245721022395862,
                                      3.112001672258, 0.0292770508011519),
                          Isomap3 = c(0.761529345514676, 0.595015565588918, 1.59943072269788,
                                      0.566884409484389, 1.53770327701819)),
                     .Names = c("Isomap1","Isomap2", "Isomap3"),
                     class = c("tbl_df", "tbl", "data.frame"),
                     row.names = c(NA, -5L))

set.seed(1)
dat1 <- matrix(rnorm(15), ncol = 3)
dat2 <- matrix(rnorm(15), ncol = 3)
colnames(dat1) <- paste0("x", 1:3)
colnames(dat2) <- paste0("x", 1:3)

rec <- recipe( ~ ., data = dat1)

test_that('correct Isomap values', {
  skip_on_cran()
  im_rec <- rec %>%
    step_isomap(x1, x2, x3, options = list(knn = 3), num = 3)

  im_trained <- prep(im_rec, training = dat1, verbose = FALSE)

  im_pred <- bake(im_trained, newdata = dat2)

  all.equal(as.matrix(im_pred), as.matrix(exp_res))
})


test_that('printing', {
  im_rec <- rec %>%
    step_isomap(x1, x2, x3, options = list(knn = 3), num = 3)
  expect_output(print(im_rec))
  expect_output(prep(im_rec, training = dat1))
})

