# remotes::install_version("recipes", "1.0.1")
library(recipes)

data("Sacramento", package = "modeldata")
Sacramento$lgl1 <- Sacramento$beds >= 1
Sacramento$lgl2 <- Sacramento$beds >= 2
Sacramento$ord1 <- as.ordered(Sacramento$type)
Sacramento$ord2 <- as.ordered(Sacramento$type)
Sacramento$date1 <- as.Date(Sacramento$sqft, "2000-01-01")
Sacramento$date2 <- as.Date(Sacramento$sqft, "2000-01-01")
Sacramento$datetime1 <- as.POSIXct(Sacramento$sqft, origin = "2000-01-01 00:00:00")
Sacramento$datetime2 <- as.POSIXct(Sacramento$sqft, origin = "2000-01-01 00:00:00")

old_rec_sac <- recipe(~., data = Sacramento)

old_pca_rec_sac <- old_rec_sac %>%
  step_normalize(all_numeric_predictors()) %>%
  step_pca(beds, baths, sqft) %>%
  prep()

# Saving -----------------------------------------------------------------------
save(
  old_rec_sac,
  old_pca_rec_sac,
  file = testthat::test_path("old-get_types.RData"),
  version = 2
)
