library(testthat)
library(recipes)

if (requireNamespace("xml2")) {
  test_check("recipes", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("recipes")
}
