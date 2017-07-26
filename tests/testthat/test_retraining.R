context("Testing retraining")

data(biomass)

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass)

test_that('training in stages', {
  skip_on_cran()
  at_once <- rec %>% 
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur) %>% 
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur) 
  
  at_once_trained <- prep(at_once, training = biomass, verbose = FALSE)
  
  ## not train in stages
  center_first <- rec %>% 
    step_center(carbon, hydrogen, oxygen, nitrogen, sulfur)
  center_first_trained <- prep(center_first, training = biomass, verbose = FALSE)
  in_stages <- center_first_trained %>%
    step_scale(carbon, hydrogen, oxygen, nitrogen, sulfur) 
  in_stages_trained <- prep(in_stages, training = biomass, verbose = FALSE)
  in_stages_retrained <- prep(in_stages, training = biomass, verbose = FALSE, fresh = TRUE) 
  
  expect_equal(at_once_trained, in_stages_trained)
  expect_equal(at_once_trained, in_stages_retrained)
})
