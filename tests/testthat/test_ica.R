library(testthat)
library(recipes)
data(biomass)

context("ICA")


biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr)

## Generated using fastICA
exp_comp <- structure(
  c(-0.741586750989301, -0.473165319478851, -0.532724778033598,
    0.347336643017696, -0.523140911818999, 0.0839020928800183, -0.689112937865132,
    1.1905359062157, 2.87389193916233, 3.87655326677861, 0.662748883270711,
    0.108848159489063, 0.509384921516091, -0.708397515735095, -0.129606867389727,
    1.7900565287023, 0.171125628795304, 0.314289325954585, -0.142425199147843,
    -0.619509248504534, 0.38690051207701, -0.414352364956822, -0.609744599991299,
    -0.144705519030626, -0.293470631707416, -0.791746573929697, -0.634208572824357,
    1.36675934105489, -0.785855217530414, -0.730790987290872, -0.236417274868796,
    -0.210596011735952, -0.413793241941344, -0.511246150962085, -0.181254985021062,
    0.298659496162363, -0.757969803548959, -0.666845883775384, -0.240983277334825,
    -0.394806974813201, 1.44451054341856, 3.33833135277739, -0.54575996404394,
    -0.423145023192357, -0.388925027133234, -0.418629250017466, -0.463085718807788,
    -0.14499128867367, 0.323243757311295, -0.417689940076107, -0.777761367811451,
    -0.799107717902467, -0.548346133015069, 0.769235286712577, -0.40466870434895,
    -0.591389964794494, -0.208052301856056, -0.945352336400244, 0.919793619211536,
    -0.561549525440524, -0.535789943464846, -0.735536725127484, 3.7162236121338,
    0.459835444175181, 0.137984939011763, -0.755831873688866, -0.757751495230731,
    -0.512815283263682, 0.901123348803226, -0.755032174981781, -1.04745496967861,
    -0.481720409476034, -0.956534770489922, 2.39775097011864, -0.537189360991569,
    0.455171520278689, -0.764070183446535, -0.0133182183358093, 0.0084094629323547,
    -0.11887530759164, -0.50492491720854, -0.731237740789087, -0.810056304451282,
    -0.0654477889270799, -0.165218457853762, -0.384457532271443,
    -1.25744957888255, -0.164838366701182, -0.818591960610985, -0.577844253001226,
    0.159731749239493, -0.350242543749645, 3.22437340069565, -0.575271823706669,
    -0.171250094126726, 1.21819592885382, -0.303636775510361, 0.192247367642684,
    0.235728177283036, -0.768212986589321, 0.333147682813931, -0.403932170943429,
    -0.261749940045069, -0.331436881499356, -0.298793661022028, -0.255788540744319,
    -0.764483629396313, -0.162133725599773, -0.10676549266036, -0.349722429991475,
    -0.340728544016434, -0.358565693266084, 0.0242508678396987, -0.277425329351928,
    0.055217077863271, 0.146403703647814, -0.241268230680493, -0.283770652745491,
    -0.573657866580657, -0.224655195396099, 0.226079102614757, 2.03305968574443,
    -0.225655562941607, -0.155789455588855, -0.613828894885655, 0.480057477445702,
    0.277055812270816, -0.263765068404404, 0.0411239101983566, 0.30164066516454,
    -0.760891669412883, -0.478609196612072, -0.162692709808673, 3.12547570195871,
    -0.189300748528298, -0.16882558146447, -0.30745201359965, 2.77823976198232,
    -0.306599455530011, -0.979722296618571, -0.913952653732135, -0.608622766593967,
    -0.061561169157735, 0.0134953299517241, -0.111595843415483, -0.0995809192931606,
    -0.353150299985198, -0.173474040260694, -0.11913118533085, -0.268152445374219,
    -1.64524056576117, -0.052825674116391, 2.82692828099746, -0.257823074604271,
    -0.0316348082448068, -0.347414676200845, -0.237534967478309,
    -0.266298103195764, -0.0555773569483491, 2.35155293218832),
  .Dim = c(80L,  2L),
  .Dimnames = list(c("15", "20", "26", "31", "36", "41", "46",
                     "51", "55", "65", "69", "73", "76", "88", "91", "126", "132",
                     "136", "141", "147", "155", "162", "167", "173", "178", "183",
                     "190", "196", "203", "208", "213", "218", "223", "230", "235",
                     "241", "252", "257", "262", "267", "277", "282", "286", "294",
                     "299", "305", "309", "314", "319", "325", "330", "348", "353",
                     "357", "359", "370", "375", "385", "399", "407", "409", "414",
                     "419", "424", "429", "434", "439", "448", "467", "473", "477",
                     "482", "485", "493", "499", "516", "519", "527", "532", "535"
  ), c("IC1", "IC2")))
rownames(exp_comp) <- NULL

test_that('correct ICA values', {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("fastICA")
  skip_if_not_installed("RSpectra")

  ica_extract <- rec %>%
    step_ica(carbon, hydrogen, oxygen, nitrogen, sulfur, num_comp = 2, id = "")

  set.seed(12)
  ica_extract_trained <- prep(ica_extract, training = biomass_tr, verbose = FALSE)

  ica_pred <- bake(ica_extract_trained, new_data = biomass_te, all_predictors())
  ica_pred <- as.matrix(ica_pred)

  rownames(ica_pred) <- NULL

  expect_equal(ica_pred, exp_comp)

  vars <- c("carbon", "hydrogen", "oxygen" ,"nitrogen", "sulfur")
  tidy_exp_un <- tibble(
    terms = rep(vars, 2),
    value = rep(NA_real_, 2*5),
    component = rep(c("IC1", "IC2"), 5),
    id = ""
  )
  expect_equal(tidy_exp_un, tidy(ica_extract, number = 1))

  loadings <- dimRed::getRotationMatrix(ica_extract_trained$steps[[1]]$res)
  comps <- ncol(loadings)
  loadings <- as.data.frame(loadings)
  rownames(loadings) <- vars
  colnames(loadings) <- paste0("IC", 1:comps)
  loadings <- utils::stack(loadings)

  tidy_exp_tr <- tibble(
    terms = tidy_exp_un$terms,
    value = loadings$values,
    component = as.character(loadings$ind),
    id = ""
  )
  expect_equal(tidy_exp_tr, tidy(ica_extract_trained, number = 1))

})


test_that('printing', {
  ica_extract <- rec %>%
    step_ica(carbon, hydrogen, num_comp = 2)
  expect_output(print(ica_extract))
  expect_output(prep(ica_extract, training = biomass_tr, verbose = TRUE))
})


test_that('No ICA comps', {
  ica_extract <- rec %>%
    step_ica(carbon, hydrogen, oxygen, nitrogen, sulfur, num_comp = 0)

  ica_extract_trained <- prep(ica_extract, training = biomass_tr)
  expect_equal(
    names(juice(ica_extract_trained)),
    names(biomass_tr)[-(1:2)]
  )
  expect_true(all(names(ica_extract_trained$steps[[1]]$res) == "x_vars"))
  expect_output(print(ica_extract_trained),
                regexp = "No ICA components were extracted")
  expect_true(all(is.na(tidy(ica_extract_trained, 1)$value)))
})



test_that('tunable', {
  rec <-
    recipe(~ ., data = iris) %>%
    step_ica(all_predictors())
  rec_param <- tunable.step_ica(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_comp"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c('name', 'call_info', 'source', 'component', 'component_id')
  )
})
