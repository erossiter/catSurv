# context("jsonCat")
# load("cat_objects.Rdata")
# 
# test_json <- toJSON(list(guessing = rep(0, 10),
#           discrimination = rnorm(n = 10, 0, 1.5),
#           difficulty = rnorm(n = 10, 0, 3),
#           answers = sample(c(0,1), size = 10, replace = TRUE),
#           priorName = "NORMAL",
#           priorParams = c(0,1),
#           lowerBound = -5,
#           upperBound = 5,
#           model = "ltm",
#           estimation = "EAP",
#           estimationDefault = "EAP",
#           selection = "EPV",
#           z = .1,
#           lengthThreshold = NA,
#           seThreshold = NA,
#           infoThreshold = NA,
#           gainThreshold = NA,
#           lengthOverride = NA,
#           gainOverride = NA))
# 
# bad_json <- toJSON(list(guessing = rep(0, 10),
#           discrimination = rnorm(n = 9, 0, 1.5),
#           difficulty = rnorm(n = 10, 0, 3),
#           answers = sample(c(0,1), size = 10, replace = TRUE),
#           priorName = "NORMAL",
#           priorParams = c(0,1),
#           lowerBound = -5,
#           upperBound = 5,
#           model = "ltm",
#           estimation = "EAP",
#           estimationDefault = "EAP",
#           selection = "EPV",
#           z = .1,
#           lengthThreshold = NA,
#           seThreshold = NA,
#           infoThreshold = NA,
#           gainThreshold = NA,
#           lengthOverride = NA,
#           gainOverride = NA))
# 
# test_that("jsonCat populates slots correctly", {
#   cat <- jsonCat(test_json)
#   expect_equal(cat@model, "ltm")
#   expect_equal(cat@z, .1)
# })
# 
# test_that("jsonCat throws errors", {
#   expect_error(catJson(1))
#   expect_error(catJson(bad_json))
# })
