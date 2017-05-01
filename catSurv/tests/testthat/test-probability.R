library(catR, quietly = TRUE)
context("probability")
load("cat_objects.Rdata")

probability_test <- function(cat, theta, question){
  discrimination = cat@discrimination[question]
  difficulty = cat@difficulty[[question]]
  guessing = cat@guessing[question]
    
  if(cat@model == "ltm" | cat@model == "tpm") {
    exp_prob = exp(difficulty + (theta * discrimination))
    probabilities <- guessing + (1-guessing) * (exp_prob / (1 + exp_prob))
  }
  
  if(cat@model == "grm"){
    probabilities <- rep(NA, length(difficulty))
    for(k in 1:length(difficulty)){
      exp_prob = exp(difficulty[k] - (theta * discrimination))
      probabilities[k] <- exp_prob/(1 + exp_prob)
    }
    probabilities <- c(0, probabilities, 1)
  }
  
  if(cat@model == "gpcm"){
    categoryparams = c(0, difficulty)
    numerator <- rep(NA,length(categoryparams))
    for(k in 1:length(categoryparams)){  
      numerator[k] <- exp(sum(discrimination * (theta - categoryparams[1:k])))
    }
    probabilities <- numerator/sum(numerator)
  }
  names(probabilities) <- NULL
  return(probabilities)
}

test_that("ltm probability calculates correctly", {
  package_prob <- probability(ltm_cat, 1, 1)
  test_prob <- probability_test(ltm_cat, 1, 1)
  catR_prob <- Pi(th = 1, it = it_ltm)$Pi[1]
    
  expect_equal(package_prob, test_prob)
  expect_equal(package_prob, catR_prob)
})

test_that("grm probability calculates correctly", {
  package_prob <- probability(grm_cat, 1, 1)
  test_prob <- probability_test(grm_cat, 1, 1)
  catR_prob <- Pi(th = 1, it = it_grm, model = "GRM")$Pi[1,]
  catR_prob <- cumsum(c(0, catR_prob))
  names(catR_prob) <- NULL
    
  expect_equal(package_prob, test_prob)
  expect_equal(round(package_prob, 5), round(catR_prob), 5)
})

test_that("gpcm probability calculates correctly", {
  package_prob <- probability(gpcm_cat, 1, 1)
  test_prob <- probability_test(gpcm_cat, 1, 1)
  catR_prob <- Pi(th = 1, it = it_gpcm, model = "GPCM")$Pi[1,]
  names(catR_prob) <- NULL
  
  expect_equal(package_prob, test_prob)
  expect_equal(round(package_prob, 5), round(catR_prob), 5)
})

test_that("probability throws error when indexing beyond questions", {
  expect_error(probability(ltm_cat, 1, 0))
  expect_error(probability(ltm_cat, 1, 41))
  
  expect_error(probability(grm_cat, 1, 0))
  expect_error(probability(grm_cat, 1, 41))
  
  expect_error(probability(gpcm_cat, 1, 0))
  expect_error(probability(gpcm_cat, 1, 41))
})

test_that("probability (for polytomous models) throws error with extreme theta values", {
  #expect_error(probability(grm_cat, -100, 1))
  #expect_error(probability(grm_cat, 100, 1))
  
  expect_error(probability(gpcm_cat, -5000, 1))
  expect_error(probability(gpcm_cat, 1000, 1))
})
