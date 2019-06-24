context("estimateThetas")
load("cat_objects.Rdata")


## Takes a true value of theta and n number of respondents simulate
## and simulates FULL response profiles
simulateRespondents_test <- function(cat, theta, n){
  if(sum(!is.na(cat@answers)) != 0){
    stop("Cat object should not have respondent specific answers.")
  }

  ans_profiles <- matrix(nrow = n, ncol = length(cat@answers))
  for(respondent in 1:n){
    for(i in 1:length(cat@answers)){
      probs <- probability(catObj = cat, theta = theta, item = i)

      ## need to calculate answer probabilities from cumulative probabilities
      if(cat@model == "grm"){
        probs <- diff(probs)
      }
      ## need to append probability of answering a 0
      if(cat@model == "ltm" | cat@model == "tpm"){
        probs <- c(1 - probs, probs)
      }
      ## gpcm is fine

      ## now generate answers with those probabilities
      ans_profiles[respondent, i] <- sample(1:(length(cat@difficulty[[i]])+1), 1, prob = probs)
    }
  }
  return(as.data.frame(ans_profiles))
}

test_that("simulateRespondents for ltm returns profiles with correct probs", {
    set.seed(1234)
    probs <- probability(ltm_cat, 1, 10)
    sims <- simulateRespondents_test(ltm_cat, 1, 500)
    recover_probs <- table(sims$V10)/nrow(sims)
    names(recover_probs) <- NULL
    
    expect_equal(round(probs, 2), round(recover_probs[2], 2))
})

test_that("simulateRespondents for grm returns profiles with correct probs", {
    set.seed(12345)
    probs <- diff(probability(grm_cat, 1, 10))
    sims <- simulateRespondents_test(grm_cat, 1, 5000)
    recover_probs <- as.numeric(table(sims$V10)/nrow(sims))
    names(recover_probs) <- NULL
    
    expect_equal(round(probs, 2), round(recover_probs, 2))
})
