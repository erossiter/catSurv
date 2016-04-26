library(catSurv)
library(testthat)
context("dLL")


test_that("dLL calculates correctly",{
  
dLL_test <- function(cat="Cat", theta="numeric", use_prior=TRUE) {
  answered_questions <- cat@applicable_rows
  L_theta <- 0
  if(length(answered_questions) == 0) {
    return_this <- ((theta - cat@priorParams[1]) / cat@priorParams[2]^2)
  }
  if(cat@poly == FALSE) {
    for(i in 1:length(answered_questions)) {
      P <- probability(cat, theta, answered_questions[i])
      Q <- 1-P
      L_theta <- L_theta + cat@discrimination * (P-cat@guessing / P(1-cat@guessing)) * (cat@answers[i]-P)
    }
  }
  if(cat@poly == TRUE) {
    for(i in 1:length(answered_questions)){
      answer_k <- cat@answers[i]
      probs <- probability(cat, theta, answered_questions[i])
      Pstar1 <- probs[answer_k]
      Qstar1 <- 1-Pstar1
      Pstar2 <- probs[answer_k -1]
      Qstar2 <- 1 - Pstar2
      P <- Pstar2 - Pstar1
      W2 <- Pstar2 * Qstar2
      W1 <- Pstar1 * Qstar1
      L_theta <- L_theta + (cat@discrimintation[item] * ((W2 - W1)/P))
    }
  }
  if(usePrior == TRUE){
    L_theta <- L_theta - ((theta - cat@priorParams[1])/cat@priorParams[2]^2)
  }
  return(L_theta)
}
  expect_equal(dLL(cat_df=allTheCats, theta=1, use_prior=TRUE), dLL_test(test_cat=allTheCats, 1, usePrior=TRUE))
  expect_equal(dLL(cat_df=allTheCats, theta=1, use_prior=FALSE), dLL_test(test_cat, 1, usePrior=FALSE))
})



catBiCreator<-function(numCats="numeric"){
  set.seed(999)
  allTheCats<-c()
  for(i in 1:numCats){
    numQuestions<-floor(abs(50*(rnorm(1))))
    newCat<-new("Cat",
                discrimination=(10*rnorm(numQuestions)),
                difficulty=sort(100*rnorm(numQuestions)),
                guessing=runif(numQuestions),
                poly=F,
                answers=rep(NA, numQuestions))
    
    allTheCats<-c(allTheCats, newCat)
    
  }
  return(as.list(allTheCats))
}

##running the function, creating 10 cats
## ADJUST THIS INPUT IF YOU WANT A SHORTER OR LONGER TEST
allTheCats<-catBiCreator(10)
