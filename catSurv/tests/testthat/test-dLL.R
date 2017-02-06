context("dLL")
load("cat_objects.Rdata")

dLL_test <- function(cat, theta, usePrior) {
  answered_questions <- which(!is.na(cat@answers))
  prior_shift <- ((theta - cat@priorParams[1]) / cat@priorParams[2]^2)
    
  if(length(answered_questions) == 0) {
    L_theta <- prior_shift
  }
    
  sum_this <- rep(0, length(answered_questions))

  if(cat@modelFit == "ltm"){
    for(i in 1:length(answered_questions)){
      item <- answered_questions[i]
      P <- probability(cat, theta, item)
      Q <- 1-P
      sum_this[i] <- cat@discrimination[i] * (cat@answers[i]-P) *
        ((P - cat@guessing[i]) / (P * (1 - cat@guessing[i])))
      }
      L_theta <- sum(sum_this)
  }
  
  if(cat@modelFit == "grm"){
    for(i in 1:length(answered_questions)){
      item <- answered_questions[i]
      answer_k <- cat@answers[item]
      probs <- probability(cat, theta, item)
      Pstar1 <- probs[answer_k+1]
      Qstar1 <- 1-Pstar1
      Pstar2 <- probs[answer_k]
      Qstar2 <- 1 - Pstar2
      P <- Pstar1 - Pstar2
      W2 <- Pstar2 * Qstar2
      W1 <- Pstar1 * Qstar1
      sum_this[i] <- -1*cat@discrimination[i] * ((W1 - W2)/P)
      }
    L_theta <- sum(sum_this)
  }
  
  if(cat@modelFit == "gpcm"){
    for(i in 1:length(answered_questions)){
      ## first derivative
      item <- answered_questions[i]
      discrimination = cat@discrimination[item]
      categoryparams = c(0, cat@difficulty[[item]])
      f <- f_prime <- rep(NA,length(categoryparams))
      for(ans in 1:length(categoryparams)){  
        f[ans] <- exp(sum(discrimination * (theta - categoryparams[1:ans])))
        f_prime[ans] <- f[ans] * (discrimination * ans)
      }
      g <- sum(f)
      g_prime <- sum(f * (discrimination * 1:length(categoryparams)))
      p_prime <- (g * f_prime - f * g_prime) / g^2
      
      answer_k <- cat@answers[item]
      p <- probability(cat, theta, item)
      sum_this[i] <- p_prime[answer_k] / p[answer_k]
      }
    L_theta <- sum(sum_this)
  }
  
  if(usePrior == TRUE){
    L_theta <- L_theta - prior_shift
  }
  return(L_theta)
}

test_that("ltm dLL calculates correctly", {
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_dLL <- dLL(ltm_cat, 1, FALSE)
  test_dLL <- dLL_test(ltm_cat, 1, FALSE)
  
  expect_equal(package_dLL, test_dLL)
})

test_that("grm dLL calculates correctly", {
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_dLL <- dLL(grm_cat, 1, FALSE)
  test_dLL <- dLL_test(grm_cat, 1, FALSE)
  
  expect_equal(package_dLL, test_dLL)
})

test_that("gpcm dLL calculates correctly", {
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4) 
  package_dLL <- dLL(gpcm_cat, 1, FALSE)
  test_dLL <- dLL_test(gpcm_cat, 1, FALSE)
  
  expect_equal(package_dLL, test_dLL)
})


