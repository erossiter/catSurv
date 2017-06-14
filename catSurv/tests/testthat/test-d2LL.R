context("d2LL")
load("cat_objects.Rdata")

d2LL_test <- function(cat, theta, usePrior) {
  answered_questions <- which(!is.na(cat@answers))
  prior_shift <- 1 / cat@priorParams[2]^2

  if(length(answered_questions) == 0) {
    L_theta <- prior_shift
  }

  sum_this <- rep(0, length(answered_questions))

  if(cat@model == "ltm" | cat@model == "tpm"){
    for(i in 1:length(answered_questions)){
      item <- answered_questions[i]
      P <- probability(cat, theta, item)
      Q <- 1 - P
      sum_this[i] <- cat@discrimination[i]^2 * ((P-cat@guessing[i]) /
                                               (1-cat@guessing[i]))^2 * (Q/P)
      }
    L_theta <- (-1 * sum(sum_this))
  }

  if(cat@model == "grm"){
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
      sum_this[i] <- cat@discrimination[i]^2 * ((-W2*(Qstar2-Pstar2)
                                                 + W1*(Qstar1-Pstar1)) /
                                                P - ((W1 - W2)^2/P^2))
    }
    L_theta <- sum(sum_this)
  }

  if(cat@model == "gpcm"){
    for(i in 1:length(answered_questions)){
      ## first and second derivatives of probability
      item <- answered_questions[i]
      discrimination = cat@discrimination[item]
      categoryparams = c(0, cat@difficulty[[item]])
      f <- f_prime <- f_primeprime <- rep(NA,length(categoryparams))
      for(ans in 1:length(categoryparams)){
        f[ans] <- exp(sum(discrimination * (theta - categoryparams[1:ans])))
        f_prime[ans] <- f[ans] * (discrimination * ans)
        f_primeprime[ans] <- f[ans] * (discrimination * ans)^2
      }
      g <- sum(f)
      g_prime <- sum(f * (discrimination * 1:length(categoryparams)))
      g_primeprime <- sum(f * (discrimination * 1:length(categoryparams))^2)
      p_prime <- (g * f_prime - f * g_prime) / g^2

      a <- (g * f_prime - f * g_prime)
      a_prime <- f_primeprime * g - g_primeprime * f
      b <- g^2
      b_prime <- (2 * g) * g_prime
      p_primeprime <- (b * a_prime - a * b_prime) / b^2

      answer_k <- cat@answers[item]
      p <- probability(cat, theta, item)
      sum_this[i] <- ((p_prime[answer_k]^2 / p[answer_k]^2)
                      - (p_primeprime[answer_k] / p[answer_k]))
      }
    L_theta <- - sum(sum_this)
  }

  if(usePrior == TRUE){
    L_theta <- L_theta - prior_shift
  }
  return(L_theta)
}

test_that("ltm d2LL calculates correctly", {
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_d2LL <- d2LL(ltm_cat, 1, FALSE)
  test_d2LL <- d2LL_test(ltm_cat, 1, FALSE)

  expect_equal(package_d2LL, test_d2LL)
})

test_that("grm d2LL calculates correctly", {
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_d2LL <- d2LL(grm_cat, 1, FALSE)
  test_d2LL <- d2LL_test(grm_cat, 1, FALSE)

  expect_equal(package_d2LL, test_d2LL)
})

test_that("gpcm d2LL calculates correctly", {
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_d2LL <- d2LL(gpcm_cat, 1, FALSE)
  test_d2LL <- d2LL_test(gpcm_cat, 1, FALSE)

  expect_equal(package_d2LL, test_d2LL)
})


