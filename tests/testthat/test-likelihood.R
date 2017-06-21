context("likelihood")
load("cat_objects.Rdata")

likelihood_test <- function(cat, theta){
  answered_qs <- which(!is.na(cat@answers), arr.ind = T)
  answers <- cat@answers[answered_qs]
  
  probs <- lapply(answered_qs, function(x) probability(cat, theta, x))
    
  if(cat@model == "ltm" | cat@model == "tpm") {
    prod_these <- sapply(1:length(probs), function(x){
      probs[[x]]^answers[x] * (1 - probs[[x]])^(1 - answers[x])})
    output <- prod(prod_these) 
  }
  
  if(cat@model == "grm"){
    prod_these <- vector("list", length(probs))
    for(i in 1:length(probs)){
      for (k in 2:length(probs[[i]])){
        prod_these[[i]][k - 1] <- probs[[i]][k] - probs[[i]][k - 1]
      }
    }
    for(i in 1:length(prod_these)){
      for(k in 1:length(prod_these[[i]])){
        prod_these[[i]][k] <- (prod_these[[i]][k])^(answers[i] == k)
      }
    }
    output <- prod(sapply(prod_these, prod))
  }
  
  if(cat@model == "gpcm"){
    L <- length(answered_qs)
    for(i in 1:length(answered_qs)){
      q <- answered_qs[i]
      answer <- cat@answers[q]
      probs <- probability(cat, theta, item = q)
      L[i] <- log(probs[answer])
      }
    output <- exp(sum(L))
  }
  
  return(output)
}


test_that("ltm likelihood calculates correctly", {
  ltm_cat@answers[1:5] <- c(0, 1, 0, 0, 1)
  package_lk <- likelihood(ltm_cat, 1)
  test_lk <- likelihood_test(ltm_cat, 1)
  
  expect_equal(package_lk, test_lk)
})

test_that("grm likelihood calculates correctly", {
  grm_cat@answers[1:5] <- c(4, 5, 2, 4, 4)
  package_lk <- likelihood(grm_cat, 1)
  test_lk <- likelihood_test(grm_cat, 1)
  
  expect_equal(package_lk, test_lk)
})

test_that("gpcm likelihood calculates correctly", {
  gpcm_cat@answers[1:5] <- c(4, 5, 2, 4, 4) 
  package_lk <- likelihood(gpcm_cat, 1)
  test_lk <- likelihood_test(gpcm_cat, 1)
  
  expect_equal(package_lk, test_lk)
})

test_that("likelihood calculates to 1 when no questions answered", {
  ltm_cat@answers[1:5] <- rep(NA, 5)
  grm_cat@answers[1:5] <- rep(NA, 5)
  gpcm_cat@answers[1:5] <- rep(NA, 5)
  
  expect_equal(likelihood(ltm_cat, 5), 1)
  expect_equal(likelihood(grm_cat, 5), 1)
  expect_equal(likelihood(gpcm_cat, 5), 1)
})


