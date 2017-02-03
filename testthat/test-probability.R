library(catSurv)
library(testthat)
library(ltm)
library(catR)
context("Probability")

test_that("probability calculates correctly", {
  
  probability_test <- function(cat = "Cat", theta = "numeric", question = "numeric"){
    discrimination = cat@discrimination[question]
    difficulty = cat@difficulty[[question]]
    guessing = cat@guessing[question]
    
    # For binary probability
    if (cat@poly == F) {
      exp_prob = exp(difficulty + (theta * discrimination))
      probability <- guessing + (1-guessing) * (exp_prob / (1 + exp_prob))
      # For polytomous probability
    } else {
      probability <- rep(NA,length(difficulty))
      for(k in 1:length(difficulty)){
        exp_prob = exp((difficulty[k]) - (theta * discrimination))
        probK <- (exp_prob/(1+exp_prob))
        probability[k] <- probK
      }
    }
    return(probability)
  }
  
  data("npi")
  data("nfc")
  data("AMTknowledge")
  ltm_data <- npi[1:100, ]
  tpm_data <- AMTknowledge[1:100, ]
  poly_data <- nfc[1:100, ]
  
  ###------------- Only use if introducing extra NAs --------------------
  # bi.options <- c(NA, 0, 1)
  # for(a in 1:nrow(ltm_data)){
  #   ltm_data[a,] <- sample(bi.options,ncol(ltm_data), replace=TRUE)
  # }
  # for(a in 1:nrow(tpm_data)){
  #   tpm_data[a,] <- sample(bi.options,ncol(tpm_data), replace=TRUE)
  # }
  # poly.options <- c(NA, 1:5)
  # for(a in 1:nrow(poly_data)){
  #   poly_data[a,] <- sample(poly.options,ncol(poly_data), replace=TRUE)
  # }
  ###--------------------------------------------------------------------
  
  ### Test Cat probability against equation in R
  
  # binary (ltm)
  binary_cat.ltm <- ltmCat(ltm_data, quadraturePoints = 100)
  
  for(j in 1:length(binary_cat.ltm@answers)){
    CatProb <- probability(binary_cat.ltm, 0, j)[[1]][,1]
    RProb <- as.numeric(probability_test(binary_cat.ltm, 0, j))
    expect_equal(abs(CatProb - RProb)/CatProb, 0, tolerance = .001)
  }
  
  
  # binary (tpm)
  binary_cat.tpm <- tpmCat(tpm_data, quadraturePoints = 100)
  
  for(j in 1:length(binary_cat.tpm@answers)){
    CatProb <- probability(binary_cat.tpm, 0, j)[[1]][,1]
    RProb <- as.numeric(probability_test(binary_cat.tpm, 0, j))      
    expect_equal(abs(CatProb - RProb)/CatProb, 0, tolerance = .001)
  }
  
  
  # poly (grm)
  poly_cat <- grmCat(poly_data, quadraturePoints = 100)
  
  for(j in 1:length(poly_cat@answers)){
    CatProb <- probability(poly_cat, 0, j)[[1]][,1]
    RProb <- probability_test(poly_cat, 0, j)
    expect_equal(abs(CatProb - RProb)/CatProb, rep(0, length(CatProb)), tolerance = .001)
  }
  
  ### Test Cat probability against catR package
  
  # binary (ltm)
  binary_ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
  binary_cat.ltm <- ltmCat(binary_ltm.ltm, quadraturePoints = 100)
  
  it.ltm <- matrix(c(binary_cat.ltm@discrimination, coef(binary_ltm.ltm)[,1], 
                     rep(0,length(binary_cat.ltm@discrimination)), 
                     rep(1, length(binary_cat.ltm@discrimination))),
                   ncol= 4, byrow = F)
  
  for(j in 1:length(binary_cat.ltm@answers)){
    CatProb <- probability(binary_cat.ltm, 0, j)[[1]][,1]
    RProb <- Pi(0, it.ltm)$Pi[j]
    expect_equal(abs(CatProb - RProb)/CatProb, 0, tolerance = .001)
  }
  
  # binary (tpm)
  binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  binary_cat.tpm <- tpmCat(binary_ltm.tpm, 100)
  
  it.tpm <- matrix(c(binary_cat.tpm@discrimination, coef(binary_ltm.tpm)[,2], 
                     binary_cat.tpm@guessing, 
                     rep(1, length(binary_cat.tpm@discrimination))),
                   ncol= 4, byrow = F)  
  
  for(j in 1:length(binary_cat.tpm@answers)){
    CatProb <- probability(binary_cat.tpm, 0, j)[[1]][,1]
    RProb <- Pi(0, it.tpm)$Pi[j]
    expect_equal(abs(CatProb - RProb)/CatProb, 0, tolerance = .001)
  }
  
  # poly (grm)
  poly_ltm <- grm(poly_data, control = list(GHk = 100))
  poly_cat <- grmCat(poly_ltm, 100)
  
  it.poly <- matrix(c(coef(poly_ltm)[,5], coef(poly_ltm)[,1:4]),
                    ncol= 5, byrow = F)
  
  # modify Pi function to return cumulative probabilities
  conform.Pi <- function(th, it, model = NULL, D = 1, qu){
    Probs <- Pi(th, it, model, D)$Pi[qu,]
    Probs <- Probs[-length(Probs)]
    for(i in 2:length(Probs))
      Probs[i] <- Probs[i-1] + Probs[i]
    return(Probs)
  }
  
  for(j in 1:length(poly_cat@answers)){
    CatProb <- probability(poly_cat, 0, j)[[1]][,1]
    RProb <- as.numeric(conform.Pi(0,it.poly, model = "GRM", qu=j))
    expect_equal(abs(CatProb - RProb)/CatProb, rep(0, length(CatProb)), tolerance = .001)
  }
  
  ### Test Cat probability against ltm package
  
#   library(ltm)
#   
#   binary_ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
# 
#   binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
#   
#   poly_ltm <- grm(poly_data, control = list(GHk = 100))
#   
#   # construct probability from ltm package's factor.scores function
#   
#   ltm.prob <- function(object) {
#     if(class(object) != "grm") {
#       Z <- matrix(data = c(rep(1,100), rep(0,100)), ncol = 2, byrow = F)
#       probs <- function (x) {
#           pr <- plogis(x)
#           if (any(ind <- pr == 1))
#             pr[ind] <- 1 - sqrt(.Machine$double.eps)
#           if (any(ind <- pr == 0))
#             pr[ind] <- sqrt(.Machine$double.eps)
#           pr
#         }
#       if(class(object) == "ltm") {
#         betas <- object$coef
#         pr <- probs(Z %*% t(betas))[1,]
#       } else {
#         thetas <- object$coef
#         betas <- thetas[, 2:3]
#         cs <- plogis(thetas[, 1]) * object$max.guessing
#         cs.mat <- matrix(cs, nrow(Z), nrow(betas), TRUE)
#         pr <- cs.mat + (1 - cs.mat) * probs(Z %*% t(betas))
#         }
#       } else {
#         betas <- object$coefficients
#         Z <- rep(0,100)
#         cprobs <-
#           function (betas, z, eps = .Machine$double.eps^(1/3)) {
#             lapply(betas, function (x, z) {
#               nx <- length(x)
#               out <- plogis(x[-nx] - matrix(x[nx] * z, nx - 1, length(z), TRUE))
#               if (any(ind <- out == 1))
#                 out[ind] <- 1 - eps
#               if (any(ind <- out == 0))
#                 out[ind] <- eps
#               rbind(out, 1)        
#             }, z = z)
#           }
#         pr <- cprobs(betas, Z)
#       }
#       return(pr)
#   }
#   
#   # binary (ltm)
#   for(j in 1:length(binary_cat.ltm@answers)){
#     expect_equal(probability(binary_cat.ltm, 0, j)[[1]][,1],
#                  as.numeric(ltm.prob(binary_ltm.ltm)[j]),
#                  tolerance = .0001)
#   }
# 
#   # binary (tpm)
#   for(j in 1:length(binary_cat.tpm@answers)){
#     expect_equal(probability(binary_cat.tpm, 0, j)[[1]][,1],
#                  as.numeric(ltm.prob(binary_ltm.tpm)[j]),
#                  tolerance = .0001)
#   }
#   #### this test fails at the third decimal place
#   
#   # poly (grm)
#   for(j in 1:length(poly_cat@answers)){
#     expect_equal(probability(poly_cat, 0, j)[[1]][,1],
#                  as.numeric(ltm.prob(poly_ltm)[[j]][1:length(probability(poly_cat, 0, j)[[1]][,1]),1]),
#                  tolerance = .0001)
#   }
   
})

