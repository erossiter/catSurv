library(catSurv)
library(testthat)
library(ltm)
context("Likelihood")

## should we keep error message below about weird proabbilities??

test_that("likelihood calculates correctly", {

  likelihood_test <- function(cat = "Cat", theta = "numeric"){
    
    # Identifies answered questions and combines them in vector
    answered_indices <- which(!is.na(cat@answers), arr.ind = T)
    ansVec <- cat@answers[answered_indices]
    
    # Create list of probability vectors for each question
    pList <- lapply(answered_indices, function(i) {
      probability(cat, theta, i)$all.probabilities$probabilities
    })
    
    if (length(answered_indices) > 0) {
      
      # For binary likelihood
      if (cat@poly == F) {
        # Creates a vector of values inside the product function in equation (3) [from the documentation]
        pqVec<-sapply(1:length(pList), function(i) {
          (pList[[i]]^(ansVec[i])) * ((1 - pList[[i]])^(1 - ansVec[i]))
        })
        # Apply product function over this vector
        #print(pqVec)
        likelihood <- prod(pqVec) 
      } 
      
      #  For polytomous likelihood
      if (cat@poly == T) {
      
      pListFinal <- vector("list", length(pList))
      for (i in 1:length(pList)) {  # Iterating over question
        # Append 0 and 1 to front and back or each vector of probabilties
        pList[[i]] <- c(0, pList[[i]], 1)
        #print(pList[[i]])
        for (k in 2:length(pList[[i]])) { #  iterating over response categories
          pListFinal[[i]][k - 1] <- pList[[i]][k] - pList[[i]][k - 1]
        }
      }
      
      probExpList <- pListFinal #  Copying for dimensions
      for (i in 1:length(probExpList)) {
        for (k in 1:length(probExpList[[i]])) {
          probExpList[[i]][k] <- (pListFinal[[i]][k])^(ansVec[i] == k)
        }
      }
      
      # Multiplying over response categories
      productVec <- sapply(probExpList, prod)
      if(any(productVec < 0)){
        stop("P_ijk shouldn't be negative?")
      }
      # Multiplying over question items
      likelihood <- prod(productVec)
      } 
      return(likelihood)
    } else return (1)
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

  ### Test Cat likelihood against equation in R
  
  # binary (ltm)
  binary_cat.ltm <- ltmCat(ltm_data, quadraturePoints = 100)
  
  for(i in 1:nrow(ltm_data)){
    binary_cat.ltm@answers <- as.numeric(ltm_data[i,])
    CatLik <- likelihood(binary_cat.ltm, 0)
    RLik <- likelihood_test(binary_cat.ltm, 0)
    expect_equal(abs(CatLik - RLik)/CatLik, 0, tolerance = .001)
  }
  
  # binary (tpm)
  binary_cat.tpm <- tpmCat(tpm_data, quadraturePoints = 100)
  
  for(i in 1:nrow(tpm_data)){
    binary_cat.tpm@answers <- as.numeric(tpm_data[i,])
    CatLik <- likelihood(binary_cat.tpm, 0)
    RLik <- likelihood_test(binary_cat.tpm, 0)
    expect_equal(abs(CatLik - RLik)/CatLik, 0, tolerance = .001)
  }
  
  # poly (grm)
  poly_cat <- grmCat(poly_data, quadraturePoints = 100)  
  
  for(i in 1:nrow(poly_data)){
    poly_cat@answers <- as.numeric(poly_data[i,])
    CatLik <- likelihood(poly_cat, 0)
    RLik <- likelihood_test(poly_cat, 0)
    expect_equal(abs(CatLik - RLik)/CatLik, 0, tolerance = .001)
  }
  
  ### Test Cat likelihood against ltm package
  
  ltm.lik <- function(object) {
    fits <- fitted(object)
    X <- fits[, -ncol(fits), drop = FALSE]
    mX <- 1 - X
    if(class(object) != "grm") {
      Z <- matrix(data = c(rep(1,100), rep(0,100)), ncol = 2, byrow = F)
      probs <- function (x) {
        pr <- plogis(x)
        if (any(ind <- pr == 1))
          pr[ind] <- 1 - sqrt(.Machine$double.eps)
        if (any(ind <- pr == 0))
          pr[ind] <- sqrt(.Machine$double.eps)
        pr
      }
      if(class(object) == "ltm") {
        betas <- object$coef
        pr <- probs(Z %*% t(betas))
        
        if (any(na.ind <- is.na(X)))
          X[na.ind] <- mX[na.ind] <- 0
        p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))[,1]
      } else {
        thetas <- binary_ltm.tpm$coef
        betas <- thetas[, 2:3]
        cs <- plogis(thetas[, 1]) * object$max.guessing
        cs.mat <- matrix(cs, nrow(Z), nrow(betas), TRUE)
        pr <- cs.mat + (1 - cs.mat) * probs(Z %*% t(betas))
        if (any(na.ind <- is.na(X)))
          X[na.ind] <- mX[na.ind] <- 0
        p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
        }
    } else {  
      betas <- poly_ltm$coefficients
      Z <- rep(0,100)
      cprobs <- function (betas, z, eps = .Machine$double.eps^(1/3)) {
        lapply(betas, function (x, z) {
          nx <- length(x)
          out <- plogis(x[-nx] - matrix(x[nx] * z, nx - 1, length(z), TRUE))
          if (any(ind <- out == 1))
            out[ind] <- 1 - eps
          if (any(ind <- out == 0))
            out[ind] <- eps
          rbind(out, 1)        
        }, z = z)
      }
      cpr <- cprobs(betas, Z)
      diff.cprs <- lapply(cpr, function (x) rbind(x[1, ], diff(x)))
      log.diff.cprs <- lapply(diff.cprs, log)
      log.p.xz <- matrix(0, nrow(X), length(Z))
      p <- length(betas)
      for (j in 1:p) {
        log.pr <- log.diff.cprs[[j]]
        xj <- X[, j]
        na.ind <- is.na(xj)
        log.pr <- log.pr[xj, , drop = FALSE]
        if (any(na.ind))
          log.pr[na.ind, ] <- 0
        log.p.xz <- log.p.xz + log.pr
      }
      p.xz <- exp(log.p.xz)[,1]
    }
    return(p.xz)
  }
  
  # binary (ltm)
  binary_ltm.ltm <- ltm(ltm_data ~ z1, control = list(GHk = 100))
  binary_cat.ltm <- ltmCat(binary_ltm.ltm, 100)
  
  fits <- fitted(binary_ltm.ltm)
  X <- fits[, -ncol(fits), drop = FALSE]
  
  for(j in 1:100){
    binary_cat.ltm@answers <- as.numeric(X[j,])
    CatLik <- likelihood(binary_cat.ltm, 0)
    ltmLik <- ltm.lik(binary_ltm.ltm)[j]
    expect_equal(abs(CatLik - ltmLik)/CatLik, 0, tolerance = .001)
  }
  
  # binary (tpm)
  binary_ltm.tpm <- tpm(tpm_data, control = list(GHk = 100))
  binary_cat.tpm <- tpmCat(binary_ltm.tpm, 100)
  
  fits <- fitted(binary_ltm.tpm)
  X <- fits[, -ncol(fits), drop = FALSE]
  
  for(j in 1:100){
    binary_cat.tpm@answers <- as.numeric(X[j,])
    CatLik <- likelihood(binary_cat.tpm, 0)
    ltmLik <- ltm.lik(binary_ltm.tpm)[j]
    expect_equal(abs(CatLik - ltmLik)/CatLik, 0, tolerance = .001)
  }
  
  # binary (grm)
  poly_ltm <- grm(poly_data, control = list(GHk = 100))
  poly_cat <- grmCat(poly_ltm, 100)
  
  fits <- fitted(poly_ltm)
  X <- fits[, -ncol(fits), drop = FALSE]
  
  for(j in 1:100){
    poly_cat@answers <- as.numeric(X[j,])
    CatLik <- likelihood(poly_cat,0)
    ltmLik <- ltm.lik(poly_ltm)[j]
    expect_equal(abs(CatLik - ltmLik)/CatLik, 0, tolerance = .001)
  }
  
})