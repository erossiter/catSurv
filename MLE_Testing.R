library(ltm)

data.ltm <- npi[1:100,]
trial.ltm <- ltm(data.ltm ~ z1, control = list(GHk = 100))
fscores <- factor.scores.ltm(trial.ltm)$score.dat

trialCat <- ltmCat(object = trial.ltm)
trialCat@answers <- as.numeric(as.vector(fscores[1,1:(dim(fscores)[2] - 4)]))

#catIrt
library(catIrt)
response <- trialCat@answers
parameters <- matrix(c(coef(trial.ltm)[,2], coef(trial.ltm)[,1], trialCat@guessing),
                 ncol=3, byrow = F)

wleEst(resp = response, params = parameters, range = c(-6,6), mod = "brm")$theta
# -0.5483
detach(package:catIrt)

#catR
library(catR)
it <- matrix(c(coef(trial.ltm)[,2], coef(trial.ltm)[,1], 
               rep(0,length(trialCat@discrimination)), 
               rep(1, length(trialCat@discrimination))),
            ncol= 4, byrow = F)

thetaEst(it = it, x = response, method = "WL", #D = 1,
         priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
# -0.5482474
detach(package:catR)

#catSurv
MLtest.bi <- function(cat) {
  
  W.bi <- function(cat, theta){
    a <- cat@difficulty
    b <- cat@discrimination
    c <- cat@guessing
  
    Pr <- c + (1 - c) * plogis(a + (b * theta))
    dPr <- (b * (1 - c) * exp(a + b * theta)) / (1 + exp(a + b * theta))^2
    d2Pr <- (b^2 * exp(a + b * theta)*(1 - exp(a + b * theta)) * (1 - c))/(1 + exp(a + b * theta))^3
    B <- sum((dPr * d2Pr) / (Pr * (1 - Pr)))
  
    W <- dLL(cat, theta, 0) + (B / (2 * sum(sapply(1:length(cat@answers), fisherInf, cat_df = cat, theta = theta))))
    return(W)
}
  
f <- function(theta) W.bi(cat, theta)
MLE <- uniroot(f, c(-10,10), extendInt = "yes")$root
return(MLE)
}
MLtest.bi(trialCat) # -0.5482474

estimates.bi.catSurv <- rep(NA,dim(fscores)[1])
estimates.bi.catR <- rep(NA,dim(fscores)[1])
for (i in 1:dim(fscores)[1]) {
  Cat.bi <- trialCat
  Cat.bi@answers <- as.numeric(as.vector(fscores[i,1:(dim(fscores)[2] - 4)]))
  estimates.bi.catSurv[i] <- MLtest.bi(Cat.bi)
  
  estimates.bi.catR[i] <- thetaEst(it = it, x = Cat.bi@answers, method = "WL",
           priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
}

comparison.bi <- data.frame(estimates.bi.catSurv, estimates.bi.catR)
comparison.bi$differences <- abs(comparison.bi[,1] - comparison.bi[,2])
colnames(comparison.bi) <- c("MLtest", "thetaEst", "differences")
comparison.bi
summary(comparison.bi)





############
#~~~~Poly~~~~
############

data.grm <- nfc[1:100,]
trial.grm <- grm(data.grm, control = list(GHk = 100))
fscores.grm <- factor.scores.grm(trial.grm)$score.dat

trialCat.grm <- grmCat(object = trial.grm)
trialCat.grm@answers <- as.numeric(as.vector(fscores.grm[1,1:(dim(fscores.grm)[2] - 4)]))

#catIrt
library(catIrt)
response.grm <- trialCat.grm@answers
parameters.grm <- matrix(c(coef(trial.grm)[,5], coef(trial.grm)[,1:4]),
                     ncol=5, byrow = F)

wleEst(resp = response.grm, params = parameters.grm, range = c(-6,6), mod = "grm")$theta
# 0.0471
detach(package:catIrt)

#catR
library(catR)
it.grm <- matrix(c(coef(trial.grm)[,5], coef(trial.grm)[,1:4]),
             ncol= 5, byrow = F)

thetaEst(it = it.grm, x = (response.grm - 1), model = "GRM", method = "WL", #D = 1,
         priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
# 0.04710347
detach(package:catR)


MLtest.poly <- function(cat) {
  
  W.poly <- function(cat, theta){
    a <- cat@difficulty
    b <- cat@discrimination
    c <- cat@guessing
    resp <- cat@answers
    
    B_j <- rep(NA, length(resp))
    for(j in 1:length(resp)){
      P_stars <- c(0, as.vector(unlist(probability(cat, theta, j))), 1)
      P_stars <- 1 - P_stars
      
      P <- P_stars[-1]
      for(k in 1:(length(P_stars) - 1)){
        P[k] <- P_stars[k] - P_stars[k+1]
      }
      
      dP_stars <- b[j] * P_stars * (1 - P_stars)
      
      d2P_stars <- b[j] * (dP_stars - (2 * P_stars * dP_stars))
      
      dP <- dP_stars[-1]
      for(k in 1:(length(dP_stars) - 1)){
        dP[k] <- dP_stars[k] - dP_stars[k+1]
        }
      
      d2P <- d2P_stars[-1]
      for(k in 1:(length(d2P_stars) - 1)){
       d2P[k] <- d2P_stars[k] - d2P_stars[k+1]
      }
      
      B_j[j] <- sum((dP * d2P)/P)
    }
    
    B <- sum(B_j)
    
    W <- dLL(cat, theta, 0) + (B / (2 * sum(sapply(1:length(cat@answers), fisherInf, cat_df = cat, theta = theta))))
    return(W)
  }
  
  f <- function(theta) W.poly(cat, theta)
  MLE <- uniroot(f, c(-4,4), extendInt = "yes")$root
  return(MLE)
}
MLtest.poly(trialCat.grm)  
# 0.04692087

estimates.poly.catSurv <- rep(NA,dim(fscores.grm)[1])
estimates.poly.catR <- rep(NA,dim(fscores.grm)[1])
for (i in 1:dim(fscores.grm)[1]) {
  Cat.poly <- trialCat.grm
  Cat.poly@answers <- as.numeric(as.vector(fscores.grm[i,1:(dim(fscores.grm)[2] - 4)]))
  estimates.poly.catSurv[i] <- MLtest.poly(Cat.poly)
  
  estimates.poly.catR[i] <- thetaEst(it = it.grm, x = (Cat.poly@answers - 1), model = "GRM", method = "WL",
                                   priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
}

comparison.poly <- data.frame(estimates.poly.catSurv, estimates.poly.catR)
comparison.poly$differences <- abs(comparison.poly[,1] - comparison.poly[,2])
colnames(comparison.poly) <- c("MLtest", "thetaEst", "differences")
comparison.poly
summary(comparison.poly)





