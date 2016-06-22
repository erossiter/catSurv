library(ltm)

##########################################
##########################################
# Binary Weighted MLE
#########################################
#########################################

# Subsets a manageable amount of binary data;
#    creates an ltm object and estimates its factor scores (thetas)
#    creates a cat with relevant parameters and defines answers
data.ltm <- npi[1:100,]

trial.ltm <- ltm(data.ltm ~ z1, control = list(GHk = 100))
fscores <- factor.scores.ltm(trial.ltm)$score.dat

trialCat <- ltmCat(object = trial.ltm)
trialCat@answers <- as.numeric(as.vector(fscores[1,1:(dim(fscores)[2] - 4)]))

##############################
# catIrt Estimate - Binary
#############################

library(catIrt)
parameters <- matrix(c(coef(trial.ltm)[,2], coef(trial.ltm)[,1], trialCat@guessing),
                 ncol=3, byrow = F)

wleEst(resp = trialCat@ answers, params = parameters, range = c(-6,6), mod = "brm")$theta
# result: -0.5483

detach(package:catIrt)

###########################
# catR Estimate - Binary
##########################

library(catR)

it <- matrix(c(coef(trial.ltm)[,2], coef(trial.ltm)[,1], 
               rep(0,length(trialCat@discrimination)), 
               rep(1, length(trialCat@discrimination))),
            ncol= 4, byrow = F)

thetaEst(it = it, x = trialCat@ answers, method = "WL", #D = 1,
         priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
# result: -0.5482691

detach(package:catR)


###################################################
# Creating a binary WLE function in R for catSurv <------------------  convert to C++
##################################################

WLtest.bi <- function(cat) {
  
# An arbitrary function to calculate W because
#    uniroot requires its input be a function
  W.bi <- function(cat, theta){
    a <- cat@difficulty
    b <- cat@discrimination
    c <- cat@guessing
  
    # Calculate probability + its first & second derivatives
    Pr <- c + (1 - c) * plogis(a + (b * theta))
    dPr <- (b * (1 - c) * exp(a + b * theta)) / (1 + exp(a + b * theta))^2
    d2Pr <- (b^2 * exp(a + b * theta)*(1 - exp(a + b * theta)) * (1 - c))/(1 + exp(a + b * theta))^3
    
    # Defines relevant components of W; calculates W
    B <- sum((dPr * d2Pr) / (Pr * (1 - Pr)))
    FisherSum <- sum(sapply(1:length(cat@answers), fisherInf, cat_df = cat, theta = theta))
    dLogLik <- dLL(cat, theta, 0)

    W <- dLogLik  + (B / (2 * FisherSum))
    return(W)
}

# uniroot requires a function in terms of the variable of interest
f <- function(theta) W.bi(cat, theta)

# uniroot is finicky with its range. 
#   Too large and it throws an error. 
#   Too small and it fails to find a root.
#   Setting a small range while passing extendInt = "yes" takes care of the problem.
#   It searches for roots outside the range if necessary.
#   Hopefully such maneuvering isn't necessary for the GSL equivalent
WLE <- uniroot(f, c(-4,4), extendInt = "yes")$root
return(WLE)
}
WLtest.bi(trialCat) # result: -0.5482474

###################################################
# Comparing estimates against catR 
##################################################

library(catR)

estimates.bi.catSurv <- rep(NA,dim(fscores)[1])
estimates.bi.catR <- rep(NA,dim(fscores)[1])
for (i in 1:dim(fscores)[1]) {
  Cat.bi <- trialCat
  Cat.bi@answers <- as.numeric(as.vector(fscores[i,1:(dim(fscores)[2] - 4)]))
  estimates.bi.catSurv[i] <- WLtest.bi(Cat.bi)
  
  estimates.bi.catR[i] <- thetaEst(it = it, x = Cat.bi@answers, method = "WL",
           priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
}

comparison.bi <- data.frame(estimates.bi.catSurv, estimates.bi.catR)
comparison.bi$differences <- abs(comparison.bi[,1] - comparison.bi[,2])
colnames(comparison.bi) <- c("WLtest", "thetaEst", "differences")
comparison.bi
summary(comparison.bi)

detach(package:catR)









##########################################
##########################################
# Categorical Weighted MLE
#########################################
#########################################

# Subsets a manageable amount of polytomous data;
#    creates a grm object and estimates its factor scores (thetas)
#    creates a cat with relevant parameters and defines answers

data.grm <- nfc[1:100,]
trial.grm <- grm(data.grm, control = list(GHk = 100))
fscores.grm <- factor.scores.grm(trial.grm)$score.dat

trialCat.grm <- grmCat(object = trial.grm)
trialCat.grm@answers <- as.numeric(as.vector(fscores.grm[1,1:(dim(fscores.grm)[2] - 4)]))

#################################
# catIrt Estimate - Categorical
################################

library(catIrt)

parameters.grm <- matrix(c(coef(trial.grm)[,5], coef(trial.grm)[,1:4]),
                     ncol=5, byrow = F)

wleEst(resp = trialCat.grm@answers, params = parameters.grm, range = c(-6,6), mod = "grm")$theta
# result: 0.0471

detach(package:catIrt)

#################################
# catR Estimate - Categorical
################################

library(catR)

it.grm <- matrix(c(coef(trial.grm)[,5], coef(trial.grm)[,1:4]),
             ncol= 5, byrow = F)

thetaEst(it = it.grm, x = (trialCat.grm@answers - 1), model = "GRM", method = "WL", #D = 1,
         priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
# result: 0.04710441

detach(package:catR)

#########################################################
# Creating a categorical WLE function in R for catSurv <------------------  convert to C++
########################################################

WLtest.poly <- function(cat) {

# An arbitrary function to calculate W because
#    uniroot requires its input be a function
  W.poly <- function(cat, theta){
    a <- cat@difficulty
    b <- cat@discrimination
    c <- cat@guessing
    resp <- cat@answers
    
    
    B_j <- rep(NA, length(resp))
    for(j in 1:length(resp)){
      
      # Create vector of each p* per question
      P_stars <- c(0, as.vector(unlist(probability(cat, theta, j))), 1)
      
      # Invert p* values such that they decrease from 1...0 and p* values are listed as 1-p*
      P_stars <- 1 - P_stars
      # This step was done to mirror catR's method, as
      # This requires ***UPDATING THE DOCUMENTATION***
      # However, because the p* value are inverted, the equations for dP and d2P
      #   remain correct in the current documentation.
      # I suspect that the function will still work if the original order is preserved, 
      #   so long as corrections are made to dP & d2P, 
      #   as well as to their equations in the documentation.
      
      # Calculate a probability estimate for each interval demarcated by P*
      P <- P_stars[-1]
      for(k in 1:(length(P_stars) - 1)){
        P[k] <- P_stars[k] - P_stars[k+1]
      }
      
      # First & second derivaties of P* 
      dP_stars <- b[j] * P_stars * (1 - P_stars)
      
      d2P_stars <- b[j] * (dP_stars - (2 * P_stars * dP_stars))
      # Note that the documentation currently shows -b as the leading coefficient
      #   for both of the above derivatives. This requires ***UPDATING THE DOCUMENTATION***
      
      # First & second derivatives  of P   
      dP <- dP_stars[-1]
      for(k in 1:(length(dP_stars) - 1)){
        dP[k] <- dP_stars[k] - dP_stars[k+1]
        }
      
      d2P <- d2P_stars[-1]
      for(k in 1:(length(d2P_stars) - 1)){
       d2P[k] <- d2P_stars[k] - d2P_stars[k+1]
      }
      
      # Sum for each question
      B_j[j] <- sum((dP * d2P)/P)
    }
    
    # Sum across all questions
    B <- sum(B_j)
    
    # Defines other relevant components of W; calculates W
    FisherSum <- sum(sapply(1:length(cat@answers), fisherInf, cat_df = cat, theta = theta))
    dLogLik <- dLL(cat, theta, 0)
    
    W <- dLogLik  + (B / (2 * FisherSum))
    return(W)
  }
  # uniroot requires a function in terms of the variable of interest
  f <- function(theta) W.poly(cat, theta)
  
  # uniroot is finicky with its range. 
  #   Too large and it throws an error. 
  #   Too small and it fails to find a root.
  #   Setting a small range while passing extendInt = "yes" takes care of the problem.
  #   It searches for roots outside the range if necessary.
  #   Hopefully such maneuvering isn't necessary for the GSL equivalent
  WLE <- uniroot(f, c(-4,4), extendInt = "yes")$root
  return(WLE)
}
WLtest.poly(trialCat.grm)  
# result: 0.04692087


###################################################
# Comparing estimates against catR 
##################################################

library(catR)

estimates.poly.catSurv <- rep(NA,dim(fscores.grm)[1])
estimates.poly.catR <- rep(NA,dim(fscores.grm)[1])
for (i in 1:dim(fscores.grm)[1]) {
  Cat.poly <- trialCat.grm
  Cat.poly@answers <- as.numeric(as.vector(fscores.grm[i,1:(dim(fscores.grm)[2] - 4)]))
  estimates.poly.catSurv[i] <- WLtest.poly(Cat.poly)
  
  estimates.poly.catR[i] <- thetaEst(it = it.grm, x = (Cat.poly@answers - 1), model = "GRM", method = "WL",
                                   priorDist = "norm", priorPar = c(0,1), range = c(-10,10))
}

comparison.poly <- data.frame(estimates.poly.catSurv, estimates.poly.catR)
comparison.poly$differences <- abs(comparison.poly[,1] - comparison.poly[,2])
colnames(comparison.poly) <- c("WLtest", "thetaEst", "differences")
comparison.poly
summary(comparison.poly)

detach(package:catR)





