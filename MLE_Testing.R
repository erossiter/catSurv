library(catIrt)
library(catR)
library(ltm)

data.ltm <- npi[1:100,]
trial.ltm <- ltm(data.ltm ~ z1, control = list(GHk = 100))
fscores <- factor.scores.ltm(trial.ltm)$score.dat

trialCat <- ltmCat(object = trial.ltm)
trialCat@answers <- as.numeric(as.vector(fscores[1,1:(dim(fscores)[2] - 4)]))

#catIrt
response <- trialCat@answers
parameters <- matrix(c(coef(trial.ltm)[,2], coef(trial.ltm)[,1], trialCat@guessing),
                 ncol=3, byrow = F)

mleEst(resp = response, params = parameters, range = c(-6,6), mod = "brm")$theta

#catR
it <- matrix(c(coef(trial.ltm)[,2], coef(trial.ltm)[,1], 
               rep(0,length(trialCat@discrimination)), 
               rep(1, length(trialCat@discrimination))),
            ncol= 4, byrow = F)

thetaEst(it = it, x = response, method = "ML", #D = 1,
         priorDist = "norm", priorPar = c(0,1),
         parInt = c(-6,6,43), current.th = 0)


#catSurv

uniroot()

B <- function(cat, theta){
  a <- cat@difficulty
  b <- cat@discrimination
  c <- cat@guessing
  
  Pr <- c + (1 - c) * plogis(a + (b * theta))
  dPr <- b * (1 - c) * exp(a + b * theta) / (1 + exp(a + b * theta))^2
  dPr2 <- b^2 * exp(a + b * theta)*(1 - exp(a + b * theta)) * ((1 - c)/(1 + exp(a + b * theta))^3)
  B <- sum((dPr * dPr2) / (Pr * (1 - Pr)))
  return(B)
}
B(testCats[[1]],0)
