library(ltm)
library(catR)
library(microbenchmark)

########## binary
data.bi <- npi[1:10,]

##### ltm
trial.l <- ltm(data.bi ~ z1, control = list(GHk = 100))
ltm.test <- factor.scores.ltm(trial.l, method = "EAP")$score.dat

## ltmCat 

#Cat.l <- ltmCat(data.bi)
Cat.l <- ltmCat(object = trial.l)


estimates.l <- rep(NA,dim(ltm.test)[1])
for (i in 1:dim(ltm.test)[1]) {
  trialCat.l <- Cat.l
  trialCat.l@answers <- as.numeric(as.vector(ltm.test[i,1:40]))
  estimates.l[i] <- estimateTheta(trialCat.l)
}

comparison.ltm <- data.frame(estimates.l, ltm.test[,"z1"])
comparison.ltm$differences <- abs(comparison.ltm[,1] - comparison.ltm[,2])
colnames(comparison.ltm) <- c("estimateTheta", "factor.scores.ltm", "differences")
comparison.ltm
summary(comparison.ltm)


## Cat probability test
trialCat <- Cat.l
trialCat@answers <- as.numeric(as.vector(ltm.test[1,1:40]))

Catprobs <- rep(NA, length(trialCat@answers))
for (i in 1:length(trialCat@answers)) {
  Catprobs[i] <- probability(trialCat, 0, i)
}

Catprobs <- as.numeric(unlist(Catprobs))
Catprobs

Catlik <- likelihood(trialCat, 0) 
Catlik

estimateTheta(trialCat)
estimateTheta_test(trialCat)

## ltm probability test
betas <- trial.l$coef
Z <- trial.l$GH$Z
Z[,2] <- rep(0, dim(Z)[1])

probs <-
  function (x) {
    pr <- plogis(x)
    if (any(ind <- pr == 1))
      pr[ind] <- 1 - sqrt(.Machine$double.eps)
    if (any(ind <- pr == 0))
      pr[ind] <- sqrt(.Machine$double.eps)
    pr
  }

pr <- probs(Z %*% t(betas))
pr[1,]

fits <- fitted(trial.l, resp.patterns = NULL)
X <- fits[, -ncol(fits), drop = FALSE]
mX <- 1 - X
if (any(na.ind <- is.na(X)))
  X[na.ind] <- mX[na.ind] <- 0
p.xz <- exp(X %*% t(log(pr)) + mX %*% t(log(1 - pr)))
p.xz[1,1]



##### catR - thetaEst

# note that the discrimination and difference parameters are taken from coef() of an ltm object
params <- matrix(c(coef(trial.l)[,2], coef(trial.l)[,1], 
                   rep(0,length(Cat.l@discrimination)), 
                   rep(1, length(Cat.l@discrimination))),
                 ncol= 4, byrow = F)

estimates.catR.ltm <- rep(NA, dim(ltm.test)[1])
for (i in 1:dim(ltm.test)[1]){
estimates.catR.ltm[i] <- thetaEst(it = params,
                     x = as.numeric(as.vector(ltm.test[i,1:40])),
                     model = NULL,
                     #D = 1,
                     method = "EAP",
                     priorDist = "norm",
                     priorPar = c(0,1),
                     parInt = c(-6,6,43),
                    #constantPatt = "EAP",
                     current.th = 0)
}

comparison.catR.ltm <- data.frame(estimates.l, estimates.catR)
comparison.catR$differences <- abs(comparison.catR.ltm[,1] - comparison.catR.ltm[,2])
colnames(comparison.catR.ltm) <- c("estimateTheta", "thetaEst", "differences")
comparison.catR.ltm
summary(comparison.catR.ltm)



## catR - Probability / Likelihood test
Pi(0, params)$Pi

L<-function(theta,parametermatrix,answervector) prod(Pi(th,it,D=1)$Pi^x*(1-Pi(th,it,D=1)$Pi)^(1-x))
L(0,params,Cat.test@answers)






##### tpm
data.tpm <- AMTknowledge[1:100,]
trial.t <- tpm(data.tpm, control = list(GHk = 100))
tpm.test <- factor.scores.tpm.correct(trial.t, method="EAP")$score.dat

## tpmCat
#Cat.t <- tpmCat(data.tpm)
Cat.t <- tpmCat(object = trial.t)

estimates.t <- rep(NA,dim(tpm.test)[1])
for (i in 1:dim(tpm.test)[1]) {
  trialCat.t <- Cat.t
  trialCat.t@answers <- as.numeric(as.vector(tpm.test[i,1:length(Cat.t@answers) - 1]))
  estimates.t[i] <- estimateTheta(trialCat.t)
}

comparison.t <- data.frame(estimates.t, tpm.test[1:length(estimates.t),ncol(tpm.test) - 1])
comparison.t$differences <- abs(comparison.t[,1] - comparison.t[,2])
colnames(comparison.t) <- c("estimateTheta", "factor.scores.tpm", "differences")
comparison.t
summary(comparison.t)

which(comparison.t[,3] > 0.1)



########## polytomous
data.poly <- nfc[1:100,]

##### grm
trial.g <- grm(data.poly, control=list(GHk = 100))
grm.test <- factor.scores.grm(trial.g, method = "EAP")$score.dat

#Cat.g <- grmCat(data = data.poly)
Cat.g <- grmCat(object = trial.g)
Cat.g@answers <- as.numeric(as.vector(grm.test[1,1:length(Cat.g@answers)]))

estimates.g <- rep(NA,dim(grm.test)[1])
for (i in 1:dim(grm.test)[1]) {
  trialCat.g <- Cat.g
  trialCat.g@answers <- as.numeric(as.vector(grm.test[i,1:18]))
  estimates.g[i] <- estimateTheta(trialCat.g)
}

comparison.grm <- data.frame(estimates.g, grm.test[,"z1"])
comparison.grm$differences <- abs(comparison.grm[,1] - comparison.grm[,2])
colnames(comparison.grm) <- c("estimateTheta", "factor.scores.grm", "differences")
comparison.grm
summary(comparison.grm)

## catR - thetaEst - grm
estimates.catR.grm <- rep(NA, dim(ltm.test)[1])
for (i in 1:dim(grm.test)[1]){
  estimates.catR.grm[i] <- thetaEst(it = coef(trial.g)[,c(5,1:4)], # discrimination must be the first column
                                x = as.numeric(as.vector(grm.test[i,1:length(Cat.g@answers)])) - 1,
                                # note that 1 is subtracted from above answers for function to process correctly
                                model = "GRM",
                                method = "EAP",
                                priorDist = "norm",
                                priorPar = c(0,1),
                                parInt = c(-6,6,43),
                                constantPatt = "EAP",
                                current.th = 0)
}

comparison.catR.grm <- data.frame(estimates.l, estimates.catR)
comparison.catR.grm$differences <- abs(comparison.catR.grm[,1] - comparison.catR.grm[,2])
colnames(comparison.catR.grm) <- c("estimateTheta", "thetaEst", "differences")
comparison.catR.grm
summary(comparison.catR.grm)




microbenchmark(
  estimateTheta(Cat.g),
  thetaEst(it = coef(trial.g)[,c(5,1:4)], # discrimination must be the first column
           x = as.numeric(as.vector(grm.test[1,1:length(Cat.g@answers)])) - 1,
           # note that 1 is subtracted from above answers for function to process correctly
           model = "GRM",
           method = "EAP",
           priorDist = "norm",
           priorPar = c(0,1),
           parInt = c(-6,6,43),
           constantPatt = "EAP",
           current.th = 0),
  times = 1000
)
