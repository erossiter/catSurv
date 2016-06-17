library(ltm)
library(catR)

# binary
data.bi <- npi[1:1000,]

###### ltm
trial.l <- ltm(data.bi ~ z1, control = list(GHk = 100))
ltm.test <- factor.scores.ltm(trial.l, method = "EAP")$score.dat

# ltmCat 

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


# Cat probability test
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

# ltm probability test
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





###### tpm
trial.t <- tpm(data.bi, control = list(GHk = 100))
tpm.test <- factor.scores.tpm.correct(trial.t, method="EAP")$score.dat

# tpmCat
Cat.t <- tpmCat(data.bi)

estimates.t <- rep(NA,dim(tpm.test)[1])
for (i in 1:dim(tpm.test)[1]) {
  trialCat.t <- Cat.t
  trialCat.t@answers <- as.numeric(as.vector(tpm.test[i,1:dim(tpm.test)[1]]))
  estimates.t[i] <- estimateTheta_test(trialCat.t)
}

comparison.t <- data.frame(estimates.t, tpm.test[1:length(estimates.t),43])
comparison.t$differences <- abs(comparison.t[,1] - comparison.t[,2])
colnames(comparison.t) <- c("estimateTheta", "factor.scores.tpm", "differences")
comparison.t
summary(comparison.t)





#polytomous
data.poly <- nfc[1:100,]

### grm
trial.g <- grm(data.poly, control=list(GHk = 100))
grm.test <- factor.scores.grm(trial.poly, method = "EAP")$score.dat

#Cat.g <- grmCat(data = data.poly)
Cat.g <- grmCat(object = trial.g)

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

