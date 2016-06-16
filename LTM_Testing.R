library(ltm)

# binary
data.bi <- npi[1:100,]

###### ltm
trial.l <- ltm(data.bi ~ z1, control = list(GHk = 100))
ltm.test <- factor.scores.ltm(trial.l, method = "EAP")$score.dat

# ltmCat 
Cat.l <- ltmCat(data.bi)

estimates.l <- rep(NA,dim(ltm.test)[1])
for (i in 1:dim(ltm.test)[1]) {
  trialCat.l <- Cat.l
  trialCat.l@answers <- as.numeric(as.vector(ltm.test[i,1:40]))
  estimates.l[i] <- estimateTheta(trialCat.l)
}

comparison.l <- data.frame(estimates.l, ltm.test[1:length(estimates.l),43])
comparison.l$differences <- abs(comparison.l[,1] - comparison.l[,2])
colnames(comparison.l) <- c("estimateTheta", "factor.scores.ltm", "differences")
comparison.l
summary(comparison.l)

comparison.l[which(comparison.l$differences > 0.1),]


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

comparison.t[which(comparison.t$differences > 1),]

#polytomous
data.poly <- nfc[1:100,]
trial.poly <- grm(data.poly)
grm.test <- factor.scores.grm(trial.poly, method = "EAP")$score.dat

trialCat.poly <- grmCat(data = data.poly)
trialCat.poly@guessing <- rep(0,length(trialCat.poly@answers))
trialCat.poly@priorParams <- c(0,9999999999)

estimates.poly <- rep(NA,dim(grm.test)[1])
for (i in 1:dim(grm.test)[1]) {
  trialCat.poly <- trialCat.poly
  trialCat.poly@answers <- as.numeric(as.vector(grm.test[i,1:18]))
  estimates.poly[i] <- estimateTheta(trialCat.poly)
}

comparison.poly <- data.frame(estimates.poly, grm.test[1:length(estimates.poly),21])
comparison.poly$differences <- abs(comparison.poly[,1] - comparison.poly[,2])
colnames(comparison.poly) <- c("estimateTheta", "factor.scores", "differences")
comparison.poly
summary(comparison.poly)

comparison.poly[which(comparison.poly$differences > 1),]
