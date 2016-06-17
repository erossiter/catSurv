## Checking estimateSE against the "ltm" and "CatR" packages
data("npi")
library(ltm)
install.packages("catR")
library(catR)

#### TESTING AGAINST LTM ####
binary_data <- npi[1:1000, ]
## creating each object
trial.l <- ltm(binary_data ~ z1, control = list(GHk = 100))
trial.c <- ltmCat(binary_data)

## checking to make sure they have the same parameters
trial.c@difficulty == trial.l$coefficients[,1]
trial.c@discrimination == trial.l$coefficients[,2]

## checking the standard errors for each 
ltm.se <- factor.scores.ltm(trial.l, method = "EAP")$score.dat[,"se.z1"]

cat.se <- rep(NA, nrow(data.bi))
for(i in 1:nrow(data.bi)){
  trial.c@answers <- c(as.numeric(npi[i, ]))
  cat.se[i] <- estimateSE(trial.c)
}

differences <- cat.se - ltm.se
summary(differences)



#### TESTING AGAINST catR ####

