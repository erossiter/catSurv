## Load libraries and set working directory
library(devtools)
library(roxygen2)
library(Rcpp)
library(testthat)
library(ltm)
setwd("/Users/iramalis/Desktop/gitstuff/CATSurv")
setwd("/Users/erinrossiter/Dropbox/Spring2016/Rclass/CATsurv")
setwd("~/Dropbox/2016Spring_Programming/CATSurv")
setwd("~/Dropbox/CATSurv")

## Loading the package
current.code <- as.package("catSurv")
load_all(current.code)
document(current.code)

data(npi)
binary_cat <- ltmCat(npi[1:100, ], 100)
binary_cat@priorName <- "NORMAL"
binary_cat@priorParams <- c(0,1)
binary_cat@lowerBound <- 0
binary_cat@upperBound <- 1
binary_cat@answers[1:35] <- unlist(npi[1,1:35])

binary_cat@estimation <- "WLE"
estimateTheta(binary_cat)
estimateSE(binary_cat)

binary_cat@estimation <- "MAP"
estimateTheta(binary_cat)
estimateSE(binary_cat)

data(nfc)
poly_cat <- grmCat(nfc[1:100, ], 100)
poly_cat@estimation <- "WLE"
poly_cat@lowerBound <- -5
poly_cat@upperBound <- 5

our_estimates <- rep(0, 100)
their_estimates <- rep(0, 100)
for(i in 1:100){
  poly_cat@answers[1:18] <- unlist(rep(NA, 18))
  poly_cat@answers[1:10] <- unlist(nfc[i,1:10])
  our_estimates[i] <- estimateTheta(poly_cat)
  their_estimates[i] <- WLtest.poly(poly_cat)
}

summary(abs(our_estimates - their_estimates))
which(abs(our_estimates - their_estimates) > 1)
poly_cat@answers[1:10] <- unlist(nfc[27,1:10])
showCppCat(poly_cat)



selectItem(binary_cat)

prior(x = 1, c = binary_cat@priorName, p = binary_cat@priorParams)



?checkStopRules


icc(poly_cat, question = 1)
iic(poly_cat, question = 1)




binary_cat@strata <- rep(c(1,2), 5)
binary_cat@selection <- "RANDOM"
selectItem(binary_cat)

binary_cat@lengthThreshold <- 5
binary_cat@seThreshold <- .5
binary_cat@infoThreshold <- 1
binary_cat@gainThreshold <- .3
binary_cat@gainOverride <- 1
binary_cat@lengthOverride <- 4

checkStopRules(binary_cat)
sum(!is.na(binary_cat@answers))




binary_cat@estimation <- "EAP"
estimateSE(binary_cat)

binary_cat@estimation <- "MLE"
estimateSE(binary_cat)
sqrt(1/fisherTestInfo(binary_cat))

binary_cat@estimation <- "MAP"
estimateSE(binary_cat)
sqrt(1/fisherTestInfo(binary_cat))




library(ltm)
data("npi")
binary_data <- npi[1:100,]
binary_cat <- ltmCat(binary_data)
binary_cat@answers <- rep(NA, length(binary_cat@guessing))
binary_cat@answers[1:5] <- unlist(binary_data[1,1:5])
binary_cat@selection <- "EPV"

showCppCat(binary_cat)

selectItem(binary_cat)
lookAhead(binary_cat, 11)
lookAhead(binary_cat, 1)


kl(binary_cat)

binary_cat@discrimination

## fix tests:
# - binary obsInf, CatR  -- DONE
# - poly obsInf, CatR -- DONE
# - poly fisherInf, CatR -- DONE
# - binary, obsInf, catIrt -- their package is annoying
# - poly, obsInf, catIrt?? -- their package is annoying
# - fisherInf, catIrt??



## I need to make MLEEstimator a child of MAPEstimator
## and then make the functions "virtual ... = 0" in MAP
## and then "... override" them in MLE.  I need to do this
## for dLL and d2LL and the while loop??



plot_likelihood <- function(theta){
  likelihood(cat, theta)
}

uniroot(plot_likelihood, c(-5,5))

plot_likelihood(-5)
plot_likelihood(-5)

plot_likelihood(0)
plot_likelihood(4)
plot_likelihood(-1)

plot_likelihood <- function(theta){
  dLL(cat, theta, FALSE)
}
abline(h=0)
abline(v=uniroot(plot_likelihood, c(-5, 5))$root)


plot(y=sapply(seq(-3,3,.1), plot_likelihood), x=seq(-3,3,.1), type = "l")

cbind(cat@answers, cat@discrimination)













## new things 
testPlot <- icc(testCats[[6]], theta_range = seq(-3,3,.1), 1)
showCppCat(testCats[[6]])



##binary
probability(testCats[[1]], -5, 5)
likelihood(testCats[[1]], -5)

probability_test(testCats[[2]], -1000, 2)
##poly
probability(testCats[[8]], 10, 1)
probability_test(testCats[[8]], 1, 1)

##binary
likelihood(testCats[[2]], 1)
likelihood_test(testCats[[2]], 1)

##poly
likelihood(testCats[[5]], 1)
likelihood_test(testCats[[7]], 1)



# cat_binary@answers <- unlist(c(npi[2,1:10], rep(NA, 10)))
# cat_binary@guessing <- rep(0, 40)
# cat_binary@poly <- FALSE
# likelihood(cat_binary, 1)
# likelihood_test(cat_binary, 1)

cat_poly@answers <- unlist(c(nfc[2,1:5], rep(NA, 18-5)))
cat_poly@guessing <- rep(0, length(cat_poly@discrimination))
likelihood(cat_poly,1)
likelihood_test(cat_poly,1)

## Making Cats with data in package
library(ltm) ## need to add as dependency
data("npi") ## binary
# Getting warning message that Hession matrix is not positive definite... should we supress this?
cat_binary <- ltmCat(data = npi[1:4000, ])

# Does grmCat work only if each question has the same number of possible answers??
data("nfc") ## categorical
cat_poly <- grmCat(data = nfc)

nfc <- NULL






## Checking some documentation; creating Cats to use
?prior

?catBiCreator
myFifteenBiCats<-catBiCreator(1, fillAnswers=.8)
myFifteenBiCats[[1]]

?catPolyCreator
myTwelvePolyCats<-catPolyCreator(12, fillAnswers=.2)
myTwelvePolyCats[[5]]@difficulty
myTwelvePolyCats[[5]]@answers

myUnansweredBiCat<-catBiCreator(1, fillAnswers=0)

test_cat <- new("Cat")
setdiscrimination(test_cat)<-c(0,3,.8)
setdiscrimination(test_cat, valid=F)<-c(0,3,.8)

probability(test_cat, 1, 1)




## Checking the "set" methods.
test_cat <- new("Cat")

setGuessing(test_cat) <- 5
test_cat@guessing

setGuessing(test_cat, valid=F) <-1

setDiscrimination(test_cat) <- 5
test_cat@Discrimination

validObject(test_cat)

setDifficulty(test_cat)<-c(0,1,2,3)

