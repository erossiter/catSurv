## Load libraries and set working directory
library(devtools)
library(roxygen2)
library(Rcpp)
library(testthat)
setwd("/Users/iramalis/Desktop/gitstuff/CATSurv")
setwd("/Users/erinrossiter/Dropbox/Spring2016/Rclass/CATsurv")
setwd("~/Dropbox/2016Spring_Programming/CATSurv")
setwd("~/Dropbox/CATSurv")

## Loading the package
current.code <- as.package("catSurv")
load_all(current.code)
document(current.code)

## new things 
icc(testCats[[6]], theta_range = seq(-3,3,.1), 1)
showCppCat(testCats[[6]])

##binary
probability(testCats[[1]], 1, 1)
probability_test(testCats[[1]], 1, 1)
##poly
probability(testCats[[5]], 1, 1)
probability_test(testCats[[5]], 1, 1)

##binary
likelihood(testCats[[4]], 1)
likelihood_test(testCats[[4]], 1)
##poly
likelihood(testCats[[6]], 2)
likelihood_test(testCats[[6]], 2)


## Making Cats with data in package
library(ltm) ## need to add as dependency
data("npi") ## binary
# Getting warning message that Hession matrix is not positive definite... should we supress this?
cat_binary <- ltmCat(data = npi[1:4000, ])

# Does grmCat work only if each question has the same number of possible answers??
data("nfc") ## categorical
cat_poly <- grmCat(data = nfc)








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


?Cat
?setters
?setDiscrimination



## Checking the "set" methods.
test_cat <- new("Cat")

setGuessing(test_cat) <- 5
test_cat@guessing

setGuessing(test_cat, valid=F) <-1

setDiscrimination(test_cat) <- 5
test_cat@Discrimination

validObject(test_cat)


setDifficulty(test_cat)<-c(0,1,2,3)





test_dir("/Users/iramalis/Desktop/gitstuff/CATSurv/catSurv/tests/testthat/probs")


?probability
?likelihood
warnings()
?estimateSE
?estimateTheta
