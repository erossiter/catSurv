## libraries for package maintenece
rm(list = ls())
library(devtools)
library(roxygen2)
#setwd("~/Github/CATsurv")
setwd("~/Dropbox/Spring2016/Rclass/CATSurv/")

## loading the package
current.code <- as.package("catSurv")
load_all(current.code)#, recompile = FALSE)
document(current.code)
#test(current.code)
#check(current.code)

## for looking at pdf of documentation
unlink("catSurv.pdf") ## deleting current version
path <- find.package("catSurv")
system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))


# ## political knowledge data set for gpcm
# load("TAPSdata2013.R")
# dataset <- dataset[ ,grep("polknow", colnames(dataset), ignore.case = T)]
# sapply(dataset, levels)
# for(i in 1:10){
#   dataset[,i]<-as.numeric(dataset[,i])
# }
# for(i in 1:10){
#   dataset[dataset[,i]==1&!is.na(dataset[,i]),i]<-NA
# }
# for(i in 1:10){
#   dataset[,i]<-dataset[,i]-1
# }
# 
# colnames(dataset) <- paste0("Q", 1:10)
# ## taking out observations where all questions are NA
# dataset2 <- dataset[-which(apply(dataset, 1, function(r) sum(is.na(r))==10)), ]
# ## better rownames
# rownames(dataset2) <- 1:nrow(dataset2)
# ## better name
# # polknowTAPS <- dataset2
# # ## saving dataset to package
# # use_data(polknowTAPS, pkg = "catSurv", overwrite = FALSE)
# # ## try it out
# # data(polknowTAPS)

data("polknowTAPS")
gpcmModel <- gpcm(data = polknowTAPS, constraint="gpcm", control=list("iter.qN"=200, "GHk"=50), IRT.param=FALSE)

gpcm_cat <- gpcmCat(gpcmModel)
gpcm_cat@difficulty[[1]]


summary(gpcm_cat)
?gpcm


test<-gpcm(data=polknowTAPS, constraint="gpcm", control=list("iter.qN"=200))
xx <- gpcmCat(test)





## loading objects for the purposes of creating tests
load("catSurv/tests/testthat/cat_objects.Rdata")

## checking what version of packages I have installed to update
## DESCRIPTION when needed (comments are what's currently 
## in description file)
library(utils)
version #3.3.2
packageVersion("RcppArmadillo") #‘0.7.700.0.0’
packageVersion("stats") #‘3.3.2’
packageVersion("Rcpp") #‘0.12.9’
packageVersion("RcppGSL") #‘0.3.1’
packageVersion("BH") #‘1.60.0.2’
packageVersion("ltm") #‘1.0.0’
packageVersion("catR") #‘3.10’
packageVersion("catIrt") #‘0.5.0’
packageVersion("testthat") #‘1.0.2’
packageVersion("methods") #‘3.3.2’


