#' Randomly created polytomous Cat objects for testing
#'
#' This function creates a user-specified number of objects of class \code{Cat}, with randomly drawn values
#'
#' @param numCats The number of different objects of class \code{Cat} this function should return
#' @param seed The seed to set for randomization. Default to 8888.
#' @param spread The spread factor the difficulty, theta, and discrimination values. Default to 2, which is multiplied by rnorm() outputs to provide values falling mostly within a range of (-4,4) 
#' @param maxGuessing The maximum value for the guessing parameter, which is multiplied by runif() for each question. Default to 0.1.
#'
#'@return A list of length \code{numCats} containing objects of class \code{Cat} with randomized values for components:
#' \itemize{
#' \item \code{difficulty} a vector of difficulty parameters for each question/item.
#' \item \code{guessing} a vector of guessing parameter for each question/item. 
#' \item \code{discrimination} a vector of disrimination parameter for each question/item.
#' and prototype (default) values for components:
#' \item \code{answers} a vector of answers to questions as given by the survey respondent. Default to a vector of NA values, whose length is the number of questions (same length as difficulty, guessing, and discrimination)
#' \item \code{priorName} a character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{normal} for the normal distirbution, \code{cauchy} for the Cauchy distribution, are \code{t} for the t-distribution. Defaults to \code{normal}. 
#' \item \code{priorParams} a numeric vector of parameters for the distribution specified in the \code{priorName} slot. See the details section for more information.  Defaults to \code{c(0,1)}.   
#' \item \code{poly} a boolean indicating the type of questions contained in this \code{Cat} object. Default is TRUE, indicating polytomous questions.
#' }
#' @note  
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{Cat-class.R}}
#' @rdname catPolyCreator
#' @export
setGeneric("catPolyCreator", function(numCats="numeric", seed=8888, spread=2, maxGuessing=.1, ...){standardGeneric("catPolyCreator")})

#' @export
setMethod(f="catPolyCreator", signature="numeric", 
          definition=function(numCats, seed, spread, maxGuessing,...){
            set.seed(seed)
            allTheCats<-c()
            for(i in 1:numCats){
              numQuestions<-3+floor(abs(50*(rnorm(1))))
              newCat<-new("Cat",
                          discrimination=(spread*rnorm(numQuestions)),
                          difficulty=lapply(1:numQuestions, function(x){
                            ## questions can have anywhere from 1 to 10 possible answers...
                            ## spread sets the probable range for the difficulty values (default to ~(-4, 4))
                            ## sorting to ensure that difficulty values for a given question item are increasing
                            return(sort(spread*rnorm(sample(10, 1, replace=T))))}) , 
                          guessing=maxGuessing*runif(numQuestions),
                          poly=T,
                          answers=rep(NA, numQuestions))
              
              allTheCats<-c(allTheCats, newCat)
              
            }
            return(as.list(allTheCats))})
