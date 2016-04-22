#' Randomly created binary Cat objects for testing
#'
#' This function creates a user-specified number of objects of class \code{Cat}, with randomly drawn values
#'
#' @param numCats The number of different objects of class \code{Cat} this function should return
#' @param seed The seed to set for randomization. Default to 9999.
#' @param spread The spread factor the difficulty, theta, and discrimination values. Default to 2, which is multiplied by rnorm() outputs to provide values falling mostly within a range of (-4,4) 
#' @param maxGuessing The maximum value for the guessing parameter, which is multiplied by runif() for each question. Default to 0.1.
#'
#' @return A list of length \code{numCats} containing objects of class \code{Cat} with components,
#' \itemize{
#' \item \code{difficulty} a vector of difficulty parameters for each question/item.
#' \item \code{guessing} a vector of guessing parameter for each question/item. 
#' \item \code{discrimination} a vector of disrimination parameter for each question/item.
#' \item \code{answers} a vector of answers to questions as given by the survey respondent.
#' \item \code{priorName} a character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{normal} for the normal distirbution, \code{cauchy} for the Cauchy distribution, are \code{t} for the t-distribution. Defaults to \code{normal}. 
#' \item \code{priorParams} a numeric vector of parameters for the distribution specified in the \code{priorName} slot. See the details section for more information.  Defaults to \code{c(0,1)}.   
#' }
#' @note  
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{Cat-class.R}}
#' @rdname catBiCreator
#' @export
setGeneric("catBiCreator", function(numCats="numeric", seed=9999, spread=2, maxGuessing=.1, ...){standardGeneric("catBiCreator")})

#' @export
setMethod(f="catBiCreator", signature="numeric", 
          definition=function(numCats, seed, spread, maxGuessing,...){
            set.seed(seed)
            allTheCats<-c()
            for(i in 1:numCats){
              numQuestions<-floor(abs(50*(rnorm(1))))
              newCat<-new("Cat",
                          discrimination=(spread*rnorm(numQuestions)),
                          difficulty=(spread*rnorm(numQuestions)),
                          guessing=maxGuessing*runif(numQuestions),
                          poly=F,
                          answers=rep(NA, numQuestions))
              
              allTheCats<-c(allTheCats, newCat)
              
            }
            return(as.list(allTheCats))})