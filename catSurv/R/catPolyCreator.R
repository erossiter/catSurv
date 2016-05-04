#' Randomly created polytomous Cat objects for testing
#'
#' This function creates a user-specified number of objects of class \code{Cat}, with randomly drawn values
#'
#' @param numCats The number of different objects of class \code{Cat} this function should return
#' @param seed The seed to set for randomization. Default to 8888.
#' @param spread The spread factor for the difficulty, theta, and discrimination values. Default to 2, which is multiplied by rnorm() outputs to provide values falling mostly within a range of (-4,4) 
#' @param maxGuessing The maximum value for the guessing parameter, which is multiplied by runif() for each question. Default to 0.1.
#' @param fillAnswers A numeric indicating what portion of each Cat's answers to fill with random values. The rest will be filled with NA's. Default to .5, meaning fill in a randomly drawn sample of half of the answer slots with random values. 
#'
#'@return A list of length \code{numCats} containing objects of class \code{Cat} with randomized values for components:
#' \itemize{
#' \item \code{difficulty} a list of vectors of difficulty parameters for each question/item: each vector corresponds to a question, and each value in the vector is the difficulty parameter for each response category
#' \item \code{guessing} a vector of guessing parameter for each question/item. 
#' \item \code{discrimination} a vector of disrimination parameter for each question/item.
#' \item \code{answers} a vector of answers to questions as given by the survey respondent. Default to a vector of half NA values and half filled values (randomly chosen) whose length is the number of questions (same length as difficulty, guessing, and discrimination)
#' Filled values correspond to response categories. Response categories range from 1 to (k+1), where k is the number of difficulty parameters for a given question item.
#' }
#' and prototype (default) values for components:
#' \itemize{
#' \item \code{priorName} a character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{normal} for the normal distirbution, \code{cauchy} for the Cauchy distribution, are \code{t} for the t-distribution. Defaults to \code{normal}. 
#' \item \code{priorParams} a numeric vector of parameters for the distribution specified in the \code{priorName} slot. See the details section for more information.  Defaults to \code{c(0,1)}.   
#' \item \code{poly} a boolean indicating the type of questions contained in this \code{Cat} object. Default is TRUE, indicating polytomous questions.
#' }
#' @note  
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{Cat-class.R}}
#' @rdname catPolyCreator
#' @export
setGeneric("catPolyCreator", function(numCats="numeric", seed=8888, spread=2, maxGuessing=.1, fillAnswers=.5, ...){standardGeneric("catPolyCreator")})

#' @export
setMethod(f="catPolyCreator", signature="numeric", 
          definition=function(numCats, seed, spread, maxGuessing, fillAnswers, ...){
            set.seed(seed)
            allTheCats<-c()
            for(i in 1:numCats){
              numQuestions<-3+floor(abs(50*(rnorm(1))))
              newCat<-new("Cat",
                          discrimination=abs((spread*rnorm(numQuestions))),
                          difficulty=lapply(1:numQuestions, function(x){
                            ## questions can have anywhere from 1 to 10 possible answers...
                            ## spread sets the probable range for the difficulty values (default to ~(-4, 4))
                            ## sorting to ensure that difficulty values for a given question item are increasing
                            return(sort(spread*rnorm(sample(10, 1, replace=T))))}) , 
                          guessing=maxGuessing*runif(numQuestions),
                          poly=T,
                          answers=rep(NA, numQuestions))
              ## randomly selecting the question items that will be filled with answers
              toBeFilled<-sample(length(newCat@answers), floor(fillAnswers*length(newCat@answers)), replace=F)
              ## filling those selected questions with a response value, which is randomly drawn from (the number of difficulty parameters + 1)
              newCat@answers[toBeFilled]<-as.numeric(sapply(newCat@difficulty[toBeFilled], function(x){
                  return(sample(length(x)+1, 1))
                }))
                  
              allTheCats<-c(allTheCats, newCat)
              
            }
            return(as.list(allTheCats))})
