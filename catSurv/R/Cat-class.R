#' A Computerized Adaptive Testing Survey (catSurv) Object
#'
#' Objects of class \code{Cat} are used in administering Computerized Adaptive Testing (CAT) Surveys.  These objects contain several pieces of information relevent for CAT surveys, and are used as input in the main functions of the \code{catSurv} package, \code{\link{nextItem}} and \code{\link{storeAnswer}}. They are created using the \code{initialize} function.
#'
#' An object of the class `Cat' has the following slots:
#' \itemize{
#' \item \code{guessing} A named vector of guessing parameters
#' \item \code{discrimination} A named vector of disrimination parameters including a numeric value per question/item.
#' \item \code{answers} A named vector of answers to questions as given by the survey respondent.
#' \item \code{priorName} A character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{normal} for the normal distirbution, \code{cauchy} for the Cauchy distribution, are \code{t} for the t-distribution. Defaults to \code{normal}.
#' \item \code{priorParams} A numeric vector of parameters for the distribution specified in the \code{priorName} slot. See the details section for more infomration.  Defaults to \code{c(1,1)}.
#' \item \code{lowerBound} The lower bound of the interval of the latent scale used in estimation. The default value is \code{-4.5}.
#' \item \code{upperBound} The upper bound of the interval of the latent scale used in estimation. The default value is \code{4.5}.
#' \item \code{quadPoints} Desired number of points to be used in approximating integral. The default value is \code{43}.
#' \item \code{difficulty} A named vector consisting of difficulty parameter for each item.
#' \item \code{X}
#' \item \code{Theta.est} A scalar value containing an estimate of a respondent's position on the latent trait.  Generally, this is the output of the \code{\link{estimateTheta}} funciton.
#' \item \code{poly} A logical containing the type of answers. The default is set for questions with dichotomous answers.
#' }
#'
#'@details When priorName is set to "normal", the first priorParam is the mean, the second is the standard deviation.  When priorName is set to "Cauchy", the first priorParam is the location, and the second is the scale.  When priorName is set to "t", the first priorParam is mu, a location parameter, the second is sigma, a scale parameter, and the third is nu, the degrees of freedom parameter.
#'
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{nextItem}}
#' @seealso \code{\link{storeAnswer}}
#' @aliases Cat-class initialize,Cat-method
#' @rdname Cat
#' @useDynLib catSurv
#' @importFrom Rcpp sourceCpp
#' @export

class.name = "Cat"
setClassUnion("logicalORnumeric", c("numeric","logical"))
setClassUnion("numericORlist", c("numeric","list"))

setClass("Cat",
         slots=list(
           guessing="numeric",
           discrimination="numeric",
           answers="logicalORnumeric",
           priorName="character",
           priorParams="numeric",
           lowerBound="numeric",
           upperBound="numeric",
           quadPoints="numeric",
           X="numeric",
           Theta.est="numeric",
           difficulty="numericORlist",
           poly="logical",
           integration="character",
           estimation="character",
           selection="character",
           coverage="numeric",
           points="numeric"
         ),
         prototype=prototype(
           priorName="normal",
           priorParams=c(0,1),
           lowerBound=-4.5,
           upperBound=4.5,
           quadPoints=43,
           poly=FALSE,
           integration="trapezoid",
           estimation="EAP",
           selection="EPV",
           coverage=0.9,
           points=40,
           answers=NA
         )
)

# 
# setValidity("Cat", function(object){
#   # guessing, discrimination, answers, poly, difficulty should all be same length
#   test1<-(length(object@discrimination)==length(object@guessing))  
#   if(!test1){return("discrimination and guessing not same length")}
#   
#   test2<-(length(object@discrimination)==length(object@answers))  
#   if(!test2){return("discrimination and answers not same length")}
#   
#   test3<-(length(object@discrimination)==length(object@poly))  
#   if(!test3){return("discrimination and poly not same length")}
#   
#   test4<-(length(object@discrimination)==length(object@difficulty))  
#   if(!test4){return("discrimination and difficulty not same length")}
#   
#   ## TEST THAT DIFFICULTY VALUES ARE STRICTLY INCREASING
#   if(any(object@poly==T)){
#     for(i in object@difficulty){
#       if (is.list(i)){
#         i<-unlist(i)
#       }
#       sorted<-sort(i)
#       uniques<-unique(i)
#       test5<-(isTRUE(all.equal(i,uniques)))
#       if(!test5){return(paste("Repeated difficulty values for question ", which(object@difficulty==i, arr.ind=T)))}
#       test6<-(isTRUE(all.equal(i,sorted)))
#       if(!test6){return(paste("Diffulty values for question ", which(object@difficulty==i, arr.ind=T), " are not increasing"))}
#     }
#   }
#   
# })

#' @export
setMethod("initialize", class.name, function(.Object, ...) {
  value = callNextMethod()
  value@X <- seq(from=value@lowerBound,to=value@upperBound,length=value@quadPoints)
  validObject(value)
  return(value)
})
