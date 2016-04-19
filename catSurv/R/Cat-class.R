#' @useDynLib catSurv
#' @importFrom Rcpp sourceCpp
NULL

class.name = "Cat"
setClassUnion("numericORlist", c("numeric","list"))
setClassUnion("logicalORnumeric", c("numeric","logical"))




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
#' @export
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


#' @export
setMethod("initialize", class.name, function(.Object, ...) {
  value = callNextMethod()
  value@X <- seq(from=value@lowerBound,to=value@upperBound,length=value@quadPoints)
  validObject(value)
  return(value)
})


setGeneric("setguessing<-",
		   function(object, value){
		   	standardGeneric("setguessing<-")
		   	})
setReplaceMethod(
	f = "setguessing",
	signature = "Cat",
	definition = function(object, value){
		object@guessing <- value
		validObject(object)
		return(object)
		})


setGeneric("setdiscrimination<-",
		   function(object, value){
		   	standardGeneric("setdiscrimination<-")
		   	})
setReplaceMethod(
	f = "setdiscrimination",
	signature = "Cat",
	definition = function(object, value){
		object@discrimination <- value
		validObject(object)
		return(object)
		})


setGeneric("setpriorParam<-",
		   function(object, value){
		   	standardGeneric("setpriorParam<-")
		   	})
setReplaceMethod(
	f = "setpriorParam",
	signature = "Cat",
	definition = function(object, value){
		object@priorParam <- value
		validObject(object)
		return(object)
		})


setGeneric("setpriorName<-",
		   function(object, value){
		   	standardGeneric("setpriorName<-")
		   	}) 
setReplaceMethod(
	f = "setpriorName",
	signature = "Cat",
	definition = function(object, value){
		object@priorName <- value
		validObject(object)
		return(object)
		})


setGeneric("setlowerBound<-",
		   function(object, value){
		   	standardGeneric("setlowerBound<-")
		   	})
setReplaceMethod(
	f = "setlowerBound",
	signature = "Cat",
	definition = function(object, value){
		object@lowerBound <- value
		validObject(object)
		return(object)
		})


setGeneric("setupperBound<-",
		   function(object, value){
		   	standardGeneric("setupperBound<-")
		   	})
setReplaceMethod(
	f = "setupperBound",
	signature = "Cat",
	definition = function(object, value){
		object@upperBound <- value
		validObject(object)
		return(object)
		})


setGeneric("setquadPoints<-",
		   function(object, value){
		   	standardGeneric("setquadPoints<-")
		   	})
setReplaceMethod(
	f = "setquadPoints",
	signature = "Cat",
	definition = function(object, value){
		object@quadPoints <- value
		validObject(object)
		return(object)
		})


setGeneric("setdifficulty<-",
		   function(object, value){
		   	standardGeneric("setdifficulty<-")
		   	})
setReplaceMethod(
	f = "setdifficulty",
	signature = "Cat",
	definition = function(object, value){
		object@difficulty <- value
		validObject(object)
		return(object)
		})


setGeneric("setpoly<-",
		   function(object, value){
		   	standardGeneric("setpoly<-")
		   	})
setReplaceMethod(
	f = "setpoly",
	signature = "Cat",
	definition = function(object, value){
		object@poly <- value
		validObject(object)
		return(object)
		})


setGeneric("setintegration<-",
		   function(object, value){
		   	standardGeneric("setintegration<-")
		   	})
setReplaceMethod(
	f = "setintegration",
	signature = "Cat",
	definition = function(object, value){
		object@integration <- value
		validObject(object)
		return(object)
		})


setGeneric("setestimation<-",
		   function(object, value){
		   	standardGeneric("setestimation<-")
		   	})
setReplaceMethod(
	f = "setestimation",
	signature = "Cat",
	definition = function(object, value){
		object@estimation <- value
		validObject(object)
		return(object)
		})


setGeneric("setselection<-",
		   function(object, value){
		   	standardGeneric("setselection<-")
		   	})
setReplaceMethod(
	f = "setselection",
	signature = "Cat",
	definition = function(object, value){
		object@selection <- value
		validObject(object)
		return(object)
		})


setGeneric("setcoverage<-",
		   function(object, value){
		   	standardGeneric("setcoverage<-")
		   	})
setReplaceMethod(
	f = "setcoverage",
	signature = "Cat",
	definition = function(object, value){
		object@coverage <- value
		validObject(object)
		return(object)
		})


setGeneric("setpoints<-",
		   function(object, value){
		   	standardGeneric("setpoints<-")
		   	})
setReplaceMethod(
	f = "setpoints",
	signature = "Cat",
	definition = function(object, value){
		object@points <- value
		validObject(object)
		return(object)
		})

