#' @useDynLib catSurv
#' @importFrom Rcpp sourceCpp
#' @import methods
NULL

class.name = "Cat"
setClassUnion("logicalORnumeric", c("numeric","logical"))
setClassUnion("numericORlist", c("numeric","list"))

#' A Computerized Adaptive Testing Survey (catSurv) Object
#'
#' Creates an object of class \code{Cat}.  Cat objects are used in administering Computerized Adaptive Testing (CAT) Surveys.  These objects contain several pieces of information relevent for CAT surveys, and are used as input in the main functions of the \code{catSurv} package. 
#'
#' Assume we have a survey battery with \code{I} questions.  An object of the class \code{Cat} has the following slots:
#' \itemize{
#' \item \code{guessing} A vector of length \code{I} of guessing parameters.  Note: guessing parameters are only applicable for \code{Cat} objects fit with the \code{ltm} model, using the \code{ltmCat} function. 
#' \item \code{discrimination} A named vector of length \code{I} of disrimination parameters.
#' \item \code{difficulty} A named vector or list of length \code{I} of difficulty parameters. For binary \code{Cat} objects, the vector will contain difficulty parameters for each item.  For categorical \code{Cat} objects, a list will constain a vector for each item, and each vector will contain a difficulty parameter for each response option.  
#' \item \code{answers} A vector of length \code{I} of answers to questions as given by the survey respondent.  Unanswered questions have the value \code{NA}.
#' \item \code{priorName} A character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{NORMAL} for the normal distirbution, \code{STUDENT_T} for the student's t distribution, and \code{UNIFORM} for the uniform distribution.  The default value is \code{NORMAL}.  
#' \item \code{priorParams} A numeric vector of length two of parameters for the distribution specified in the \code{priorName} slot. When \code{priorName} is set to \code{NORMAL}, the first element of \code{priorParams} is the mean, the second element is the standard deviation.  When \code{priorName} is set to \code{STUDENT_T}, the first element of \code{priorParams} is \eqn{mu}, a location parameter, the second is degrees of freedom.  When \code{priorName} is set to \code{UNIFORM}, the elements of \code{priorParams} are lower and upper bounds, respectively.  Note that the uniform distribution is only applicable for the "EAP" estimation method.  The default values are \eqn{0,1}. 
#' \item \code{lowerBound} A numeric indicating the lower bound of the interval of the latent scale used in estimation. The default value is \eqn{-5}.
#' \item \code{upperBound} A numeric indicating the upper bound of the interval of the latent scale used in estimation. The default value is \eqn{5}.
#' \item \code{model} A string indicating the model fit to the data.  The options are \code{ltm} for the latent trait model, \code{tpm} for Birnbaum's three parameter model, \code{grm} for the graded response model, and \code{gpcm} for the generalized partial credit model.  
#' \item \code{estimation} A string indicating the choice of approach to estimate ability parameters.  The options are \code{EAP} for the expected a posteriori approach, \code{MAP} for the modal a posteriori approach, \code{MLE} for the maximum likelihood approach, and \code{WLE} for the weighted maximum likelihood approach.  The default value is \code{EAP}.
#' \item \code{estimationDefault} A string indicating the choice of approach to estimate ability parameters when the primary estimation choice indicated in the \code{estimation} slot fails to converge.  The options are \code{EAP} and \code{MAP}.  The default value is \code{MAP}.
#' \item \code{selection} A string indicating the choice of approach for selecting the next item.  The options are \code{EPV} for minimum expected posterior variance, \code{MEI} for maximum expected information, \code{MFI} for maximum Fisher information, \code{MPWI} for maximum posterior weighted information, \code{MLWI} for maximum likelihood weighted information, \code{KL} for the maximum expected Kullback-Leibler (KL) information, \code{LKL} maximum likelihood weighted KL information, \code{PKL} maximum posterior weighted KL information, \code{MFII}, and \code{RANDOM} where the next item is chosen randomly.  The default value is \code{EPV}.  
#' \item \code{z} A numeric used in calculating \eqn{\delta}.  \eqn{\delta} is used in determining the bounds of integration for some \code{selectItem} methods.  Default value is \code{0.9}.
#' \item \code{lengthThreshold} A numeric.  The number of questions answered must be greater than or equal to this threshold to stop administering items.  The default value is \code{NA}.
#' \item \code{seThreshold} A numeric.  The standard error estimate of the latent trait must be less than this threshold to stop administering items.  The default value is \code{NA}.
#' \item \code{infoThreshold} A numeric.  The Fisher's information for all remaining items must be less than this threshold to stop administering items.  The default value is \code{NA}.
#' \item \code{gainThreshold} A numeric.  The absolute value of the difference between the standard error of the latent trait estimate and the square root of the expected posterior variance for each item must be less than this threshold to stop administering items.  The default value is \code{NA}.
#' \item \code{lengthOverride} A numeric.  The number of questions answered must be less than this override to continue administering items.  The default value is \code{NA}.
#' \item \code{gainOverride} A numeric.  The absolute value of the difference between the standard error of the latent trait estimate and the square root of the expected posterior variance for each item must be less than this override to continue administering items.  The default value is \code{NA}.  
#' }
#' 
#' @seealso 
#' \code{\link{estimateTheta}} for more information on the estimation procedures
#' 
#' \code{\link{selectItem}} for more information on the item selection procedures
#' 
#' \code{\link{checkStopRules}} for more information on stopping thresholds and overrides
#'
#'
#'
#'@author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, 
#'Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' @aliases Cat-class initialize,Cat-method
#' @rdname Cat
#' @export
setClass("Cat",
  slots = list(
    guessing = "numeric",
    discrimination = "numeric",
    difficulty = "numericORlist",
    answers = "logicalORnumeric",
    priorName = "character",
    priorParams = "numeric",
    lowerBound = "numeric",
    upperBound = "numeric",
    model = "character",
    estimation = "character",
    estimationDefault = "character",
    selection = "character",
    z = "numeric",
    lengthThreshold = "logicalORnumeric",
    seThreshold = "logicalORnumeric",
    infoThreshold = "logicalORnumeric",
    gainThreshold = "logicalORnumeric",
    lengthOverride = "logicalORnumeric",
    gainOverride = "logicalORnumeric"),
  prototype = prototype(
    guessing = rep(0, 10),
    discrimination = rep(0, 10),
    difficulty = rep(0, 10),
    answers = rep(NA, 10),
    priorName = "NORMAL",
    priorParams = c(0,1),
    lowerBound = -5,
    upperBound = 5,
    model = "ltm",
    estimation = "EAP",
    estimationDefault = "MAP",
    selection = "EPV",
    z = 0.9,
    lengthThreshold = NA,
    seThreshold = NA,
    infoThreshold = NA,
    gainThreshold = NA,
    lengthOverride = NA,
    gainOverride = NA))

#' @export
setMethod("initialize", "Cat", function(.Object, ...) {
  .Object <- callNextMethod()
  validObject(.Object)
  return(.Object)
})


setValidity("Cat", function(object){
  if(! length(object@discrimination) > 1){
    stop("Discrimination needs length greater than 1.")
  }

  if(! length(object@discrimination) == length(object@guessing)){
    stop("Discrimination and guessing need to be same length.")
  }

  if(! length(object@discrimination) == length(object@answers)){
    stop("Discrimination and answers need to be same length.")
  }

  if(! length(object@discrimination)==length(object@difficulty)){
    stop("Discrimination and difficulty need to be same length.")
  }

  if(object@model == "grm" | object@model == "gpcm"){
    if(class(object@difficulty) != "list") stop("Difficulty needs to be a list.")
  }
  
  if(object@upperBound < object@lowerBound){
    stop("Lower bound value must be less than upper bound value.")
  }
  
  if(object@model == "grm"){
    for(i in object@difficulty){
      sorted <- sort(i)
      uniques <- unique(sorted)
      if(sum(sorted != i) > 0){
        stop("Difficulty values must be increasing.")
      }
      if(length(uniques) != length(i)){
        stop("Difficulty values must be unique within each item.")
      }
    }
  }
  
  if(sum(is.na(object@discrimination)) > 0){
    stop("Discrimination values cannot be NA.")
  }
  
  if(object@priorName == 'UNIFORM'){
    if(object@estimation != "EAP"){
      stop("Uniform prior requires EAP estimation.")
    }
  }
  
  if(sum(is.na(object@guessing)) > 0){
    stop("Guessing values cannot be NA.")
  }
  
  if(sum(object@guessing < 0) > 0 | sum(object@guessing > 1) > 0){
    stop("Guessing values must be between 0 and 1.")
  }
  
  model_options = c("ltm", "grm", "gpcm", "tpm")
  if(! object@model %in% model_options){
    stop("Model is not valid.  Must be 'ltm', 'tpm', 'grm' or 'gpcm'.")
  }
  
  estimation_options = c("EAP", "MAP", "MLE", "WLE")
  if(! object@estimation %in% estimation_options){
    stop("Estimation method is not valid.  Must be 'EAP', 'MAP', 'MLE', or 'WLE'.")
  }
  
  estdefault_options = c("EAP", "MAP")
  if(! object@estimationDefault %in% estdefault_options){
    stop("Estimation default method is not valid.  Must be 'EAP' or 'MAP'.")
  }
  
  prior_options <- c("NORMAL", "STUDENT_T", "UNIFORM")
  if(! object@priorName %in% prior_options){
    stop("Prior name is not valid.")
  }
  
  selection_options = c("EPV", "MEI", "MFI", "MPWI", "MLWI",
                        "KL", "LKL", "PKL", "MFII", "RANDOM")
  if(!object@selection %in% selection_options){
    stop("Selection method is not valid.")
  }
})


setGeneric("setGuessing<-", function(object, value) standardGeneric("setGuessing<-"))

#' @export
setReplaceMethod("setGuessing", "Cat", definition = function(object, value){
  slot(object, "guessing") <- value
  validObject(object)
  return(object)
})

setGeneric("setDiscrimination<-", function(object, value) standardGeneric("setDiscrimination<-"))

#' @export
setReplaceMethod("setDiscrimination", "Cat", definition = function(object, value){
  slot(object, "discrimination") <- value
  validObject(object)
  return(object)
})


setGeneric("setDifficulty<-", function(object, value) standardGeneric("setDifficulty<-"))

#' @export
setReplaceMethod("setDifficulty", "Cat", definition = function(object, value){
  slot(object, "difficulty") <- value
  validObject(object)
  return(object)
})


setGeneric("setAnswers<-", function(object, value) standardGeneric("setAnswers<-"))

#' @export
setReplaceMethod("setAnswers", "Cat", definition = function(object, value){
  slot(object, "answers") <- value
  validObject(object)
  return(object)
})


setGeneric("setModel<-", function(object, value) standardGeneric("setModel<-"))

#' @export
setReplaceMethod("setModel", "Cat", definition = function(object, value){
  slot(object, "model") <- value
  validObject(object)
  return(object)
})


setGeneric("setPriorName<-", function(object, value) standardGeneric("setPriorName<-"))

#' @export
setReplaceMethod("setPriorName", "Cat", definition = function(object, value){
  slot(object, "priorName") <- value
  validObject(object)
  return(object)
})


setGeneric("setPriorParams<-", function(object, value) standardGeneric("setPriorParams<-"))

#' @export
setReplaceMethod("setPriorParams", "Cat", definition = function(object, value){
  slot(object, "priorParams") <- value
  validObject(object)
  return(object)
})


setGeneric("setLowerBound<-", function(object, value) standardGeneric("setLowerBound<-"))

#' @export
setReplaceMethod("setLowerBound", "Cat", definition = function(object, value){
  slot(object, "lowerBound") <- value
  validObject(object)
  return(object)
})

setGeneric("setUpperBound<-", function(object, value) standardGeneric("setUpperBound<-"))

#' @export
setReplaceMethod("setUpperBound", "Cat", definition = function(object, value){
  slot(object, "upperBound") <- value
  validObject(object)
  return(object)
})



setGeneric("setEstimation<-", function(object, value) standardGeneric("setEstimation<-"))

#' @export
setReplaceMethod("setEstimation", "Cat", definition = function(object, value){
  slot(object, "estimation") <- value
  validObject(object)
  return(object)
})


setGeneric("setEstimationDefault<-", function(object, value) standardGeneric("setEstimationDefault<-"))

#' @export
setReplaceMethod("setEstimationDefault", "Cat", definition = function(object, value){
  slot(object, "estimationDefault") <- value
  validObject(object)
  return(object)
})


setGeneric("setSelection<-", function(object, value) standardGeneric("setSelection<-"))

#' @export
setReplaceMethod("setSelection", "Cat", definition = function(object, value){
  slot(object, "selection") <- value
  validObject(object)
  return(object)
})


setGeneric("setZ<-", function(object, value) standardGeneric("setZ<-"))

#' @export
setReplaceMethod("setZ", "Cat", definition = function(object, value){
  slot(object, "z") <- value
  validObject(object)
  return(object)
})


setGeneric("setLengthThreshold<-", function(object, value) standardGeneric("setLengthThreshold<-"))

#' @export
setReplaceMethod("setLengthThreshold", "Cat", definition = function(object, value){
  slot(object, "lengthThreshold") <- value
  validObject(object)
  return(object)
})


setGeneric("setSeThreshold<-", function(object, value) standardGeneric("setSeThreshold<-"))

#' @export
setReplaceMethod("setSeThreshold", "Cat", definition = function(object, value){
  slot(object, "seThreshold") <- value
  validObject(object)
  return(object)
})


setGeneric("setGainThreshold<-", function(object, value) standardGeneric("setGainThreshold<-"))

#' @export
setReplaceMethod("setGainThreshold", "Cat", definition = function(object, value){
  slot(object, "gainThreshold") <- value
  validObject(object)
  return(object)
})


setGeneric("setInfoThreshold<-", function(object, value) standardGeneric("setInfoThreshold<-"))

#' @export
setReplaceMethod("setInfoThreshold", "Cat", definition = function(object, value){
  slot(object, "infoThreshold") <- value
  validObject(object)
  return(object)
})


setGeneric("setLengthOverride<-", function(object, value) standardGeneric("setLengthOverride<-"))

#' @export
setReplaceMethod("setLengthOverride", "Cat", definition = function(object, value){
  slot(object, "lengthOverride") <- value
  validObject(object)
  return(object)
})


setGeneric("setGainOverride<-", function(object, value) standardGeneric("setGainOverride<-"))

#' @export
setReplaceMethod("setGainOverride", "Cat", definition = function(object, value){
  slot(object, "gainThreshold") <- value
  validObject(object)
  return(object)
})


setGeneric("getModel", function(object) standardGeneric("getModel"))

#' @export
setMethod("getModel", "Cat", function(object) return(object@model))

setGeneric("getGuessing", function(object) standardGeneric("getGuessing"))

#' @export
setMethod("getGuessing", "Cat", function(object) return(object@guessing))

setGeneric("getDiscrimination", function(object) standardGeneric("getDiscrimination"))

#' @export
setMethod("getDiscrimination", "Cat", function(object) return(object@discrimination))

setGeneric("getDifficulty", function(object) standardGeneric("getDifficulty"))

#' @export
setMethod("getDifficulty", "Cat", function(object) return(object@difficulty))

setGeneric("getAnswers", function(object) standardGeneric("getAnswers"))

#' @export
setMethod("getAnswers", "Cat", function(object) return(object@answers))

setGeneric("getPriorName", function(object) standardGeneric("getPriorName"))

#' @export
setMethod("getPriorName", "Cat", function(object) return(object@priorName))

setGeneric("getPriorParams", function(object) standardGeneric("getPriorParams"))

#' @export
setMethod("getPriorParams", "Cat", function(object) return(object@priorParams))

setGeneric("getLowerBound", function(object) standardGeneric("getLowerBound"))

#' @export
setMethod("getLowerBound", "Cat", function(object) return(object@lowerBound))

setGeneric("getUpperBound", function(object) standardGeneric("getUpperBound"))

#' @export
setMethod("getUpperBound", "Cat", function(object) return(object@upperBound))

setGeneric("getEstimation", function(object) standardGeneric("getEstimation"))

#' @export
setMethod("getEstimation", "Cat", function(object) return(object@estimation))

setGeneric("getEstimationDefault", function(object) standardGeneric("getEstimationDefault"))

#' @export
setMethod("getEstimationDefault", "Cat", function(object) return(object@estimationDefault))

setGeneric("getSelection", function(object) standardGeneric("getSelection"))

#' @export
setMethod("getSelection", "Cat", function(object) return(object@selection))

setGeneric("getZ", function(object) standardGeneric("getZ"))

#' @export
setMethod("getZ", "Cat", function(object) return(object@z))

setGeneric("getLengthThreshold", function(object) standardGeneric("getLengthThreshold"))

#' @export
setMethod("getLengthThreshold", "Cat", function(object) return(object@lengthThreshold))

setGeneric("getSeThreshold", function(object) standardGeneric("getSeThreshold"))

#' @export
setMethod("getSeThreshold", "Cat", function(object) return(object@seThreshold))

setGeneric("getInfoThreshold", function(object) standardGeneric("getInfoThreshold"))

#' @export
setMethod("getInfoThreshold", "Cat", function(object) return(object@infoThreshold))

setGeneric("getGainThreshold", function(object) standardGeneric("getGainThreshold"))

#' @export
setMethod("getGainThreshold", "Cat", function(object) return(object@gainThreshold))

setGeneric("getLengthOverride", function(object) standardGeneric("getLengthOverride"))

#' @export
setMethod("getLengthOverride", "Cat", function(object) return(object@lengthOverride))

setGeneric("getGainOverride", function(object) standardGeneric("getGainOverride"))

#' @export
setMethod("getGainOverride", "Cat", function(object) return(object@gainOverride) )

