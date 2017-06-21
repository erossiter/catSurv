#' @useDynLib catSurv, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import methods
NULL

class.name = "Cat"
setClassUnion("logicalORnumeric", c("numeric","logical"))
setClassUnion("numericORlist", c("numeric","list"))

#' Computerized Adaptive Testing Survey (catSurv) Object
#'
#' Creates an object of class \code{Cat}.  \code{Cat} objects are used in administering Computerized Adaptive Testing (CAT) Surveys.  These objects contain several pieces of information relevant for CAT surveys, and are used as input in the main functions of the \code{catSurv} package. 
#'
#' Assume we have a survey battery with \code{I} questions.  An object of the class \code{Cat} has the following slots:
#' \itemize{
#' \item \code{guessing} A vector of length \code{I} of guessing parameters.  Guessing parameters are only applicable for \code{Cat} objects fit with the \code{"tpm"} model, using the \code{tpmCat} function. 
#' \item \code{discrimination} A vector of length \code{I} of discrimination parameters.
#' \item \code{difficulty} A vector or list of length \code{I} of difficulty parameters. For \code{Cat} objects of the \code{"ltm"} or \code{"tpm"} model, \code{difficulty} is a vector that contains a parameter for each item.  For \code{Cat} objects of the \code{"grm"} or \code{"gpcm"} model, \code{difficulty} is a list that contains a vector for each item, and each vector contains parameters for each response option.  
#' \item \code{answers} A vector of length \code{I} of answers to questions as given by the survey respondent.  Unanswered questions have the value \code{NA}.  Questions respondent has skipped or refused to answer have a value of \code{-1}.
#' \item \code{priorName} A character vector of length one giving the prior distribution to use for the ability parameter estimates.  The options are \code{"NORMAL"} for the normal distribution, \code{"STUDENT_T"} for the student's t distribution, and \code{"UNIFORM"} for the uniform distribution.  The default value is \code{"NORMAL"}.  
#' \item \code{priorParams} A numeric vector of length two of parameters for the distribution specified in the \code{priorName} slot. When \code{priorName} is set to \code{"NORMAL"}, the first element of \code{priorParams} is the mean, and the second element is the standard deviation.  When \code{priorName} is set to \code{"STUDENT_T"}, the first element of \code{priorParams} is the location parameter and the second is degrees of freedom.  When \code{priorName} is set to \code{"UNIFORM"}, the elements of \code{priorParams} are lower and upper bound, respectively.  Note that the uniform distribution is only applicable for the \code{"EAP"} estimation method.  The default values are \eqn{0,1}. 
#' \item \code{lowerBound} A numeric indicating the lower bound of the interval of the latent scale used in estimation. The default value is \eqn{-5}.
#' \item \code{upperBound} A numeric indicating the upper bound of the interval of the latent scale used in estimation. The default value is \eqn{5}.
#' \item \code{model} A string indicating the model fit to the data.  The options are \code{"ltm"} for the latent trait model, \code{"tpm"} for Birnbaum's three parameter model, \code{"grm"} for the graded response model, and \code{"gpcm"} for the generalized partial credit model.  
#' \item \code{estimation} A string indicating the approach to estimating ability parameters.  The options are \code{"EAP"} for the expected a posteriori approach, \code{"MAP"} for the modal a posteriori approach, \code{"MLE"} for the maximum likelihood approach, and \code{"WLE"} for the weighted maximum likelihood approach.  The default value is \code{"EAP"}.
#' \item \code{estimationDefault} A string indicating the approach to estimating ability parameters when the primary estimation choice indicated in the \code{estimation} slot is \code{"MLE"} or \code{"WLE"} and this estimation fails to converge.  The options are \code{"EAP"} and \code{"MAP"}.  The default value is \code{"MAP"}.
#' \item \code{selection} A string indicating the approach for selecting the next item.  The options are \code{"EPV"} for minimum expected posterior variance, \code{"MEI"} for maximum expected information, \code{"MFI"} for maximum Fisher information, \code{"MPWI"} for maximum posterior weighted information, \code{"MLWI"} for maximum likelihood weighted information, \code{"KL"} for the maximum expected Kullback-Leibler (KL) information, \code{"LKL"} maximum likelihood weighted KL information, \code{"PKL"} maximum posterior weighted KL information, \code{"MFII"} for maximum Fisher interval information, and \code{"RANDOM"} where the next item is chosen randomly.  The default value is \code{"EPV"}.  
#' \item \code{z} A numeric used in calculating \eqn{\delta}.  \eqn{\delta} is used in determining the bounds of integration for some \code{selectItem} methods.  Default value is \code{0.9}.
#' \item \code{lengthThreshold} A numeric.  The number of questions answered must be greater than or equal to this threshold to stop administering items.  The default value is \code{NA}.
#' \item \code{seThreshold} A numeric.  The standard error estimate of the latent trait must be less than this threshold to stop administering items.  The default value is \code{NA}.
#' \item \code{infoThreshold} A numeric.  The Fisher's information for all remaining items must be less than this threshold to stop administering items.  The default value is \code{NA}.
#' \item \code{gainThreshold} A numeric.  The absolute value of the difference between the standard error of the latent trait estimate and the square root of the expected posterior variance for each item must be less than this threshold to stop administering items.  The default value is \code{NA}.
#' \item \code{lengthOverride} A numeric.  The number of questions answered must be less than this override to continue administering items.  The default value is \code{NA}.
#' \item \code{gainOverride} A numeric.  The absolute value of the difference between the standard error of the latent trait estimate and the square root of the expected posterior variance for each item must be less than this override to continue administering items.  The default value is \code{NA}.  
#' }
#' 
#' @seealso \code{\link{checkStopRules}}, \code{\link{estimateTheta}}, \code{\link{gpcmCat}}, \code{\link{grmCat}}, \code{\link{ltmCat}}, \code{\link{selectItem}}, \code{\link{tpmCat}}
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


#' Methods for Setting Value(s) to \code{Cat} Object Slots
#' 
#' Setter methods to control changes to the slots of a \code{Cat} object.
#' 
#' @param catObj An object of class \code{Cat}
#' @param value The new value(s)
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, 
#'Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' 
#' 
#' @examples
#' ## Loading ltm Cat object
#' data(ltm_cat)
#' 
#' ## Setting estimation slot
#' getEstimation(ltm_cat)
#' setEstimation(ltm_cat) <- "MAP"
#' getEstimation(ltm_cat)
#' 
#' ## Setting distrimination slot
#' getDiscrimination(ltm_cat)
#' setDiscrimination(ltm_cat) <- rep(1, 40)
#' getDiscrimination(ltm_cat)
#'
#'@name setters
#'@seealso \code{\link{Cat-class}}, \code{\link{getters}}
NULL

setGeneric("setGuessing<-", function(catObj, value) standardGeneric("setGuessing<-"))

#' @aliases setGuessing<- setters
#' @rdname setters
#' @export
setReplaceMethod("setGuessing", "Cat", definition = function(catObj, value){
  slot(catObj, "guessing") <- value
  validObject(catObj)
  return(catObj)
})

setGeneric("setDiscrimination<-", function(catObj, value) standardGeneric("setDiscrimination<-"))

#' @aliases setDiscrimination<- setters
#' @rdname setters
#' @export
setReplaceMethod("setDiscrimination", "Cat", definition = function(catObj, value){
  slot(catObj, "discrimination") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setDifficulty<-", function(catObj, value) standardGeneric("setDifficulty<-"))

#' @aliases setDifficulty<- setters
#' @rdname setters
#' @export
setReplaceMethod("setDifficulty", "Cat", definition = function(catObj, value){
  slot(catObj, "difficulty") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setAnswers<-", function(catObj, value) standardGeneric("setAnswers<-"))

#' @aliases setAnswers<- setters
#' @rdname setters
#' @export
setReplaceMethod("setAnswers", "Cat", definition = function(catObj, value){
  slot(catObj, "answers") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setModel<-", function(catObj, value) standardGeneric("setModel<-"))

#' @aliases setModel<- setters
#' @rdname setters
#' @export
setReplaceMethod("setModel", "Cat", definition = function(catObj, value){
  slot(catObj, "model") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setPriorName<-", function(catObj, value) standardGeneric("setPriorName<-"))

#' @aliases setPriorName<- setters
#' @rdname setters
#' @export
setReplaceMethod("setPriorName", "Cat", definition = function(catObj, value){
  slot(catObj, "priorName") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setPriorParams<-", function(catObj, value) standardGeneric("setPriorParams<-"))

#' @aliases setPriorParams<- setters
#' @rdname setters
#' @export
setReplaceMethod("setPriorParams", "Cat", definition = function(catObj, value){
  slot(catObj, "priorParams") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setLowerBound<-", function(catObj, value) standardGeneric("setLowerBound<-"))

#' @aliases setLowerBound<- setters
#' @rdname setters
#' @export
setReplaceMethod("setLowerBound", "Cat", definition = function(catObj, value){
  slot(catObj, "lowerBound") <- value
  validObject(catObj)
  return(catObj)
})

setGeneric("setUpperBound<-", function(catObj, value) standardGeneric("setUpperBound<-"))

#' @aliases setUpperBound<- setters
#' @rdname setters
#' @export
setReplaceMethod("setUpperBound", "Cat", definition = function(catObj, value){
  slot(catObj, "upperBound") <- value
  validObject(catObj)
  return(catObj)
})

setGeneric("setEstimation<-", function(catObj, value) standardGeneric("setEstimation<-"))

#' @aliases setEstimation<- setters
#' @rdname setters
#' @export
setReplaceMethod("setEstimation", "Cat", definition = function(catObj, value){
  slot(catObj, "estimation") <- value
  validObject(catObj)
  return(catObj)
})

setGeneric("setEstimationDefault<-", function(catObj, value) standardGeneric("setEstimationDefault<-"))

#' @aliases setEstimationDefault<- setters
#' @rdname setters
#' @export
setReplaceMethod("setEstimationDefault", "Cat", definition = function(catObj, value){
  slot(catObj, "estimationDefault") <- value
  validObject(catObj)
  return(catObj)
})

setGeneric("setSelection<-", function(catObj, value) standardGeneric("setSelection<-"))

#' @aliases setSelection<- setters
#' @rdname setters
#' @export
setReplaceMethod("setSelection", "Cat", definition = function(catObj, value){
  slot(catObj, "selection") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setZ<-", function(catObj, value) standardGeneric("setZ<-"))

#' @aliases setZ<- setters
#' @rdname setters
#' @export
setReplaceMethod("setZ", "Cat", definition = function(catObj, value){
  slot(catObj, "z") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setLengthThreshold<-", function(catObj, value) standardGeneric("setLengthThreshold<-"))

#' @aliases setLengthThreshold<- setters
#' @rdname setters
#' @export
setReplaceMethod("setLengthThreshold", "Cat", definition = function(catObj, value){
  slot(catObj, "lengthThreshold") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setSeThreshold<-", function(catObj, value) standardGeneric("setSeThreshold<-"))

#' @aliases setSeThreshold<- setters
#' @rdname setters
#' @export
setReplaceMethod("setSeThreshold", "Cat", definition = function(catObj, value){
  slot(catObj, "seThreshold") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setGainThreshold<-", function(catObj, value) standardGeneric("setGainThreshold<-"))

#' @aliases setGainThreshold<- setters
#' @rdname setters
#' @export
setReplaceMethod("setGainThreshold", "Cat", definition = function(catObj, value){
  slot(catObj, "gainThreshold") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setInfoThreshold<-", function(catObj, value) standardGeneric("setInfoThreshold<-"))

#' @aliases setInfoThreshold<- setters
#' @rdname setters
#' @export
setReplaceMethod("setInfoThreshold", "Cat", definition = function(catObj, value){
  slot(catObj, "infoThreshold") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setLengthOverride<-", function(catObj, value) standardGeneric("setLengthOverride<-"))

#' @aliases setLengthOverride<- setters
#' @rdname setters
#' @export
setReplaceMethod("setLengthOverride", "Cat", definition = function(catObj, value){
  slot(catObj, "lengthOverride") <- value
  validObject(catObj)
  return(catObj)
})


setGeneric("setGainOverride<-", function(catObj, value) standardGeneric("setGainOverride<-"))


#' @aliases setGainOverride<- setters
#' @rdname setters
#' @export
setReplaceMethod("setGainOverride", "Cat", definition = function(catObj, value){
  slot(catObj, "gainThreshold") <- value
  validObject(catObj)
  return(catObj)
})



#' Methods for Accessing \code{Cat} Object Slots
#' 
#' Getter methods to access slots of a \code{Cat} object.
#' 
#' @param catObj An object of class \code{Cat}
#' @return These functions return the respective slot from Cat object.
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, 
#'Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' 
#' @examples
#' ## Loading ltm Cat object
#' data(ltm_cat)
#' 
#' ## Getting estimation slot before and after change
#' getEstimation(ltm_cat)
#' setEstimation(ltm_cat) <- "MAP"
#' getEstimation(ltm_cat)
#' 
#' ## Getting discrimination slot before and after change
#' getDiscrimination(ltm_cat)
#' setDiscrimination(ltm_cat) <- rep(1, 40)
#' getDiscrimination(ltm_cat)
#'
#'
#'@name getters
#'@seealso \code{\link{Cat-class}}, \code{\link{setters}}
NULL

setGeneric("getModel", function(catObj) standardGeneric("getModel"))

#' @aliases getModel getters
#' @rdname getters
#' @export
setMethod("getModel", "Cat", function(catObj) return(catObj@model))

setGeneric("getGuessing", function(catObj) standardGeneric("getGuessing"))

#' @aliases getGuessing getters
#' @rdname getters
#' @export
setMethod("getGuessing", "Cat", function(catObj) return(catObj@guessing))

setGeneric("getDiscrimination", function(catObj) standardGeneric("getDiscrimination"))

#' @aliases getDiscrimination getters
#' @rdname getters
#' @export
setMethod("getDiscrimination", "Cat", function(catObj) return(catObj@discrimination))

setGeneric("getDifficulty", function(catObj) standardGeneric("getDifficulty"))

#' @aliases getDifficulty getters
#' @rdname getters
#' @export
setMethod("getDifficulty", "Cat", function(catObj) return(catObj@difficulty))

setGeneric("getAnswers", function(catObj) standardGeneric("getAnswers"))

#' @aliases getAnswers getters
#' @rdname getters
#' @export
setMethod("getAnswers", "Cat", function(catObj) return(catObj@answers))

setGeneric("getPriorName", function(catObj) standardGeneric("getPriorName"))

#' @aliases getPriorName getters
#' @rdname getters
#' @export
setMethod("getPriorName", "Cat", function(catObj) return(catObj@priorName))

setGeneric("getPriorParams", function(catObj) standardGeneric("getPriorParams"))

#' @aliases getPriorParams getters
#' @rdname getters
#' @export
setMethod("getPriorParams", "Cat", function(catObj) return(catObj@priorParams))

setGeneric("getLowerBound", function(catObj) standardGeneric("getLowerBound"))

#' @aliases getLowerBound getters
#' @rdname getters
#' @export
setMethod("getLowerBound", "Cat", function(catObj) return(catObj@lowerBound))

setGeneric("getUpperBound", function(catObj) standardGeneric("getUpperBound"))

#' @aliases getUpperBound getters
#' @rdname getters
#' @export
setMethod("getUpperBound", "Cat", function(catObj) return(catObj@upperBound))

setGeneric("getEstimation", function(catObj) standardGeneric("getEstimation"))

#' @aliases getEstimation getters
#' @rdname getters
#' @export
setMethod("getEstimation", "Cat", function(catObj) return(catObj@estimation))

setGeneric("getEstimationDefault", function(catObj) standardGeneric("getEstimationDefault"))

#' @aliases getEstimationDefault getters
#' @rdname getters
#' @export
setMethod("getEstimationDefault", "Cat", function(catObj) return(catObj@estimationDefault))

setGeneric("getSelection", function(catObj) standardGeneric("getSelection"))

#' @aliases getSelection getters
#' @rdname getters
#' @export
setMethod("getSelection", "Cat", function(catObj) return(catObj@selection))

setGeneric("getZ", function(catObj) standardGeneric("getZ"))

#' @aliases getZ getters
#' @rdname getters
#' @export
setMethod("getZ", "Cat", function(catObj) return(catObj@z))

setGeneric("getLengthThreshold", function(catObj) standardGeneric("getLengthThreshold"))

#' @aliases getLengthThreshold getters
#' @rdname getters
#' @export
setMethod("getLengthThreshold", "Cat", function(catObj) return(catObj@lengthThreshold))

setGeneric("getSeThreshold", function(catObj) standardGeneric("getSeThreshold"))

#' @aliases getSeThreshold getters
#' @rdname getters
#' @export
setMethod("getSeThreshold", "Cat", function(catObj) return(catObj@seThreshold))

setGeneric("getInfoThreshold", function(catObj) standardGeneric("getInfoThreshold"))

#' @aliases getInfoThreshold getters
#' @rdname getters
#' @export
setMethod("getInfoThreshold", "Cat", function(catObj) return(catObj@infoThreshold))

setGeneric("getGainThreshold", function(catObj) standardGeneric("getGainThreshold"))

#' @aliases getGainThreshold getters
#' @rdname getters
#' @export
setMethod("getGainThreshold", "Cat", function(catObj) return(catObj@gainThreshold))

setGeneric("getLengthOverride", function(catObj) standardGeneric("getLengthOverride"))

#' @aliases getLengthOverride getters
#' @rdname getters
#' @export
setMethod("getLengthOverride", "Cat", function(catObj) return(catObj@lengthOverride))

setGeneric("getGainOverride", function(catObj) standardGeneric("getGainOverride"))

#' @aliases getGainOverride getters
#' @rdname getters
#' @export
setMethod("getGainOverride", "Cat", function(catObj) return(catObj@gainOverride) )

