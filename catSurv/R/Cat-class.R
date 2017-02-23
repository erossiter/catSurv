#' @useDynLib catSurv
#' @importFrom Rcpp sourceCpp
NULL

class.name = "Cat"
setClassUnion("logicalORnumeric", c("numeric","logical"))
setClassUnion("numericORlist", c("numeric","list"))

#' A Computerized Adaptive Testing Survey (catSurv) Object
#'
#' Objects of class \code{Cat} are used in administering Computerized Adaptive Testing (CAT) Surveys.  These objects contain several pieces of information relevent for CAT surveys, and are used as input in the main functions of the \code{catSurv} package. They are created using the \code{initialize} function.
#'
#' Assume we have a survey battery with \code{I} questions.  An object of the class \code{Cat} has the following slots:
#' \itemize{
#' \item \code{guessing} A named vector of length \code{I} of guessing parameters.  Note: guessing parameters are only applicable for binary \code{Cat} objects. 
#' \item \code{discrimination} A named vector of length \code{I} of disrimination parameters.
#' \item \code{difficulty} A named vector or list of length \code{I} of difficulty parameters. For binary \code{Cat} objects, the vector will contain difficulty parameters for each item.  For categorical \code{Cat} objects, a list will constain a vector for each item, and each vector will contain a difficulty parameter for each response option.  
#' \item \code{answers} A named vector of length \code{I} of answers to questions as given by the survey respondent.  Unanswered questions have the value \code{NA}.
#' \item \code{priorName} A character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{NORMAL} for the normal distirbution, \code{STUDENT_T} for the student's t distribution, and \code{UNIFORM} for the uniform distribution.  
#' \item \code{priorParams} A numeric vector of length two of parameters for the distribution specified in the \code{priorName} slot. See 'details' for more information.
#' \item \code{lowerBound} A numeric indicating the lower bound of the interval of the latent scale used in estimation. The default value is \eqn{-6}.
#' \item \code{upperBound} A numeric indicating the upper bound of the interval of the latent scale used in estimation. The default value is \eqn{6}.
#' \item \code{poly} A logical containing the type of answers.  TRUE indicates categorical response options, FALSE indicates binary response options.  Default is TRUE.
#' \item \code{estimation} A character vector of length one indicating the choice of approach to estimate ability parameters.  The options are \code{EAP}, \code{MAP}, \code{MLE}, and \code{WLE}.
#' \item \code{estimationDefault} A character vector of length one indicating the choice of approach to estimate ability parameters when the primary estimation choice indicating in the \code{estimation} slot fails to converge.  The options are \code{EAP} and \code{MAP}.
#' \item \code{selection} A character vector of length one indicating the choice of approach select the next item.  The options are \code{EPV}, \code{MEI}, \code{MFI}, \code{MPWI}, \code{MLWI}, \code{KL}, \code{LKL}, \code{PKL}, \code{MFII}, and \code{RANDOM}.
#' \item \code{z} A numeric.  Used in calculating delta, which is used in calculating the bounds of integration for some \code{selectItem} methods.  See 'details' for more information.
#' \item \code{lengthThreshold} A numeric.  The number of questions answered must be greater than or equal to this threshold.
#' \item \code{seThreshold} A numeric.  The standard error estimate of the latent trait must be less than this threshold.
#' \item \code{infoThreshold} A numeric.  The Fisher's information for all remaining items must be less than this threshold.
#' \item \code{gainThreshold} A numeric.  The absolute value of the difference between the standard error of the latent trait estimate and the square root of the expected posterior variance for each item must be less than this threshold.
#' \item \code{lengthOverride} A numeric.  The number of questions answered must be less than this override.
#' \item \code{gainOverride} A numeric.  The absolute value of the difference between the standard error of the latent trait estimate and the square root of the expected posterior variance for each item must be less than this override.  
#' }
#'
#'@details When \code{priorName} is set to "NORMAL", the first element of \code{priorParams} is the mean, the second element is the standard deviation.  When \code{priorName} is set to "STUDENT_T", the first element of \code{priorParam} is mu, a location parameter, the second is degrees of freedom.  When \code{priorName} is set to "UNIFORM", the elements of \code{priorParams} are lower and upper, respectively.  Note that the uniform distribution is only applicable for the "EAP" estimation method.  
#'
#' Talk about 'z' and how it's used to calculate delta.
#' 
#'
#'
#'
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @aliases Cat-class initialize,Cat-method
#' @rdname Cat
#' @export
setClass("Cat",
  slots = list(
    guessing = "numeric",
    discrimination = "numeric",
    answers = "logicalORnumeric",
    priorName = "character",
    priorParams = "numeric",
    lowerBound = "numeric",
    upperBound = "numeric",
    difficulty = "numericORlist",
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
setMethod("initialize", class.name, function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})

########### VALIDITY CHECKS ##############

setValidity("Cat", function(object){
  # guessing, discrimination, answers, difficulty should all be same length
  #... and that length must be greater than 1
  test0<-(length(object@discrimination)>1)
  if(!test0) stop("discrimination needs length greater than 1")

  test1<-(length(object@discrimination)==length(object@guessing))
  if(!test1) stop("discrimination and guessing not same length")

  test2<-(length(object@discrimination)==length(object@answers))
  if(!test2) stop("discrimination and answers not same length")

  test3<-(length(object@discrimination)==length(object@difficulty))
  if(!test3) stop("discrimination and difficulty not same length")

  ## If the Cat is polytomous, the difficulty needs to be list
  if(object@model!="ltm"){
    if(class(object@difficulty)!="list") stop("Cat object is polytomous, but difficulty slot is not a list")
  }
  else{
    if(class(object@difficulty)!="numeric") stop("Cat object is binary, but difficulty slot is not a numeric vector")}
  

  
  ## TEST THAT DIFFICULTY VALUES ARE STRICTLY INCREASING, and not NA
  if(object@model!="ltm"){
    for(i in 1:length(object@difficulty)){
      if (is.list(object@difficulty[[i]])){
        item<-object@difficulty[[i]]
      } else(item<-object@difficulty[[i]]) #don't want to change the value stored in the object itself...
      sorted<-sort(item)
      uniques<-unique(sorted)
       test6<-(all(!is.na(item)))
      if(!test6) stop (paste("Diffulty values for question", i, " include NAs"))
     #test5<-(isTRUE(all.equal(item,sorted)))
      #if(!test5) stop(paste("Diffulty values for question", i, " are not increasing"))
       #test4<-(isTRUE(all.equal(item,uniques)))
       #if(!test4) stop(paste("Repeated difficulty values for question", i))
        
    }
  }
  
  ## test that discrimination and guessing are not NA
  for(i in object@discrimination){
    test7<-!is.na(i)
    if(!test7) stop(paste("Discrimination value for question ", which(object@discrimination==i, arr.ind=T), " is NA"))
  }
  if(object@model == "ltm"){
    for(i in object@guessing){
      test8<-!is.na(i)
      if(!test8) stop(paste("Discrimination value for question ", which(object@discrimination==i, arr.ind=T), " is NA"))
    }
  }
  
  ## checks for priorName = 'uniform'...
  if(object@priorName=='UNIFORM'){
    test9<- (object@estimation=='EAP')
    if(!test9) stop("priorName is UNIFORM, but estimation is not EAP")
    test10<- (object@lower<=object@priorParam[1])
    test11<- (object@upper>=object@priorParam[2])
    if(!test10||!test11) stop("priorName is UNIFORM but priorParam values not bounded by lower and upper bounds")
  }
  
  ## range for guessing within [0,1]
  for (i in 1:length(object@guessing)){
    if (object@guessing[i] <0 || object@guessing[i] >1){
      stop (paste("Guessing parameter for item ", i, "is out of bounds: value is ", object@guessing[i], ", but should be within [0,1]"))
    }
  }
  
  ## values for estimation:
  estimationOptions = c("EAP", "MAP", "MLE", "WLE")
  if(!object@estimation %in% estimationOptions){
    stop ("dstimation value invalid. See ?Cat documentation for valid options.")
  }
  
  ## values for estimationDefault:
  estimationDefaultOptions = c("EAP", "MAP")
  if(!object@estimationDefault %in% estimationDefaultOptions){
    stop ("estimationDefault value invalid. See ?Cat documentation for valid options.")
  }
  
  ## values for selection:
  selectionOptions = c("EPV", "MEI", "MFI", "MPWI", "MLWI", "KL", "LKL", "PKL", "MFII", "RANDOM")
  if(!object@selection %in% selectionOptions){
    stop ("selection value invalid. See ?Cat documentation for valid options.")
  }
  
  
})



#' Set methods for slots within \code{Cat} class objects
#'
#' This functions replaces the values currently stored in a specified slot of an object of class \code{Cat} with the user-provided values.  
#'
#' @param object An object of class \code{Cat} to be modified
#' @param valid Boolean for whether to check validity of object before allowing setter method to execute, default to TRUE
#' @param value The value to replace the current value stored in the specified slot of the \code{Cat}
#' 
#' @return Modifies the \code{Cat} object that was provided by the user, and returns the modified object 
#' 
#' @note There are setters for every slot in \code{Cat} objects, all following the format \code{setSlot}: 
#' eg. \code{setDiscrimination}, \code{setEstimationDefault}, \code{setSeThreshold}, etc.
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{Cat}}
#' @rdname setters
#' @aliases setGuessing setDiscrimination setDifficulty setAnswers setPriorName setPriorParams
#' setUpperBound setLowerBound setPoly setEstimation setEstimationDefault setSelection setZ
#' setLengthThreshold setSeThreshold setInfoThreshold setGainThreshold setLengthOverride setGainOverride
#' @export
setGeneric("setGuessing", function(object, value) standardGeneric("setGuessing"))

setMethod("setGuessing", "Cat", definition = function(object, value){
  object@guessing <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setDiscrimination", function(object, value) standardGeneric("setDiscrimination"))

setMethod("setDiscrimination", "Cat", definition = function(object, value){
  object@discrimination <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setDifficulty", function(object, value) standardGeneric("setDifficulty"))

setMethod("setDifficulty", "Cat", definition = function(object, value){
  object@difficulty <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setAnswers", function(object, value) standardGeneric("setAnswers"))

setMethod("setAnswers", "Cat", definition = function(object, value){
  object@answers <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setModel", function(object, value) standardGeneric("setModel"))

setMethod("setModel", "Cat", definition = function(object, value){
  object@model <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setPriorName", function(object, value) standardGeneric("setPriorName"))

setMethod("setPriorName", "Cat", definition = function(object, value){
  object@priorName <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setPriorParams", function(object, value) standardGeneric("setPriorParams"))

setMethod("setPriorParams", "Cat", definition = function(object, value){
  object@priorParams <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setLowerBound", function(object, value) standardGeneric("setLowerBound"))

setMethod("setLowerBound", "Cat", definition = function(object, value){
  object@lowerBound <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setUpperBound", function(object, value) standardGeneric("setUpperBound"))

setMethod("setUpperBound", "Cat", definition = function(object, value){
  object@upperBound <- value
  validObject(object)
  return(object)
})


#' @export
setGeneric("setEstimation", function(object, value) standardGeneric("setEstimation"))

setMethod("setEstimation", "Cat", definition = function(object, value){
  object@estimation <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setEstimationDefault", function(object, value) standardGeneric("setEstimationDefault"))

setMethod("setEstimationDefault", "Cat", definition = function(object, value){
  object@estimationDefault <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setSelection", function(object, value) standardGeneric("setSelection"))

setMethod("setSelection", "Cat", definition = function(object, value){
  object@selection <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setZ", function(object, value) standardGeneric("setZ"))

setMethod("setZ", "Cat", definition = function(object, value){
  object@z <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setLengthThreshold", function(object, value) standardGeneric("setLengthThreshold"))

setMethod("setLengthThreshold", "Cat", definition = function(object, value){
  object@lengthThreshold <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setSeThreshold", function(object, value) standardGeneric("setSeThreshold"))

setMethod("setSeThreshold", "Cat", definition = function(object, value){
  object@seThreshold <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setGainThreshold", function(object, value) standardGeneric("setGainThreshold"))

setMethod("setGainThreshold", "Cat", definition = function(object, value){
  object@gainThreshold <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setInfoThreshold", function(object, value) standardGeneric("setInfoThreshold"))

setMethod("setInfoThreshold", "Cat", definition = function(object, value){
  object@infoThreshold <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setLengthOverride", function(object, value) standardGeneric("setLengthOverride"))

setMethod("setLengthOverride", "Cat", definition = function(object, value){
  object@lengthOverride <- value
  validObject(object)
  return(object)
})

#' @export
setGeneric("setGainOverride", function(object, value) standardGeneric("setGainOverride"))

setMethod("setGainOverride", "Cat", definition = function(object, value){
  object@gainThreshold <- value
  validObject(object)
  return(object)
})

#' Get methods for slots within \code{Cat} class objects
#'
#' These functions access and return the values currently stored in a specified slot of an object of class \code{Cat}.  
#'
#' @param object An object of class \code{Cat} to be modified

#' @return The value stored in the specified slot of the \code{Cat} object. 
#' 
#' @note There are getters for every slot in \code{Cat} objects, all following the format \code{getSlot}: 
#' eg. \code{getDiscrimination}, \code{getEstimationDefault}, \code{getSeThreshold}, etc.
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso {\link{Cat}} {\link{setters}}
#' @rdname getters
#' @aliases getGuessing getDiscrimination getDifficulty getAnswers getPriorName getPriorParams
#' getUpperBound getLowerBound setPoly getEstimation getEstimationDefault getSelection getZ
#' getLengthThreshold getSeThreshold getInfoThreshold getGainThreshold getLengthOverride getGainOverride
#' @export
setGeneric("getModel", function(object) standardGeneric("getModel"))

setMethod("getModel", "Cat", function(object) return(object@model))

#' @export
setGeneric("getGuessing", function(object) standardGeneric("getGuessing"))

setMethod("getGuessing", "Cat", function(object) return(object@guessing))

#' @export
setGeneric("getDiscrimination", function(object) standardGeneric("getDiscrimination"))

setMethod("getDiscrimination", "Cat", function(object) return(object@discrimination))

#' @export
setGeneric("getDifficulty", function(object) standardGeneric("getDifficulty"))

setMethod("getDifficulty", "Cat", function(object) return(object@difficulty))

#' @export
setGeneric("getAnswers", function(object) standardGeneric("getAnswers"))

setMethod("getAnswers", "Cat", function(object) return(object@answers))

#' @export
setGeneric("getPriorName", function(object) standardGeneric("getPriorName"))

setMethod("getPriorName", "Cat", function(object) return(object@priorName))

#' @export
setGeneric("getPriorParams", function(object) standardGeneric("getPriorParams"))

setMethod("getPriorParams", "Cat", function(object) return(object@priorParams))

#' @export
setGeneric("getLowerBound", function(object) standardGeneric("getLowerBound"))

setMethod("getLowerBound", "Cat", function(object) return(object@lowerBound))

#' @export
setGeneric("getUpperBound", function(object) standardGeneric("getUpperBound"))

setMethod("getUpperBound", "Cat", function(object) return(object@upperBound))

#' @export
setGeneric("getEstimation", function(object) standardGeneric("getEstimation"))

setMethod("getEstimation", "Cat", function(object) return(object@estimation))

#' @export
setGeneric("getEstimationDefault", function(object) standardGeneric("getEstimationDefault"))

setMethod("getEstimationDefault", "Cat", function(object) return(object@estimationDefault))

#' @export
setGeneric("getSelection", function(object) standardGeneric("getSelection"))

setMethod("getSelection", "Cat", function(object) return(object@selection))

#' @export
setGeneric("getZ", function(object) standardGeneric("getZ"))

setMethod("getZ", "Cat", function(object) return(object@z))

#' @export
setGeneric("getLengthThreshold", function(object) standardGeneric("getLengthThreshold"))

setMethod("getLengthThreshold", "Cat", function(object) return(object@lengthThreshold))

#' @export
setGeneric("getSeThreshold", function(object) standardGeneric("getSeThreshold"))

setMethod("getSeThreshold", "Cat", function(object) return(object@seThreshold))

#' @export
setGeneric("getInfoThreshold", function(object) standardGeneric("getInfoThreshold"))

setMethod("getInfoThreshold", "Cat", function(object) return(object@infoThreshold))

#' @export
setGeneric("getGainThreshold", function(object) standardGeneric("getGainThreshold"))

setMethod("getGainThreshold", "Cat", function(object) return(object@gainThreshold))

#' @export
setGeneric("getLengthOverride", function(object) standardGeneric("getLengthOverride"))

setMethod("getLengthOverride", "Cat", function(object) return(object@lengthOverride))

#' @export
setGeneric("getGainOverride", function(object) standardGeneric("getGainOverride"))

setMethod("getGainOverride", "Cat", function(object) return(object@gainOverride) )

