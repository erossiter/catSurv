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
#' \item \code{difficulty} A named list consisting of a vector of difficulty parameters for each item. (vector will be length=1 for binary Cats)
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
#' @seealso \code{\link{setGuessing}}, \code{\link{setDiscrimination}}
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
           priorName="NORMAL",
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
           answers=c(NA, NA),
           discrimination=c(0,0),
           guessing=c(0,0),
           difficulty=c(0,0)
         )
)


#' @export
setMethod("initialize", class.name, function(.Object, ...) {
  value = callNextMethod()
  value@X <- seq(from=value@lowerBound,to=value@upperBound,length=value@quadPoints)
  validObject(value)
  return(value)
})

########### VALIDITY CHECKS ##############

setValidity("Cat", function(object){
  # guessing, discrimination, answers, difficulty should all be same length
  #... and that length must be greater than 1
  test0<-(length(object@discrimination)>1)
  if(!test0){return("discrimination needs length greater than 1")}
      
  test1<-(length(object@discrimination)==length(object@guessing))  
  if(!test1){return("discrimination and guessing not same length")}
  
  test2<-(length(object@discrimination)==length(object@answers))  
  if(!test2){return("discrimination and answers not same length")}
  
  test3<-(length(object@discrimination)==length(object@difficulty))  
  if(!test3){return("discrimination and difficulty not same length")}
  
  ## If the Cat is polytomous, the difficulty needs to be list
  if(object@poly==T){
    if(class(object@difficulty)!=list){return("Cat object is polytomous, but difficulty slot is not a list")}
  }
  else{
    if(class(object@diffiuclty)!=numeric){return("Cat object is binary, but difficulty slot is not a numeric vector")}
  }
  
  
  ## TEST THAT DIFFICULTY VALUES ARE STRICTLY INCREASING, and not NA
  if(object@poly==T){
    for(i in object@difficulty){
      if (is.list(i)){
        item<-unlist(i)
      }
      item<-i #don't want to change the value stored in the object itself...
      sorted<-sort(item)
      uniques<-unique(item)
      test4<-(isTRUE(all.equal(item,uniques)))
      if(!test4){return(paste("Repeated difficulty values for question ", which(object@difficulty==item, arr.ind=T)))}
      test5<-(isTRUE(all.equal(i,sorted)))
      if(!test5){return(paste("Diffulty values for question ", which(object@difficulty==item, arr.ind=T), " are not increasing"))}
      test6<-(all(!is.na(item)))
      if(!test6){return(paste("Diffulty values for question ", which(object@difficulty==item, arr.ind=T), " include NAs"))}
      
    }
  }
  
  ## test that discrimination and guessing are not NA
  for(i in object@discrimination){
    test7<-!is.na(i)
    if(!test7){return(paste("Discrimination value for question ", which(object@discrimination==i, arr.ind=T), " is NA"))}
  }
  if(!object@poly){
    for(i in object@guessing){
      test8<-!is.na(i)
      if(!test8){return(paste("Discrimination value for question ", which(object@discrimination==i, arr.ind=T), " is NA"))}
    }
  }
})


########### SETTERS  ##############


#' Setter for guessing slot within Cat class objects
#'
#' This functions replaces the values currently stored in the guessing slot of an object of class \code{Cat} with the user-provided values.  
#'
#' @param object An object of class \code{Cat} to be updated
#' @param valid Boolean for whether to check validity of object before allowing setter method to execute, default to TRUE
#' @param value The value to replace the current value stored in the guessing slot of the \code{Cat}
#' 
#' @return Updates the \code{Cat} object that was provided by the user, and returns the updated object 
#' 
#' @note There are setters for every slot in \code{Cat} objects, all following the format \code{setSlot}
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{Cat}}, \code{\link{setDiscrimination}}, \code{\link{setPriorParam}}
#' @rdname setGuessing
#' @export
setGeneric("setGuessing<-",
		   function(object, valid=T, value){
		   	standardGeneric("setGuessing<-")
		   	})
setReplaceMethod(
	f = "setGuessing",
	signature = "Cat",
	definition = function(object, valid, value){
		object@guessing <- value
		if(valid){validObject(object)}
		return(object)
		})

#' Setter for discrimination slot within Cat class objects
#'
#' This functions replaces the values currently stored in the discrimination slot of an object of class \code{Cat} with the user-provided values.  
#'
#' @param object An object of class \code{Cat} to be updated
#' @param valid Boolean for whether to check validity of object before allowing setter method to execute, default to TRUE
#' @param value The value to replace the current value stored in the discrimination slot of the \code{Cat}
#' 
#' @return Updates the \code{Cat} object that was provided by the user, and returns the updated object 
#' 
#' @note There are setters for every slot in \code{Cat} objects, all following the format \code{setSlot}
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{Cat}}, \code{\link{setDiscrimination}}, \code{\link{setPriorParam}}
#' @rdname setDiscrimination
#' @export
setGeneric("setDiscrimination<-",
		   function(object, valid=T, value){
		   	standardGeneric("setDiscrimination<-")
		   	})
setReplaceMethod(
	f = "setDiscrimination",
	signature = "Cat",
	definition = function(object, valid, value){
		object@discrimination <- value
		if(valid){validObject(object)}
		return(object)
		})

#' Setter for priorParam slot within Cat class objects
#'
#' This functions replaces the values currently stored in the priorParam slot of an object of class \code{Cat} with the user-provided values.  
#'
#' @param object An object of class \code{Cat} to be updated
#' @param valid Boolean for whether to check validity of object before allowing setter method to execute, default to TRUE
#' @param value The value to replace the current value stored in the priorParam slot of the \code{Cat}
#' 
#' @return Updates the \code{Cat} object that was provided by the user, and returns the updated object 
#' 
#' @note There are setters for every slot in \code{Cat} objects, all following the format \code{setSlot}
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{Cat}}, \code{\link{setDiscrimination}}, \code{\link{setGuessing}}
#' @rdname setPriorParam
#' @export
setGeneric("setPriorParam<-",
		   function(object, valid=T, value){
		   	standardGeneric("setPriorParam<-")
		   	})
setReplaceMethod(
	f = "setPriorParam",
	signature = "Cat",
	definition = function(object, valid, value){
		object@priorParam <- value
		if(valid){validObject(object)}
		return(object)
		})

#' @export
setGeneric("setPriorName<-",
		   function(object, valid=T, value){
		   	standardGeneric("setPriorName<-")
		   	}) 
setReplaceMethod(
	f = "setPriorName",
	signature = "Cat",
	definition = function(object, valid, value){
		object@priorName <- value
		if(valid){validObject(object)}
		return(object)
		})

#' @export
setGeneric("setLowerBound<-",
		   function(object, valid=T, value){
		   	standardGeneric("setLowerBound<-")
		   	})
setReplaceMethod(
	f = "setLowerBound",
	signature = "Cat",
	definition = function(object, valid, value){
		object@lowerBound <- value
		if(valid){validObject(object)}
		return(object)
		})

#' @export
setGeneric("setUpperBound<-",
		   function(object, valid=T, value){
		   	standardGeneric("setUpperBound<-")
		   	})
setReplaceMethod(
	f = "setUpperBound",
	signature = "Cat",
	definition = function(object, valid, value){
		object@upperBound <- value
		if(valid){validObject(object)}
		return(object)
		})

#' @export
setGeneric("setQuadPoints<-",
		   function(object, valid=T, value){
		   	standardGeneric("setQuadPoints<-")
		   	})
setReplaceMethod(
	f = "setQuadPoints",
	signature = "Cat",
	definition = function(object, valid, value){
		object@quadPoints <- value
		if(valid){validObject(object)}
		return(object)
		})

#' @export
setGeneric("setDifficulty<-",
		   function(object, valid=T, value){
		   	standardGeneric("setDifficulty<-")
		   	})
setReplaceMethod(
	f = "setDifficulty",
	signature = "Cat",
	definition = function(object, valid, value){
		object@difficulty <- value
		if(valid){validObject(object)}
		return(object)
		})

#' @export
setGeneric("setPoly<-",
		   function(object, valid=T, value){
		   	standardGeneric("setPoly<-")
		   	})
setReplaceMethod(
	f = "setPoly",
	signature = "Cat",
	definition = function(object, valid, value){
		object@poly <- value
		if(valid){validObject(object)}
		return(object)
		})

#' @export
setGeneric("setIntegration<-",
		   function(object, valid=T, value){
		   	standardGeneric("setIntegration<-")
		   	})
setReplaceMethod(
	f = "setIntegration",
	signature = "Cat",
	definition = function(object, valid, value){
		object@integration <- value
		if(valid){validObject(object)}
		return(object)
		})

#' @export
setGeneric("setEstimation<-",
		   function(object, valid=T, value){
		   	standardGeneric("setEstimation<-")
		   	})
setReplaceMethod(
	f = "setEstimation",
	signature = "Cat",
	definition = function(object, valid, value){
		object@estimation <- value
		if(valid){validObject(object)}
		return(object)
		})

#' @export
setGeneric("setSelection<-",
		   function(object, valid=T, value){
		   	standardGeneric("setSelection<-")
		   	})
setReplaceMethod(
	f = "setSelection",
	signature = "Cat",
	definition = function(object, valid, value){
		object@selection <- value
		if(valid){validObject(object)}
		return(object)
		})

#' @export
setGeneric("setCoverage<-",
		   function(object, valid=T, value){
		   	standardGeneric("setCoverage<-")
		   	})
setReplaceMethod(
	f = "setCoverage",
	signature = "Cat",
	definition = function(object, valid, value){
		object@coverage <- value
		if(valid){validObject(object)}
		return(object)
		})

#' @export
setGeneric("setPoints<-",
		   function(object, valid=T, value){
		   	standardGeneric("setPoints<-")
		   	})
setReplaceMethod(
	f = "setPoints",
	signature = "Cat",
	definition = function(object, valid, value){
		object@points <- value
		if(valid){validObject(object)}
		return(object)
		})

