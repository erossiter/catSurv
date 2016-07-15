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
#' Assume we have a survey battery with \code{I} questions.  An object of the class `Cat' has the following slots:
#' \itemize{
#' \item \code{guessing} A named vector of length \code{I} of guessing parameters.  Note: guessing parameters are only applicable for binary Cat objects. 
#' \item \code{discrimination} A named vector of length \code{I} of disrimination parameters.
#' \item \code{difficulty} A named vector or list of length \code{I} of difficulty parameters. For binary Cat objects, the vector will contain difficulty parameters for each item.  For categorical Cat objects, a list will constain a vector for each item, and each vector will contain a difficulty parameter for each response option.  
#' \item \code{answers} A named vector of length \code{I} of answers to questions as given by the survey respondent.  Unanswered questions have the value \code{NA}.
#' \item \code{priorName} A character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{NORMAL} for the normal distirbution, \code{STUDENT_T} for the student's t distribution, and \code{UNIFORM} for the uniform distribution.  
#' \item \code{priorParams} A numeric vector of length two of parameters for the distribution specified in the \code{priorName} slot. See 'details' for more information.
#' \item \code{lowerBound} A numeric indicating the lower bound of the interval of the latent scale used in estimation. The default value is \code{-4.5}.
#' \item \code{upperBound} A numeric indicating the upper bound of the interval of the latent scale used in estimation. The default value is \code{4.5}.
#' \item \code{poly} A logical containing the type of answers.  TRUE indicates categorical response options, FALSE indicates binary response options.  Default is TRUE.
#' \item \code{estimation} A character vector of length one indicating the choice of approach to estimate ability parameters.  The options are \code{EAP}, \code{MAP}, and \code{MLE}.
#' \item \code{estimationDefault} A character vector of length one indicating the choice of approach to estimate ability parameters when the primary estimation choice indicating in the \code{estimation} slot fails to converge.  The options are \code{EAP} and \code{MAP}.
#' \item \code{selection} A character vector of length one indicating the choice of approach select the next item.  The options are \code{EPV}, \code{MEI}, \code{MFI}, \code{MPWI}, \code{MLWI}, \code{KL}, \code{LKL}, \code{PKL}, \code{MFII}, and \code{RANDOM}.
#' \item \code{z} A numeric.  Used in calculating delta, which is used in calculating the bounds of integration for some selectItem methods.  See 'details' for more information.
#' \item \code{lengthThreshold} A numeric.  The number of questions answered must be greater than or equal to this threshold.
#' \item \code{seThreshold} A numeric.  The standard error estimate of the latent trait must be less than this threshold.
#' \item \code{infoThreshold} A numeric.  The Fisher's information for all remaining items must be less than this threshold.
#' \item \code{gainThreshold} A numeric.  The absolute value of the difference between the standard error of the latent trait estimate and the square root of the expected posterior variance for each item must be less than this threshold.
#' \item \code{lengthOverride} A numeric.  The number of questions answered must be less than this override.
#' \item \code{gainOverride} A numeric.  The absolute value of the difference between the standard error of the latent trait estimate and the square root of the expected posterior variance for each item must be less than this override.  
#' 
#' 
#' \item \code{points} ????
#' \item \code{quadPoints} ????
#' }
#'
#'@details When priorName is set to "NORMAL", the first element of priorParams is the mean, the second element is the standard deviation.  When priorName is set to "STUDENT_T", the first element of priorParam is mu, a location parameter, the second is degrees of freedom.  When priorName is set to "UNIFORM", the elements of priorParams are lower and upper, respectively.  Note that the uniform distribution is only applicable for the "EAP" estimation method.  
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
         slots=list(
           guessing="numeric",
           discrimination="numeric",
           answers="logicalORnumeric",
           priorName="character",
           priorParams="numeric",
           lowerBound="numeric",
           upperBound="numeric",
           quadPoints="numeric",
           difficulty="numericORlist",
           poly="logical",
           estimation="character",
           estimationDefault="character",
           selection="character",
           z="numeric",
           points="numeric",
           lengthThreshold = "logicalORnumeric",
           seThreshold = "logicalORnumeric",
           infoThreshold = "logicalORnumeric",
           gainThreshold = "logicalORnumeric",
           lengthOverride = "logicalORnumeric",
           gainOverride = "logicalORnumeric",
           strata = "logicalORnumeric"
         ),
         prototype=prototype(
           priorName="NORMAL",
           priorParams=c(0,1),
           lowerBound=-4.5,
           upperBound=4.5,
           quadPoints=43,
           poly=FALSE,
           estimation="EAP",
           estimationDefault="MAP",
           selection="EPV",
           z=0.9,
           points=40,
           answers=rep(NA, 10),
           discrimination=rep(0, 10),
           guessing=rep(0, 10),
           difficulty=rep(0, 10),
           lengthThreshold = NA,
           seThreshold = NA,
           infoThreshold = NA,
           gainThreshold = NA,
           lengthOverride = NA,
           gainOverride = NA,
           strata = rep(NA, 10)
         )
)


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
  if(!test0){return("discrimination needs length greater than 1")}

  test1<-(length(object@discrimination)==length(object@guessing))
  if(!test1){return("discrimination and guessing not same length")}

  test2<-(length(object@discrimination)==length(object@answers))
  if(!test2){return("discrimination and answers not same length")}

  test3<-(length(object@discrimination)==length(object@difficulty))
  if(!test3){return("discrimination and difficulty not same length")}

  ## If the Cat is polytomous, the difficulty needs to be list
  if(object@poly==T){
    if(class(object@difficulty)!="list"){return("Cat object is polytomous, but difficulty slot is not a list")}
  }
  else{
    if(class(object@difficulty)!="numeric"){return("Cat object is binary, but difficulty slot is not a numeric vector")}
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
      # if(!test4){return(paste("Repeated difficulty values for question ", which(object@difficulty==item, arr.ind=T)))}
      # test5<-(isTRUE(all.equal(i,sorted)))
      # if(!test5){return(paste("Diffulty values for question ", which(object@difficulty==item, arr.ind=T), " are not increasing"))}
      # test6<-(all(!is.na(item)))
      # if(!test6){return(paste("Diffulty values for question ", which(object@difficulty==item, arr.ind=T), " include NAs"))}
      
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



#' Allow us to access to the slots within Cat class objects
#'
#' These functions allow us to access to the values currently stored in a given slot of an object of class \code{Cat} with the user-provided values.  
#'
#' @param object An object of class \code{Cat} with a slot to be changed
#' 
#' @return A value of an object of class \code{Cat} with an updated slot
#' 
#' @note Notes
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{Cat}}
#' @rdname get
#' @export 
setGeneric("getcatSurv",
           function(object="Cat")  {
             standardGeneric("getcatSurv")
           })

#' @export
setMethod("getcatSurv", "Cat",
          function(object){
            return(object@Cat)
          })


#' @export
setGeneric("getguessing",
           function(object="Cat")  {
             standardGeneric("getguessing")
           })

#' @export
setMethod("getguessing", "Cat",
          function(object){
            return(object@guessing)
          })

#' @export
setGeneric("getdiscrimination",
           function(object="Cat")  {
             standardGeneric("getdiscrimination")
           })

#' @export
setMethod("getdiscrimination", "Cat",
          function(object){
            return(object@discrimination)
          })

#' @export
setGeneric("getpriorParams",
           function(object="Cat")  {
             standardGeneric("getpriorParams")
           })

#' @export
setMethod("getpriorParams", "Cat",
          function(object){
            return(object@priorParams)
          })

#' @export
setGeneric("getpriorName",
           function(object="Cat")  {
             standardGeneric("getpriorName")
           })


#' @export
setMethod("getpriorName", "Cat",
          function(object){
            return(object@priorName)
          })

#' @export
setGeneric("getlowerBound",
           function(object="Cat")  {
             standardGeneric("getlowerBound")
           })

#' @export
setMethod("getlowerBound", "Cat",
          function(object){
            return(object@lowerBound)
          })


#' @export
setGeneric("getupperBound",
           function(object="Cat")  {
             standardGeneric("getupperBound")
           })

#' @export
setMethod("getupperBound", "Cat",
          function(object){
            return(object@upperBound)
          })

#' @export
setGeneric("getquadPoints",
           function(object="Cat")  {
             standardGeneric("getquadPoints")
           })

#' @export
setMethod("getquadPoints", "Cat",
          function(object){
            return(object@quadPoints)
          })


#' @export
setGeneric("getdifficulty",
           function(object="Cat")  {
             standardGeneric("getdifficulty")
           })

#' @export
setMethod("getdifficulty", "Cat",
          function(object){
            return(object@difficulty)
          })

#' @export
setGeneric("getpoly",
           function(object="Cat")  {
             standardGeneric("getpoly")
           })

#' @export
setMethod("getpoly", "Cat",
          function(object){
            return(object@poly)
          })

#' @export
setGeneric("getestimation",
           function(object="Cat")  {
             standardGeneric("getestimation")
           })

#' @export
setMethod("getestimation", "Cat",
          function(object){
            return(object@estimation)
          })

#' @export
setGeneric("getselection",
           function(object="Cat")  {
             standardGeneric("getselection")
           })

#' @export
setMethod("getselection", "Cat",
          function(object){
            return(object@selection)
          })



#' @export
setGeneric("getpoints",
           function(object="Cat")  {
             standardGeneric("getpoints")
           })

#' @export
setMethod("getpoints", "Cat",
          function(object){
            return(object@points)
          })


