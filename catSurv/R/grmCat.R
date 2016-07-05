#' Computerized Adaptive Testing Graded Response Model
#'
#' This function fits the Graded Response model for ordinal polytomous data and populates the fitted values for discimination and difficulty parameters to an object of class \code{Cat}.
#'
#' @param data a \code{data.frame} or a numeric \code{matrix} of manifest variables.
#' @param object an object of class \code{Cat} to be populated. If omitted, a new object of class \code{Cat} is created.
#' @param ... arguments to be passed to methods. For more details about the arguments, see \code{\link{grm}}.
#'
#'  @return An object of class \code{Cat} with components,
#' \itemize{
#' \item \code{difficulty} a named list of difficulty parameters for use with polytomous questions/items.  Each element's name tells the question/item to which it applies.
#' \item \code{guessing} a vector of guessing parameter for each question/item.
#' \item \code{discrimination} a vector of disrimination parameter for each question/item.
#' \item \code{answers} a vector of answers to questions as given by the survey respondent.
#' \item \code{priorName} a character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{normal} for the normal distirbution, \code{cauchy} for the Cauchy distribution, are \code{t} for the t-distribution. Defaults to \code{normal}.
#' \item \code{priorParams} a numeric vector of parameters for the distribution specified in the \code{priorName} slot. See the details section for more infomration.  Defaults to \code{c(0,1)}.
#' }
#' @note In case the Hessian matrix at convergence is not positive definite try to use \code{start.val="random"}.
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{ltmCat}},\code{\link{nextItem}}, \code{\link{question.path}}
#' @rdname grmCat
#' @export
setGeneric("grmCat", function(data, object=NULL, quadraturePoints = 15, ...){standardGeneric("grmCat")})

#' @export
setMethod(f="grmCat", signature="data.frame",
          definition=function(data, object, quadraturePoints,...){
            if(is.null(object)){ ## if no Cat object provided, create a new Cat
              object<-new("Cat")
            }
            else  if(class(object)!="Cat"){ ## if the object provided is not a Cat, error
              stop("object is not class Cat")
            } 
            
            ## run grm function on the data
            fit <- grm(data=data, control = list(GHk = quadraturePoints))
            
            ## extract the parameters
            coefficients <- fit$coef
            ## coefficients is a list of parameter vectors, one vector for each question item
            ## the last element of each vector is discrimination; all elements before that are difficulty
            discrimination <- sapply(1:length(objects(coefficients)), function(i){
              return(coefficients[[i]][length(coefficients[[i]])])
            })
            names(discrimination)<-names(coefficients)
            
            difficulty <- lapply(1:length(objects(coefficients)), function(i){
              return(coefficients[[i]][-length(coefficients[[i]])])
            })
            names(difficulty)<-names(coefficients)
            
            
            
            ## check if parameters are out of expected range
            if (any(discrimination< -5) || any(discrimination>5)){
              warning("Measurement model poorly estimated: discrimination values outside of [-5, 5]")
            }
            for (i in 1:length(difficulty)){
              if (any(difficulty[[i]]< -5) || any(difficulty[[i]]>5)){
                warning(paste("Measurement model poorly estimated: difficulty values outside of [-5, 5];
                        See question item: ", names(difficulty)[i]))
                }}
            
            ## store those extracted parameters in the Cat
            object@discrimination <- discrimination
            object@difficulty <- difficulty
            
            ## by default (for grmCat), the Cat is polytomous with no guessing parameter
            object@poly <- TRUE
            object@guessing <- rep(0, length(discrimination))
            
            ## fill the answers slot with NAs
            object@answers <- rep(NA,length(objects(coefficients)))
            
            ## Cat is complete! Send it back
            return(object)
        
          }
          
)


setMethod(f="grmCat", signature="grm",
          definition=function(data, object, quadraturePoints,...){
            if(is.null(object)){ ## if no Cat object provided, create a new Cat
              object<-new("Cat")
            }
            else  if(class(object)!="Cat"){ ## if the object provided is not a Cat, error
              stop("object is not class Cat")
            } 
            
            ## data is of class 'grm'
            ## extract the parameters
            coefficients <- data$coef
            ## coefficients is a list of parameter vectors, one vector for each question item
            ## the last element of each vector is discrimination; all elements before that are difficulty
            discrimination <- sapply(1:length(objects(coefficients)), function(i){
              return(coefficients[[i]][length(coefficients[[i]])])
            })
            names(discrimination)<-names(coefficients)
            
            difficulty <- lapply(1:length(objects(coefficients)), function(i){
              return(coefficients[[i]][-length(coefficients[[i]])])
            })
            names(difficulty)<-names(coefficients)
            
            
            
            ## check if parameters are out of expected range
            if (any(discrimination< -5) || any(discrimination>5)){
              warning("Measurement model poorly estimated: discrimination values outside of [-5, 5]")
            }
            for (i in 1:length(difficulty)){
              if (any(difficulty[[i]]< -5) || any(difficulty[[i]]>5)){
                warning(paste("Measurement model poorly estimated: difficulty values outside of [-5, 5];
                        See question item: ", names(difficulty)[i]))
              }}
            
            ## store those extracted parameters in the Cat
            object@discrimination <- discrimination
            object@difficulty <- difficulty
            
            ## by default (for grmCat), the Cat is polytomous with no guessing parameter
            object@poly <- TRUE
            object@guessing <- rep(0, length(discrimination))
            
            ## fill the answers slot with NAs
            object@answers <- rep(NA,length(objects(coefficients)))
            
            ## Cat is complete! Send it back
            return(object)
            
          }
          
)


## i don't know what's going on here
setMethod(f="grmCat", signature="missing",
          definition=function(data, object,...){
            if(is.null(object)){
              fit <- grm(data=data, IRT.param = TRUE, control = list(GHk = 100),...)
            }
            if(!is.null(object)){
              if(class(object)!="grm"){
                stop("object is not class grm")
              } else {
                fit <- object
              }}
            
            coefficient <- fit$coef
            answer <- rep(NA,length(objects(coefficient)))
            discrimination <- sapply(1:length(objects(coefficient)), function(i) coefficient[[i]][length(coefficient[[i]])])
            names(discrimination) <- names(coefficient)
            difficulty <- lapply(1:length(objects(coefficient)), function(i) coefficient[[i]][-length(coefficient[[i]])])
            names(difficulty) <- names(coefficient)
            guessing <- rep(0, length(discrimination))
            #             if(is.null(object)){
            #               return(new("Cat", discrimination=discrimination, difficulty=difficulty, poly=TRUE, guessing=guessing, answers=answer))
            #             }
            #            else {
            
            object<-new("Cat")
            object@discrimination <- discrimination
            object@difficulty <- difficulty
            object@poly <- TRUE
            object@guessing <- guessing
            object@answers <- answer
            return(object)
          }
)









