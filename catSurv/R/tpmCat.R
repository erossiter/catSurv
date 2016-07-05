#' Computerized Adaptive Testing Latent Trait Model with Binary Data
#'
#' This function fits the latent trait model for binary data and populates the fitted values for discimination, difficulty, and guessing parameters to an object of class \code{Cat}.
#'
#' @param data a \code{data.frame} or a numeric \code{matrix} of manifest variables.
#' @param object an object of class \code{Cat} to be populated. If omitted, a new object of class \code{Cat} is created.
#' @param ... arguments to be passed to methods. For more details about the arguments, see \code{\link{tpm}}.
#'
#'  @return An object of class \code{Cat} with components,
#' \itemize{
#' \item \code{difficulty} a named vector of difficulty parameters for use with dichotomous questions/items.  Each element's name tells the question/item to which it applies.
#' \item \code{guessing} a vector of guessing parameter for each item.
#' \item \code{discrimination} a vector of disrimination parameter for each item.
#' \item \code{answers} a vector of answers to questions as given by the survey respondent.
#' \item \code{priorName} a character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{normal} for the normal distirbution, \code{cauchy} for the Cauchy distribution, are \code{t} for the t-distribution. Defaults to \code{normal}.
#' \item \code{priorParams} a numeric vector of parameters for the distribution specified in the \code{priorName} slot. See the details section for more infomration.  Defaults to \code{c(0,1)}.
#' }
#'
#' @note In case the Hessian matrix at convergence is not positive definite try to use \code{start.val="random"}.
#'
#' @seealso \code{\link{grmCat}}, \code{\link{nextItem}}, \code{\link{question.path}}
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @rdname tpmCat
#' @export
setGeneric("tpmCat", function(data, object=NULL, quadraturePoints = 15, ...){standardGeneric("tpmCat")})

#' @export
setMethod(f="tpmCat", signature="data.frame",
          definition=function(data, object, quadraturePoints,...){
            if(is.null(object)){ ## if no Cat object provided, create a new Cat
              object<-new("Cat")
            }
            else  if(class(object)!="Cat"){ ## if the object provided is not a Cat, error
              stop("object is not class Cat")
            } 
            
            ## run tpm function on the data
            fit<-tpm(data, control=list(GHk = quadraturePoints))
            
            ## extract the parameters
            discrimination <- fit$coef[,"beta.2i"]
            difficulty <- fit$coef[,"beta.1i"]
            guessing <- coef(fit)[,"Gussng"]
            names(difficulty) <- rownames(fit$coef)
            
            ## check if any parameters out of expected range:
            if (any(discrimination< -5) || any(discrimination>5)){
              warning("Measurement model poorly estimated: discrimination values outside of [-5, 5]")
            }
            if (any(difficulty< -5) || any(difficulty>5)){
              warning("Measurement model poorly estimated: difficulty values outside of [-5, 5]")
            }
            if (any(guessing< 0) || any(guessing>1)){
              warning("Measurement model poorly estimated: guessing values outside of [0, 1]")
            }
            
            ## store those extracted parameters in the Cat
            object@discrimination <- discrimination
            object@difficulty <- difficulty
            object@guessing<- guessing
            
            ## by default (for tpmCat), the Cat is binary 
            object@poly <- FALSE
            
            ## fill the answers slot with NAs
            object@answers <- rep(NA,length(discrimination))
            
            ## Cat is complete! Send it back
            return(object)

          })

setMethod(f="tpmCat", signature="tpm",
          definition=function(data, object, quadraturePoints,...){
            if(is.null(object)){ ## if no Cat object provided, create a new Cat
              object<-new("Cat")
            }
            else  if(class(object)!="Cat"){ ## if the object provided is not a Cat, error
              stop("object is not class Cat")
            } 
            
            ## data is of class 'tpm'
            ## extract the parameters
            discrimination <- data$coef[,"beta.2i"]
            difficulty <- data$coef[,"beta.1i"]
            guessing <- coef(data)[,"Gussng"]
            names(difficulty) <- rownames(data$coef)
            
            
            ## check if any parameters out of expected range:
            if (any(discrimination< -5) || any(discrimination>5)){
              warning("Measurement model poorly estimated: discrimination values outside of [-5, 5]")
            }
            if (any(difficulty< -5) || any(difficulty>5)){
              warning("Measurement model poorly estimated: difficulty values outside of [-5, 5]")
            }
            if (any(guessing< 0) || any(guessing>1)){
              warning("Measurement model poorly estimated: guessing values outside of [0, 1]")
            }
            
            ## store those extracted parameters in the Cat
            object@discrimination <- discrimination
            object@difficulty <- difficulty
            object@guessing<- guessing
            
            ## by default (for tpmCat), the Cat is binary 
            object@poly <- FALSE
            
            ## fill the answers slot with NAs
            object@answers <- rep(NA,length(discrimination))
            
            ## Cat is complete! Send it back
            return(object)
            
          })



## i don't know what's going on here
setMethod(f="tpmCat", signature="missing",
          definition=function(data, object,...){
            if(is.null(object)){
              fit <- tpm(data, control = list(GHk = 100), ...)
            } 
            if(!is.null(object)){
              if(class(object)!="tpm"){ 
                stop("object is not class tpm")
              } else {
                fit <- object
              }
            }
            
            answer <- rep(NA,dim(fit$coef)[1])
            discrimination <- fit$coef[,"beta.2i"]
            difficulty <- fit$coef[,"beta.1i"]
            guessing <- coef(fit)[,"Gussng"]
            names(difficulty) <- rownames(fit$coef)
            
            object<-new("Cat")
            object@discrimination <- discrimination
            object@difficulty <- difficulty
            object@poly <- FALSE
            object@guessing <- guessing
            object@answers <- answer
            return(object)
          })
