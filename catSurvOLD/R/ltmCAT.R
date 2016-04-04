#' Computerized Adaptive Testing Latent Trait Model with Binary Data
#'
#' This function fits the latent trait model for binary data and populates the fitted values for discimination, difficulty, and guessing parameters to an object of class \code{CATsurv}.  
#'
#' @param data a \code{data.frame} or a numeric \code{matrix} of manifest variables. 
#' @param object an object of class \code{CATsurv} to be populated. If omitted, a new object of class \code{CATsurv} is created.
#' @param ... arguments to be passed to methods. For more details about the arguments, see \code{\link{tpm}}.
#'
#'  @return An object of class \code{CATsurv} with components,
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
#' @seealso \code{\link{grmCAT}}, \code{\link{nextItem}}, \code{\link{question.path}}
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @rdname ltmCAT
#' @export
setGeneric("ltmCAT", function(data, object=NULL, ...){standardGeneric("ltmCAT")})

#' @export
setMethod(f="ltmCAT", signature="data.frame", 
          definition=function(data, object,...){
            if(!is.null(object)) if(class(object)!="CATsurv") stop("object is not class CATsurv")            
            fit <- tpm(data,...)
            coefficient <- coef(fit)
            answer <- rep(NA,nrow(coefficient))
            discrimination <- coefficient[,"Dscrmn"]
            difficulty <- coefficient[,"Dffclt"]
            guessing <- coefficient[,"Gussng"]
            names(difficulty) <- rownames(coefficient)
            if(is.null(object)){
              return(new("CATsurv", discrimination=discrimination, difficulty=difficulty, guessing=guessing, answers=answer))
            } else {
              object@discrimination <- discrimination
              object@difficulty <- difficulty
              object@guessing <- guessing
              object@answers <- answer
              return(object)
            }
          })