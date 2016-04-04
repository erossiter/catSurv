#' Computerized Adaptive Testing Graded Response Model
#'
#' This function fits the Graded Response model for ordinal polytomous data and populates the fitted values for discimination and difficulty parameters to an object of class \code{CATsurv}.  
#'
#' @param data a \code{data.frame} or a numeric \code{matrix} of manifest variables. 
#' @param object an object of class \code{CATsurv} to be populated. If omitted, a new object of class \code{CATsurv} is created.
#' @param ... arguments to be passed to methods. For more details about the arguments, see \code{\link{grm}}.
#'
#'  @return An object of class \code{CATsurv} with components,
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
#' @seealso \code{\link{ltmCAT}},\code{\link{nextItem}}, \code{\link{question.path}}
#' @rdname grmCAT
#' @export
setGeneric("grmCAT", function(data, object=NULL, ...){standardGeneric("grmCAT")})

#' @export
setMethod(f="grmCAT", signature="data.frame", 
          definition=function(data, object,...){
            if(!is.null(object)) if(class(object)!="CATsurv") stop("object is not class CATsurv")            
            fit <- grm(data=data, IRT.param = TRUE,...)
            coefficient <- coef(fit)
            answer <- rep(NA,nrow(coefficient))
            discrimination <- coefficient[,"Dscrmn"]
            difficulty <- lapply(1:nrow(coefficient), function(i) coefficient[i,-ncol(coefficient)])
            names(difficulty) <- rownames(coefficient)
            guessing <- rep(0, length(discrimination))
            if(is.null(object)){
              return(new("CATsurv", discrimination=discrimination, difficulty=difficulty, poly=TRUE, guessing=guessing, answers=answer))
            } else {
              object@discrimination <- discrimination
              object@difficulty <- difficulty
              object@poly <- TRUE
              object@guessing <- 0
              object@answers <- answer
              return(object)
            }
          })
