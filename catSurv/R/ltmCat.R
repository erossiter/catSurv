#' Computerized Adaptive Testing Latent Trait Model
#'
#' This function fits the latent trait model for binary data and populates the fitted values for discimination and difficulty parameters to an object of class \code{Cat}.
#'
#' @param data A data frame of manifest variables or an object of class \code{ltm}.
#' @param quadraturePoints A numeric to be passed into the \code{ltm} function indicating the number of Gauss-Hermite quadrature points.  Only applicable when \code{data} is a data frame.  Default value is \code{21}.
#' @param ... arguments to be passed to methods. For more details about the arguments, see \code{ltm} in the \code{ltm} package.
#'
#'@return The function \code{ltmCat} returns an object of class \code{Cat} with changes to the following slots:
#' \itemize{
#' \item \code{difficulty} A vector consisting of difficulty parameters for each item.
#' \item \code{discrimination} A vector consisting of disrimination parameters for each item.
#' \item \code{model} The string \code{"ltm"}, indicating this \code{Cat} object corresponds to a latent trait model.
#' }
#'
#' @note In case the Hessian matrix at convergence is not positive definite try to use \code{start.val = "random"}.
#' 
#' 
#' @examples
#' \dontrun{
#' ## Creating Cat object with raw data
#' data(npi)
#' ltm_cat1 <- ltmCat(npi, quadraturePoints = 100)
#' 
#' ## Creating Cat object with fitted object of class tpm
#' ltm_fit <- grm(npi, control = list(GHk = 100)) ## from ltm package
#' class(ltm_fit)
#' ltm_cat2 <- ltmCat(ltm_fit)
#' 
#' ## Note the two Cat objects are identical
#' identical(ltm_cat1, ltm_cat2)
#' 
#' ## Note the slots that have changed from default values
#' ltm_cat1@model
#' ltm_cat1@difficulty
#' ltm_cat1@discrimination
#'}
#' 
#' @references 
#' 
#' Baker, Frank B. and Seock-Ho Kim. 2004. Item Response Theory: Parameter Estimation Techniques. New York: Marcel Dekker.
#' 
#' Rizopoulos, Dimitris. 2006. ``ltm: An R Package for Latent Variable Modeling and Item Response Theory Analyses." Journal of Statistical Software 17(5):1-25.
#' 
#' 
#' 
#' @details The \code{data} argument of the function \code{ltmCat} is either a data frame or an object of class \code{ltm} from the \code{ltm} package.  If it is a data frame each row represents a respondent and each column represents a question item.  If it is an object of the class \code{ltm}, it is output from the \code{ltm} function in the \code{ltm} package.
#' 
#' The \code{quadraturePoints} argument of the function \code{ltmCat} is used only when the \code{data} argument is a data frame.  \code{quadraturePoints} is then passed to the \code{ltm} function from the \code{ltm} package when fitting the latent trait model to the data and is used when approximating the value of integrals.
#' 
#' @seealso 
#' 
#' \code{\link{Cat-class}}, \code{\link{tpmCat}}
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' 
#' @rdname ltmCat
#' @aliases ltm
#' 
#' @import ltm
#' @export ltm
#' @name ltmCat
setOldClass("ltm")

setGeneric("ltmCat", function(data, quadraturePoints = NULL, ...){
  standardGeneric("ltmCat")
})


#' @rdname ltmCat
#' @export
setMethod("ltmCat",
          signature(data = "data.frame"),
          function(data, quadraturePoints = 21, ...){
            fit <- ltm(data ~ z1, control = list(GHk = quadraturePoints), ...)
            discm <- fit$coef[,"z1"]
            diff <- fit$coef[,"(Intercept)"]
            names(diff) <- rownames(fit$coef)
             
            if(any(discm < -5) || any(discm > 5)){
             warning("Measurement model poorly estimated: 
                     discrimination values outside of [-5, 5]")
            }
            if(any(diff < -5) || any(diff > 5)){
             warning("Measurement model poorly estimated: 
                       difficulty values outside of [-5, 5]")
            }
             
            object <- new("Cat")
            object@discrimination <- discm
            object@difficulty <- diff
            object@guessing <- rep(0, length(discm))
            object@answers <- rep(NA,length(discm))
            object@model <- "ltm"
            return(object)
})

#' @rdname ltmCat
#' @export
setMethod("ltmCat",
          signature(data = c("ltm")),
          function(data, quadraturePoints = NULL, ...){
            discm <- data$coef[,"z1"]
            diff <- data$coef[,"(Intercept)"]
            names(diff) <- rownames(data$coef)
            
            if(any(discm < -5) || any(discm > 5)){
               warning("Measurement model poorly estimated: 
                       discrimination values outside of [-5, 5]")
            }
            if(any(diff < -5) || any(diff > 5)){
             warning("Measurement model poorly estimated: 
                     difficulty values outside of [-5, 5]")
            }
             
            object <- new("Cat")
            object@discrimination <- discm
            object@difficulty <- diff
            object@guessing <- rep(0, length(discm))
            object@answers <- rep(NA,length(discm))
            object@model <- "ltm"
            return(object)
})
