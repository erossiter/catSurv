#' Computerized Adaptive Testing Birnbaum's Three Parameter Model
#'
#' This function fits Birnbaum's three parameter model for binary data and populates the fitted values for discimination, difficulty, and guessing parameters to an object of class \code{Cat}.
#'
#' @param data A \code{data.frame} of manifest variables or an object of class \code{tpm}.
#' @param quadraturePoints A numeric to be passed into the \code{tpm} function indicating the number of Gauss-Hermite quadrature points.  Only applicable when \code{data} is a \code{data.frame}.  Default value is \code{21}.
#' @param ... arguments to be passed to methods. For more details about the arguments, see \code{tpm} in the \code{ltm} package.
#'
#'@return The function \code{tpmCat} returns an object of class \code{Cat} with changes to the following slots:
#' \itemize{
#' \item \code{difficulty} A vector consisting of difficulty parameters for each item.
#' \item \code{discrimination} A vector consisting of disrimination parameters for each item.
#' \item \code{model} The string \code{"tpm"}, indicating this \code{Cat} object corresponds to Birnbaum's three parameter model.
#' }
#'
#' @note In case the Hessian matrix at convergence is not positive definite try to use \code{start.val = "random"}.
#' 
#' @references 
#' 
#' Baker, Frank B. and Seock-Ho Kim. 2004. Item Response Theory: Parameter Estimation Techniques. New York: Marcel Dekker.
#' 
#' Birnbaum, Allan. 1968. Some Latent Trait Models and their Use in Inferring an Examinee's Ability. In F. M. Lord and M. R. Novick (Eds.), Statistical Theories of Mental Test Scores, 397-479. Reading, MA: Addison-Wesley.
#' 
#' Rizopoulos, Dimitris. 2006. ``ltm: An R Package for Latent Variable Modeling and Item Response Theory Analyses." Journal of Statistical Software 17(5):1-25.
#' 
#' 
#' @examples
#' \dontrun{
#' ## Creating Cat object with raw data
#' data(polknow)
#' tpm_cat1 <- tpmCat(polknow, quadraturePoints = 100)
#' 
#' ## Creating Cat object with fitted object of class tpm
#' tpm_fit <- grm(polknow, control = list(GHk = 100)) ## from ltm package
#' class(tpm_fit)
#' tpm_cat2 <- tpmCat(tpm_fit)
#' 
#' ## Note the two Cat objects are identical
#' identical(tpm_cat1, tpm_cat2)
#' 
#' ## Note the slots that have changed from default values
#' tpm_cat1@model
#' tpm_cat1@difficulty
#' tpm_cat1@discrimination
#'}
#' 
#' 
#' @details The \code{data} argument of the function \code{tpmCat} is either a \code{data.frame} or an object of class \code{tpm} from the \code{ltm} package.  If it is a \code{data.frame} each row represents a respondent and each column represents a question item.  If it is an object of the class \code{tpm}, it is output from the \code{tpm} function in the \code{ltm} package.
#' 
#' The \code{quadraturePoints} argument of the function \code{tpmCat} is used only when the \code{data} argument is of class \code{data.frame}.  \code{quadraturePoints} is then passed to the \code{tpm} function from the \code{ltm} package when fitting Birnbaum's three parameter model to the data and is used when approximating the value of integrals.
#'
#' @seealso
#' 
#' \code{\link{Cat-class}}, \code{\link{ltmCat}}
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' @rdname tpmCat
#' 
#' @import ltm
#' @export tpm
#' @import stats
#' @name tpm-class
setOldClass("tpm")

setGeneric("tpmCat", function(data, quadraturePoints = NULL, ...){
  standardGeneric("tpmCat")
})

#' @export
setMethod("tpmCat",
          signature(data = "data.frame"),
          function(data, quadraturePoints = 21, ...){
            fit <- tpm(data, control = list(GHk = quadraturePoints), ...)

            discm <- fit$coef[,"beta.2i"]
            diff <- fit$coef[,"beta.1i"]
            guess <- coef(fit)[,"Gussng"]
            names(diff) <- rownames(fit$coef)

            if(any(discm < -5) || any(discm > 5)){
              warning("Measurement model poorly estimated: 
                      discrimination values outside of [-5, 5]")
            }
            if(any(diff < -5) || any(diff > 5)){
              warning("Measurement model poorly estimated: 
                      difficulty values outside of [-5, 5]")
            }
            if(any(guess < 0) || any(guess > 1)){
              warning("Measurement model poorly estimated: 
                      guessing values outside of [0, 1]")
            }
  
            object <- new("Cat")
            object@discrimination <- discm
            object@difficulty <- diff
            object@guessing <- guess
            object@answers <- rep(NA, length(discm))
            object@model <- "tpm"
            return(object)
})

#' @export
setMethod("tpmCat",
          signature(data = c("tpm")),
          function(data, quadraturePoints = NULL, ...){
            discm <- data$coef[,"beta.2i"]
            diff <- data$coef[,"beta.1i"]
            guess <- coef(data)[,"Gussng"]
            names(diff) <- rownames(data$coef)
            
            if(any(discm < -5) || any(discm > 5)){
              warning("Measurement model poorly estimated: 
                      discrimination values outside of [-5, 5]")
            }
            if(any(diff < -5) || any(diff > 5)){
              warning("Measurement model poorly estimated: 
                      difficulty values outside of [-5, 5]")
            }
            if(any(guess < 0) || any(guess > 1)){
              warning("Measurement model poorly estimated: 
                      guessing values outside of [0, 1]")
            }
  
            object <- new("Cat")
            object@discrimination <- discm
            object@difficulty <- diff
            object@guessing <- guess
            object@answers <- rep(NA, length(discm))
            object@model <- "tpm"
            return(object)
})


