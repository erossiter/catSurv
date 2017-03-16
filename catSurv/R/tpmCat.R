#' Computerized Adaptive Testing Latent Trait Model
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
#' @seealso
#' 
#' \code{\link{ltmCat}} for an alternative model fit to binary data
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
            fit <- tpm(data, control = list(GHk = quadraturePoints))

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
          function(data, quadraturePoints = 21, ...){
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


