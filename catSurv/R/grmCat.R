#' Computerized Adaptive Testing Graded Response Model
#'
#' This function fits the Graded Response model for ordinal polytomous data and populates the fitted values for discimination and difficulty parameters to an object of class \code{Cat}.
#'
#' @param data a \code{data.frame} or a numeric \code{matrix} of manifest variables.
#' @param quadraturePoints a single numeric value to be passed into the grm function
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
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' @seealso
#' 
#' \code{\link{gpcmCat}} for an alternative model fit to ordinal polytomous data
#' 
#' @rdname grmCat
#' 
#' @import ltm
#' @export grm
#' @name grm-class
setOldClass("grm")

setGeneric("grmCat", function(data, quadraturePoints = NULL, ...){
  standardGeneric("grmCat")
})

#' @export
setMethod("grmCat",
          signature(data = "data.frame"),
          function(data, quadraturePoints = 21, ...){
            fit <- grm(data = data, control = list(GHk = quadraturePoints))
            coefficients <- fit$coef
            
            discm <- sapply(1:length(coefficients), function(i){
              coefficients[[i]][length(coefficients[[i]])]})
            names(discm) <- names(discm)
            
            diff <- lapply(1:length(coefficients), function(i){
              coefficients[[i]][-length(coefficients[[i]])]})
            names(diff) <- names(diff)
            
            if(any(discm < -5) || any(discm > 5)){
              warning("Measurement model poorly estimated: 
                      discrimination values outside of [-5, 5]")
              }
            
            for (i in 1:length(diff)){
              if (any(diff[[i]] < -5) || any(diff[[i]] > 5)){
                warning("Measurement model poorly estimated: 
                        difficulty values outside of [-5, 5]")
              }
            }
            
            object <- new("Cat")
            object@discrimination <- discm
            object@difficulty <- diff
            object@guessing <- rep(0, length(discm))
            object@answers <- rep(NA, length(discm))
            object@model <- "grm"
            return(object)
})


#' @export
setMethod("grmCat",
          signature(data = c("grm")),
          function(data, quadraturePoints = 21, ...){
            coefficients <- data$coef
            
            discm <- sapply(1:length(coefficients), function(i){
              coefficients[[i]][length(coefficients[[i]])]})
            names(discm) <- names(discm)
            
            diff <- lapply(1:length(coefficients), function(i){
              coefficients[[i]][-length(coefficients[[i]])]})
            names(diff) <- names(diff)
            
            if(any(discm < -5) || any(discm > 5)){
              warning("Measurement model poorly estimated: 
                      discrimination values outside of [-5, 5]")
              }
            for (i in 1:length(diff)){
              if(any(diff[[i]] < -5) || any(diff[[i]] > 5)){
                warning("Measurement model poorly estimated: 
                        difficulty values outside of [-5, 5]")
              }
            }
            
            object <- new("Cat")
            object@discrimination <- discm
            object@difficulty <- diff
            object@guessing <- rep(0, length(discm))
            object@answers <- rep(NA, length(discm))
            object@model <- "grm"
            return(object)
})
 