#' Computerized Adaptive Testing Generalized Partial Credit Model
#'
#' This function fits the Generalized Partial Credit model for ordinal polytomous data and populates the fitted values for discimination and difficulty parameters to an object of class \code{Cat}.
#'
#' @param data A \code{data.frame} of manifest variables or an object of class \code{gpcm}.
#' @param quadraturePoints A numeric to be passed into the \code{gpcm} function indicating the number of Gauss-Hermite quadrature points.  Only applicable when \code{data} is a \code{data.frame}.  Default value is \code{21}.
#' @param ... arguments to be passed to methods. For more details about the arguments, see \code{gpcm} in the \code{ltm} package.
#'
#'@return The function \code{gpcmCat} returns an object of class \code{Cat} with changes to the following slots:
#' \itemize{
#' \item \code{difficulty} A list of difficulty parameters, where each element in the list corresponds to the difficulty parameters for an item.
#' \item \code{discrimination} A vector consisting of of disrimination parameters for each item.
#' \item \code{model} The string \code{"gpcm"}, indicating this \code{Cat} object corresponds to a Generalized Partial Credit model.
#' }
#' 
#' 
#' @details The \code{data} argument of the function \code{gpcmCat} is either a \code{data.frame} or an object of class \code{gpcm} from the \code{ltm} package.  If it is a \code{data.frame} each row represents a respondent and each column represents a question item.  If it is an object of the class \code{gpcm}, it is output from the \code{gpcm} function in the \code{ltm} package.
#' 
#' The \code{quadraturePoints} argument of the function \code{gpcmCat} is used only when the \code{data} argument is of class \code{data.frame}.  \code{quadraturePoints} is then passed to the \code{gpcm} function from the \code{ltm} package when fitting the Generalized Partial Credit model to the data and is used when approximating the value of integrals.
#' 
#' 
#' @seealso 
#' 
#' \code{\link{Cat}} for information on all \code{Cat} slots and their default values
#' 
#' \code{\link{grmCat}} for an alternative model fit to ordinal polytomous data
#' 
#' @note In case the Hessian matrix at convergence is not positive definite try to use \code{start.val = "random"}.
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil
#' 
#' 
#' @rdname gpcmCat
#' 
#' @import ltm
#' @export gpcm
#' @name gpcm-class
setOldClass("gpcm")

setGeneric("gpcmCat", function(data, quadraturePoints = NULL, ...){
  standardGeneric("gpcmCat")
})

#' @export
setMethod("gpcmCat",
          signature(data = "data.frame"),
          function(data, quadraturePoints = 21, ...){
            fit <- gpcm(data = data, constraint = "gpcm",
                        control = list(GHk = quadraturePoints), ...)
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
            object@model <- "gpcm"
            return(object)
})


#' @export
setMethod("gpcmCat",
          signature(data = c("gpcm")),
          function(data, quadraturePoints = NULL, ...){
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
            object@model <- "gpcm"
            return(object)
})
