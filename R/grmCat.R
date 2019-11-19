#' Computerized Adaptive Testing Graded Response Model
#'
#' This function fits the graded response model for ordinal polytomous data and populates the fitted values for discrimination and difficulty parameters to an object of class \code{Cat}.
#'
#' @param data A data frame of manifest variables or an object of class \code{grm}.
#' @param quadraturePoints A numeric to be passed into the \code{grm} function indicating the number of Gauss-Hermite quadrature points.  Only applicable when \code{data} is a data frame.  Default value is \code{21}.
#' @param ... arguments to be passed to methods. For more details about the arguments, see \code{grm} in the \code{ltm} package.
#'
#'
#'@return The function \code{grmCat} returns an object of class \code{Cat} with changes to the following slots:
#' \itemize{
#' \item \code{difficulty} A list of difficulty parameters, where each element in the list corresponds to the difficulty parameters for an item.
#' \item \code{discrimination} A vector consisting of discrimination parameters for each item.
#' \item \code{model} The string \code{"grm"}, indicating this \code{Cat} object corresponds to a graded response model.
#' }
#' 
#' See \code{\link{Cat-class}} for default values of \code{Cat} object slots.  See \strong{Examples} and \code{\link{setters}} for example code to change slot values.
#' 
#' @examples
#' \dontrun{
#' ## Creating Cat object with raw data
#' data(nfc)
#' grm_cat1 <- grmCat(nfc, quadraturePoints = 100)
#' 
#' ## Creating Cat object with fitted object of class grm
#' grm_fit <- grm(nfc, control = list(GHk = 100)) ## from ltm package
#' class(grm_fit)
#' grm_cat2 <- grmCat(grm_fit)
#' 
#' ## Note the two Cat objects are identical
#' identical(grm_cat1, grm_cat2)
#' }
#' 
#' ## Creating Cat objects from large datasets is computationally expensive
#' ## Load the Cat object created from the above code
#' data(grm_cat)
#' 
#' ## Slots that have changed from default values
#' getModel(grm_cat)
#' getDifficulty(grm_cat)
#' getDiscrimination(grm_cat)
#' 
#' ## Changing slots from default values
#' setEstimation(grm_cat) <- "MLE"
#' setSelection(grm_cat) <- "MFI"
#'
#' 
#' @references 
#' 
#' Baker, Frank B. and Seock-Ho Kim. 2004. Item Response Theory: Parameter Estimation Techniques. New York: Marcel Dekker.
#' 
#' Samejima, Fumiko. 1969. ``Estimation of Latent Ability Using a Response Pattern of Graded Scores." Psychometrika monograph supplement 34(4):100-114.
#' 
#' Rizopoulos, Dimitris. 2006. ``ltm: An R Package for Latent Variable Modeling and Item Response Theory Analyses.`` Journal of Statistical Software 17(5):1-25.
#' 
#' 
#' @details The \code{data} argument of the function \code{grmCat} is either a data frame or an object of class \code{grm} from the \code{ltm} package.  If it is a data frame each row represents a respondent and each column represents a question item.  If it is an object of the class \code{grm}, it is output from the \code{grm} function in the \code{ltm} package.
#' 
#' The \code{quadraturePoints} argument of the function \code{grmCat} is used only when the \code{data} argument is a data frame.  \code{quadraturePoints} is then passed to the \code{grm} function from the \code{ltm} package when fitting the graded response model to the data and is used when approximating the value of integrals.
#' 
#' 
#' @seealso 
#' 
#' \code{\link{Cat-class}}, \code{\link{gpcmCat}}, \code{\link{nfc}}, \code{\link{probability}}
#' 
#' @note In case the Hessian matrix at convergence is not positive definite try to use \code{start.val = "random"}.
#' 
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' 
#' 
#' 
#' @rdname grmCat
#' @aliases grm
#' 
#' @import ltm
#' @export grm
#' @name grmCat
setOldClass("grm")

setGeneric("grmCat", function(data, quadraturePoints = NULL, ...){
  standardGeneric("grmCat")
})

#' @rdname grmCat
#' @export
setMethod("grmCat",
          signature(data = "data.frame"),
          function(data, quadraturePoints = 21, ...){
              fit <- grm(data = data, control = list(GHk = quadraturePoints), ...)
              coefficients <- fit$coef

              discm <- sapply(1:length(coefficients), function(i){
                  coefficients[[i]][length(coefficients[[i]])]
              })

              diff <- lapply(1:length(coefficients), function(i){
                  out <- coefficients[[i]][-length(coefficients[[i]])]
                  names(out) <- NULL
                  return(out)
              })

              object <- new("Cat")
              object@ids <- colnames(data)
              names(diff) <- names(discm) <- object@ids
              
              object@discrimination <- discm
              object@difficulty <- diff
              object@guessing <- rep(0, length(discm))
              object@answers <- rep(NA, length(discm))
              object@model <- "grm"
              return(object)
})

#' @rdname grmCat
#' @export
setMethod("grmCat",
          signature(data = c("grm")),
          function(data, quadraturePoints = NULL, ...){
              coefficients <- data$coef
              
              discm <- sapply(1:length(coefficients), function(i){
                  coefficients[[i]][length(coefficients[[i]])]
              })

              diff <- lapply(1:length(coefficients), function(i){
                  out <- coefficients[[i]][-length(coefficients[[i]])]
                  names(out) <- NULL
                  return(out)
              })

              object <- new("Cat")
              object@ids <- names(data$coef)
              names(diff) <- names(discm) <- object@ids
              
              object@discrimination <- discm
              object@difficulty <- diff
              object@guessing <- rep(0, length(discm))
              object@answers <- rep(NA, length(discm))
              object@model <- "grm"
              return(object)
})
 