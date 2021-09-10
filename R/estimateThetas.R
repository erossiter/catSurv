#' Estimates of Ability Parameters for a Dataset of Response Profiles
#'
#' Estimates the expected value of the ability parameter \eqn{\theta}, conditioned on the observed answers, prior, and the item parameters
#' for complete response profiles for a dataset of respondents.
#'
#' @param catObj An object of class \code{Cat}
#' @param responses A dataframe of complete response profiles
#'
#' @return The function \code{estimateThetas} returns a vector containing respondents' estimated ability parameters.
#'
#' @details
#' 
#' Estimation approach is specified in \code{estimation} slot of \code{Cat} object.
#' 
#' The expected a posteriori approach is used when \code{estimation} slot is \code{"EAP"}.  This method involves integration.  See \strong{Note} for more information.
#' 
#' The modal a posteriori approach is used when \code{estimation} slot is \code{"MAP"}.  This method is only available using the normal prior distribution.
#' 
#' The maximum likelihood approach is used when \code{estimation} slot is \code{"MLE"}.  When the likelihood is undefined,
#' the MAP or EAP method will be used, determined by what is specified in the \code{estimationDefault} slot in \code{Cat} object.
#' 
#' The weighted maximum likelihood approach is used when \code{estimation} slot is \code{"WLE"}.
#' Estimating \eqn{\theta} requires root finding with the ``Brent'' method in the GNU Scientific
#'  Library (GSL) with initial search interval of \code{[-5,5]}.
#' 
#' @examples
#'## Loading ltm Cat object
#'data(ltm_cat)
#'
#'## Set different estimation procedures and estimate ability parameter
#'data(npi)
#'setEstimation(ltm_cat) <- "EAP"
#'estimateThetas(ltm_cat, responses = npi[1:25, ])
#'
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
#'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#'  
#' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
#' 
#' This function uses adaptive quadrature methods from the GNU Scientific
#'  Library (GSL) to approximate single-dimensional
#'  integrals with high accuracy.  The bounds of integration are determined by the
#'  \code{lowerBound} and \code{upperBound} slots of the \code{Cat} object.
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{estimateTheta}}
#' @name estimateThetas
NULL

setGeneric("estimateThetas", function(catObj, responses) standardGeneric("estimateThetas"))

#' @rdname estimateThetas
#' @export
setMethod(f = "estimateThetas", signature = "Cat", definition = function(catObj, responses){
    if(length(catObj@answers) != ncol(responses)){
        stop("Cat object not compatible with responses.")
    }
    if(!all(apply(responses, 2, function(x) all(is.numeric(x) | is.na(x))))) {
        stop("Responses need to be numeric.")
    }
    out <- apply(X = responses,
                 MARGIN = 1,
                 FUN = function(x, catObj){
                     catObj@answers <- unlist(x)
                     estimateTheta(catObj)
                 },
                 catObj = catObj)
    names(out) <- NULL
    return(out)
})


