#' Computerized Adaptive Testing Survey Store Answer Function
#'
#' This function updates the object of class \code{CATsurv} by storing the answer to item \emph{k} into questions data frame.
#'
#' @param cat An object of class \code{CATsurv}
#' @param item The question for which to estimate the expected posterior variance for a respondent with a latent trait estimate of theta.hat.  This should be the name of a row in the "questions" data-frame in the "questions" slot of a \code{CATsurv} object.
#' @param answer The answer to the item \emph{k} to be stored
#'
#' @return An updated object of class \code{CATsurv} containing the answers to \emph{k} items
#'  
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}},  \code{\link{nextItem}}, \code{\link{debugNextItem}}
#' @rdname storeAnswer
#' @export
setGeneric("storeAnswer", function(cat, item, answer){standardGeneric("storeAnswer")})

#' @export
setMethod(f="storeAnswer", signature=class.name, definition=function(cat, item, answer) {
  cat@answers[item] <- answer
  return(cat)
})