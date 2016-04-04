#' Computerized Adaptive Testing Survey Debug Next Item Function
#'
#' This function enables user to debug a next item based on an estimate of a respondent's position by plotting difficulty of next item with respect to its expected posterior variance.
#'
#' @param cat An object of class \code{CATsurv}
#' @param theta.est A scalar value to contain an estimate of a respondent's position on the latent trait, using the \code{\link{estimateTheta}} funciton. Defaults to NA.
#' @param D A numeric value used as model parameter.  For logit models, set D=1.  For an approximation of the probit model, set D=1.702.  Defaults to D=1. 
#' @param D A numeric value used as model parameter.  For logit models, set D=1.  For an approximation of the probit model, set D=1.702.  Defaults to D=1.   
#' @param lowerBound The lower bound of the interval of the latent trait used in estimation.  Defaults to -4.
#' @param upperBound The upper bound of the interval of the latent trait used in estimation.  Defaults to 4.
#' @param quadPoints The number of points used in approximating the integral.  Defaults to 33.
#' @param answer The answer to the item \emph{k} to be stored
#'
#' @return A next item to be asked based on the estimate of a respondent's position on the latent trait
#'  
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}},  \code{\link{nextItem}}, \code{\link{storeAnswer}}
#' @rdname debugNextItem
#' @export
setGeneric("debugNextItem", function(cat, theta.est=NA, D=1, lowerBound=-4, upperBound=4, quadPoints=33){standardGeneric("debugNextItem")})

#' @export
setMethod(f="debugNextItem", signature="CATsurv", definition=function(cat, theta.est=NA, D=1, lowerBound=-4, upperBound=4, quadPoints=33) {
  if (is.na(theta.est)) {
    theta.est = estimateTheta(cat, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
  }
  
  next.item = nextItem(cat, theta.est, D=D, lowerBound=lowerBound, upperBound=upperBound, quadPoints=quadPoints)
  
  plot(next.item$all.estimates$difficulty, next.item$all.estimates$epv, type="n", xlab="Difficulty", ylab="EPV")
  lines(next.item$all.estimates$difficulty, next.item$all.estimates$epv)
  segments(theta.est, 0, theta.est, 10, lty=2)
  points(next.item$all.estimates[next.item$next.item,]$difficulty, next.item$all.estimates[next.item$next.item,]$epv, col="red")
  
  return(next.item)
})