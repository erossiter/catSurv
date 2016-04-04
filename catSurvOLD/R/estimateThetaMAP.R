#' Computerized Adaptive Testing Survey Maximum A Posteriori Function
#'
#' This function estimates the maximum value of the posterior density of theta.
#'
#' @param cat an object of \code{CATsurv} class.
#'
#' @return A scalar valur representing the maximum value of the posterior density of theta.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{likelihood}},\code{\link{prior}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname estimateThetaMAP

#' @export
setGeneric("estimateThetaMAP", function(cat){standardGeneric("estimateThetaMAP")})

#' @export
setMethod(f="estimateThetaMAP", signature="CATsurv", definition=function(cat) {  
  applicable_rows = which(!is.na(cat@answers))
  
  posterior<- function(theta, cat, items){
    likelihood(cat, theta, items)*prior(cat, theta, cat@priorName, cat@priorParams)
  }
    optimize(f=posterior,interval=c(cat@lowerBound,cat@upperBound),cat=cat,items=applicable_rows,maximum=TRUE)$maximum
})