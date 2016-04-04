#' Computerized Adaptive Testing Survey Expected Information Function
#'
#' This function calculates the expected Fisher's information for the given theta estimate over these \eqn{k-1} items.
#'
#' @param cat an object of \code{CATsurv} class.
#' @param theta vector consisting of each respondent's position on the latent scale of interest.
#' @param item-level parameters data frame containing discrimination parameter, guessing parameter, difficulty parameter, and answer for each item.
#'
#' @return A scalar value representing the expected Fisher's information.
#'
#' @details The scalar value of expected Fisher's information calculated by van der Linden and Parsley sum formula where nominator is calculated by \deqn{(\frac{\partial p(\theta)}{\partial \theta})^2=Da_i(1-c_i)\frac{exp[Da_i(\theta-b_i)]}{(1+exp[Da_i(\theta-b_i)])^2}} and denominator is calculated by calling the three.pl function for \eqn{p_i(\theta_j)} and \eqn{q_i(\theta_j)}.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{likelihood}},\code{\link{prior}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname expectedInfo

#' @export
setGeneric("expectedInfo", function(cat, theta, items){standardGeneric("expectedInfo")})

#' @export
setMethod(f="expectedInfo", signature="CATsurv", definition=function(cat, theta, items) {  
  num<- (cat@D*(1-cat@guessing[items])*exp(cat@D*cat@difficulty[items]*(theta-cat@discrimination[items]))/((1-exp(cat@D*cat@difficulty[items]*(theta-cat@discrimination[items])))^2))^2
  prob.p<- three.pl(cat, theta, cat@difficulty[items], cat@discrimination[items], cat@guessing[items])
  prob.q<- 1-prob.p
  denom<- prob.p*prob.q
  expectedInfo<- sum(num/denom)
  return(expectedInfo)
})

