#' Computerized Adaptive Testing Survey Observed Information Function
#'
#' This function calculates the observed information for the given theta estimate over these \eqn{k-1} items.
#'
#' @param cat an object of \code{CATsurv} class.
#' @param theta vector consisting of each respondent's position on the latent scale of interest.
#' @param item-level parameters data frame containing discrimination parameter, guessing parameter, difficulty parameter, and answer for each item.
#'
#' @return A scalar value representing the expected Fisher's information.
#'
#' @details The scalar value of observed information measure is the negative of second derivative of the log likelihood function.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{expectedInfo}}
#' @rdname observedInfo

#' @export
setGeneric("observedInfo", function(cat, theta, items){standardGeneric("observedInfo")})

#' @export
setMethod(f="observedInfo", signature="CATsurv", definition=function(cat, theta, items) { 
  e.value<- exp(cat@D*cat@difficulty*(theta-cat@discrimination))
  p.theta<- cat@guessing+((cat@D-cat@guessing)*e.value^(cat@D*cat@difficulty*(theta-cat@discrimination))/(1+e.value))
  p.prime.theta<- cat@D*cat@difficulty*(1-cat@guessing)*(e.value/(1+e.value)^2)
  p.2prime.theta<- (cat@D^2*(cat@difficulty)^2*e.value*(1-e.value)*(1-cat@guessing))/(1+e.value)^3                        
  q.theta<- 1-p.theta
  observedInfo<- (p.theta*q.theta*p.prime.theta^2-(cat@answers-p.theta))*((p.theta*q.theta*p.2prime.theta)+(p.prime.theta^2*(p.theta-q.theta)))/(p.theta^2*q.theta^2)
  return(observedInfo[items])
})