#' Computerized Adaptive Testing Survey Estimating Each Respondent's Position on the Latent Scale of Interest - Maximum Likelihood method
#'
#' This function returns the maximum likelihood (ML) estimate of each respondent's popsition on the latent scale. 
#'
#' @param cat an object of class \code{CATsurv}.
#' @param ... arguement passed to other functions
#'
#' @return A scalar with the ML estimate of the respondent's place on the latent scale  
#'  
#' @details The ML estimate is based off of the likelihood funciton \deqn{L(\theta|\mathbf{y}_{k-1}) = \prod_{i=1}^{k-1}p_i(\theta_j)^{y_ij}q_i(\theta_j)^{(1-y_{ij})}}.  This procedure does not produce finite estimates for response patters with all items correct or all items incorrect.  
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname estimateThetaML
#' @export
setGeneric("estimateThetaML", function(cat, ...){standardGeneric("estimateThetaML")})

#' @export
setMethod(f="estimateThetaML", signature="CATsurv", definition=function(cat,...) {
  applicable_rows = which(!is.na(cat@answers))

  #make a new version of the likelihood function which is A. logged and B. has theta as the first arguement
  log.likelihood <- function(theta,cat,items){
  probabilities = three.pl(cat, theta, cat@difficulty[items], cat@discrimination[items], cat@guessing[items])
  L = log(prod(probabilities^cat@answers[items] * (1 - probabilities)^(1 - cat@answers[items])))
  }
    
  #optimize this log likelihood function w.r.t theta.
  optimize(f=log.likelihood,interval=c(cat@lowerBound,cat@upperBound),cat=cat,items=applicable_rows,maximum=TRUE)$maximum
}
)