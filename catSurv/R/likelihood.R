#' Computerized Adaptive Testing Survey Likelihood Function
#'
#' This function returns the value of likelihood funtion asscociated with the responses to the first \eqn{k-1} items under a local independence assumption.
#'
#' @param cat an object of class \code{CATsurv}.
#' @param theta vector consisting of each respondent's position on the latent scale of interest
#' @param item-level parameters data frame containing discrimination parameter, guessing parameter, difficulty parameter, and answer for each item, 
#'
#' @return A vector of the value of the likelihood function associated with the responses to the first \eqn{k-1} items for each respondent.
#'  
#' @details Letting \eqn{q_i(\theta_j)=1-p_i(\theta_j)}, the likelihood function associated with the responses to the first \eqn{k-1} items under a local independence assumption is \deqn{L(\theta_j|\mathbf{y}_{k-1,j})=\prod^{k-1}_{i=1}p_i(\theta_j)^{Y_{ij}}q_i(\theta_j)^{(1-y_{ij}}}.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{three.pl}},\code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname likelihood

#' @export
setGeneric("likelihood", function(cat, theta, items){standardGeneric("likelihood")})

#' @export
setMethod(f="likelihood", signature=class.name, definition=function(cat, theta, items) {
  if (cat@poly) {
#    browser()
    probabilities = c()
    L = 1
    for (question in items) {
      this.question.cdf = three.pl(cat, theta, cat@difficulty[[question]], cat@discrimination[question], cat@guessing[question])
      this.question.cdf = c(1, this.question.cdf, 0)
      this.question.pdf = c()
      for (i in 2:(length(this.question.cdf))) {
        this.question.pdf[i-1] = this.question.cdf[i-1] - this.question.cdf[i]
      }    
      L = L * this.question.pdf[cat@answers[question]]
    }
  } else {
    probabilities = three.pl(cat, theta, cat@difficulty[items], cat@discrimination[items], cat@guessing[items])
    L = prod(probabilities^cat@answers[items] * (1 - probabilities)^(1 - cat@answers[items]))
  }
  return(L)
})

