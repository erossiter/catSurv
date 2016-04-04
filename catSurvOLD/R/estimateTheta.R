#' Computerized Adaptive Testing Survey Estimating Each Respondent's Position on the Latent Scale of Interest
#'
#' This function returns the expected \emph{a posteriori} (EAP) estimate of each respondent's popsition on the latent scale. 
#'
#' @param cat an object of class \code{CATsurv}.
#' @param ... argument passed to other functions.
#'
#' @return A vector consisting of the expected \emph{a posteriori} estimate for each respondent's position on the latent scale of interest. 
#'  
#' @details The expected \emph{a posteriori} (EAP) estimate of respondent \eqn{j}'s position on the latent scale is calculated as \deqn{\hat{\theta}_j^{(\text{EAP})}\equiv E(\theta_j|\mathbf{y}_{k-1j})=\frac{\int\theta_j\pi(\theta_j)L(\theta_j|\mathbf{y}_{k-1j}d\theta_j}{\int\pi(\theta_j)L(\theta_j|\mathbf{y}_{k-1j}d\theta_j}}.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname estimateTheta
#' @export
setGeneric("estimateTheta", function(cat, ...){standardGeneric("estimateTheta")})

#' @export
setMethod(f="estimateTheta", signature=class.name, definition=function(cat,...) {
  applicable_rows = which(!is.na(cat@answers))

  prior.values = prior(cat, cat@X, cat@priorName, cat@priorParams)
  likelihood.values = rep(NA, times=length(cat@X))

  for (i in 1:length(likelihood.values)) {
    likelihood.values[i] = likelihood(cat, cat@X[i], applicable_rows)
  }
  results = integrate.xy(cat@X, cat@X*likelihood.values*prior.values) / integrate.xy(cat@X, likelihood.values*prior.values)

  
  return(results)
})
