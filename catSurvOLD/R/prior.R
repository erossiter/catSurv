#' Computerized Adaptive Testing Survey Prior Value Function
#'
#' This function returns the prior value for each repondent's position on the latent scale of interest. 
#'
#' @param cat an object of class \code{CATsurv}.
#' @param values vector consisting of quantile for each respondent. 
#' @param name character string for type of assumed distribution. \code{normal} for normal distribution, \code{cauchy} for Cauchy distribtuion, and \code{t} for t-distribution.
#' @param params vector of parameters for selected distribution. The first element for mean value, the second element for standard deviation, and the third element for degree of freedom.
#'
#' @return A vector consisting of prior value for each respondent according to the assumed distribution.
#'  
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname prior.value
#' @export
setGeneric("prior", function(cat, values, name, params){standardGeneric("prior")})

#' @export
setMethod(f="prior", signature=class.name, definition=function(cat, values, name, params) {
  prior.value = switch(name,
                       normal = dnorm(values, params[1], params[2]),
                       cauchy = dcauchy(values, params[1], params[2]),
                       t = 1 / params[2] * dt((values - params[1]) / params[2], df=params[3])
  )
  return(prior.value)
})
