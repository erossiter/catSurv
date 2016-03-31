#' Computerized Adaptive Testing Survey Expected Posterior Variance Estimator
#'
#' This function estimates the expected posterior variance for a respondent's estimated position on the latent trait on an item which he/she has yet to answer based on a respondent's position on the latent trait from the items he/she has already answered.  
#'
#' @param cat an object of class \code{CATsurv}
#' @param item The question for which to estimate the expected posterior variance for a respondent with a latent trait estimate of theta.hat.  This should be the name of a row in the "questions" data-frame in the "questions" slot of a \code{CATsurv} object.
#'
#' @return The expected posterior variance for respondent \emph{j} on item \emph{k}.  
#' @details The expected posterior variance is calculated as \deqn{P(y^*_{kj}=1|\mathbf{y}_{k-1,j})\text{Var}(\theta_j|\mathbf{y}_{k-1,j},y^*_{kj}=1)+P(y^*_{kj}=0|\mathbf{y}_{k-1,j})\text{Var}(\theta_j|\mathbf{y}_{k-1,j},y^*_{kj}=0)}, where \eqn{y_{kj}^*} is a possible response to item \emph{k} by respondent \emph{j}.
#' @author Josh W. Cutler: \email{josh@@zistle.com} and Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @seealso \code{\link{three.pl}},\code{\link{likelihood}}, \code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname expectedPV 
#' @export
setGeneric("expectedPV", function(cat, item){standardGeneric("expectedPV")})

#' @export
setMethod(f="expectedPV", signature=class.name, definition=function(cat, item) {
  if (cat@poly) {
#    browser()    
    row.name = item
    thetas = rep(NA, length(cat@difficulty[[row.name]]) + 1)
    variances = rep(NA, length(cat@difficulty[[row.name]]) + 1)


    for (i in 1:(length(cat@difficulty[[row.name]])+1)) {
      cat@answers[row.name] = i
      thetas[i] = estimateTheta(cat)
      variances[i] = estimateSE(cat, thetas[i])^2
    }
    cat@answers[row.name] = NA



    cat@Theta.est = estimateTheta(cat) ## Could comment this out
    this.question.cdf =  three.pl(cat, cat@Theta.est, cat@difficulty[[row.name]], cat@discrimination[row.name], cat@guessing[row.name])
    this.question.cdf = c(1, this.question.cdf, 0)
    this.question.pdf = rep(NA, length(this.question.cdf)-1)
    for (i in 2:(length(this.question.cdf))) {
      this.question.pdf[i-1] <- this.question.cdf[i-1] - this.question.cdf[i]
    }

    return (sum(variances * this.question.pdf))
  } else {
    prob.correct = three.pl(cat, cat@Theta.est, cat@difficulty[item], cat@discrimination[item], cat@guessing[item])
    prob.incorrect = 1 - prob.correct
    
    old_val = cat@answers[item]
    
    cat@answers[item] = 1
    theta.correct = estimateTheta(cat)
    variance.correct = estimateSE(cat, theta.correct)^2
    
    cat@answers[item] = 0
    theta.incorrect = estimateTheta(cat)
    variance.incorrect = estimateSE(cat, theta.incorrect)^2
    
    cat@answers[item] = if (is.null(old_val) || is.na(old_val)) NA else old_val
    
    return(prob.correct*variance.correct + prob.incorrect*variance.incorrect)
  }
})
