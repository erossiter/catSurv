#' ICC plot
#'
#' This function fits the latent trait model for binary data and populates the fitted values for discimination, difficulty, and guessing parameters to an object of class \code{Cat}.
#'
#' @param data a \code{data.frame} or a numeric \code{matrix} of manifest variables.
#' @param object an object of class \code{Cat} to be populated. If omitted, a new object of class \code{Cat} is created.
#' @param ... arguments to be passed to methods. For more details about the arguments, see \code{\link{tpm}}.
#'
#'  @return An object of class \code{Cat} with components,
#' \itemize{
#' \item \code{difficulty} a named vector of difficulty parameters for use with dichotomous questions/items.  Each element's name tells the question/item to which it applies.
#' \item \code{guessing} a vector of guessing parameter for each item.
#' \item \code{discrimination} a vector of disrimination parameter for each item.
#' \item \code{answers} a vector of answers to questions as given by the survey respondent.
#' \item \code{priorName} a character vector of length one giving the prior distribution to use for the latent trait estimates.  The options are \code{normal} for the normal distirbution, \code{cauchy} for the Cauchy distribution, are \code{t} for the t-distribution. Defaults to \code{normal}.
#' \item \code{priorParams} a numeric vector of parameters for the distribution specified in the \code{priorName} slot. See the details section for more infomration.  Defaults to \code{c(0,1)}.
#' }
#'
#' @note In case the Hessian matrix at convergence is not positive definite try to use \code{start.val="random"}.
#'
#' @rdname icc
#' @export
setGeneric("icc", function(object, theta_range, question, ...){standardGeneric("icc")})

#' @export
setMethod(f="icc", signature="Cat",
          definition=function(object, theta_range = seq(-3, 3, .1), question, ...){
            if(!is.null(object)) if(class(object)!="Cat") stop("object is not class Cat")
            
            prob_given_theta <- function(i){
              probability(object, theta_range[i], question)$all.probabilities$probabilities
            }
            x <- sapply(1:length(theta_range), prob_given_theta)
            
            plot.new()
            plot.window(c(min(theta_range), max(theta_range)), c(0,1))
            title(main = paste0("Item Characteristic Curves for Question ", question),
                  xlab = "Theta",
                  ylab = "Probability")
            axis(1, at = seq(-3,3,1), labels = seq(-3,3,1))
            axis(2, at = seq(0,1,.25), labels = seq(0,1,.25), las = 1)
            legend("topright", c(paste0("Item ", 1:(nrow(x)+1))), lty = 1:(nrow(x)+1), bty = "n")
            
            lines(x = theta_range, y = (x[1, ] - 0), lty = 1)
            lines(x = theta_range, y = (1 - x[nrow(x), ]), lty = nrow(x)+1)
            for(i in 2:(nrow(x))){
              lines(x = theta_range, y = (x[i, ] - x[(i-1), ]), lty = i)
            }
          })
