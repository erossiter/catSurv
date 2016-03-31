#' Computerized Adaptive Testing Survey Probability Function
#'
#' This function returns the probability of a correct response for each respondent on item \eqn{i}.
#'
#' @param cat an object of \code{CATsurv} class.
#' @param theta vector consisting of each respondent's position on the latent scale of interest.
#' @param difficulty vector consisting of difficulty parameter for each item.
#' @param discrimination vector consisting of discrimination parameter for each item.
#' @param guessing vector consisting of guessing parameter for each item . 
#'
#' @return A vector consisting of the probability of a correct response for each respondent on item \eqn{i}.
#'
#' @details The probability of a correct response for respondent \eqn{j} on item \eqn{i} is \deqn{Pr(y_{ij}=1|\theta_j)=c+(1-c)\frac{\exp(Da_i(\theta_j-b_i))}{1+\exp(Da_i(\theta_j-b_i))}} where \eqn{\theta_j} is respondent \eqn{j}'s position on the latent scale of interest, \eqn{a_i} is item \eqn{i}'s discrimination parameter, \eqn{b_i} is item \eqn{i}'s difficulty parameter, and \eqn{c_i} is item \eqn{i}'s guessing parameter.
#'
#' @author Josh W. Cutler and Jacob M. Montgomery
#' @seealso \code{\link{likelihood}},\code{\link{prior.value}}, \code{\link{estimateTheta}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{nextItem}}, \code{\link{storeAnswer}}, \code{\link{debugNextItem}}
#' @rdname three.pl
#' @export
setGeneric("three.pl", function(cat, theta, difficulty, discrimination, guessing){standardGeneric("three.pl")})

#' @export 
setMethod(f="three.pl", signature="CATsurv",
          definition=function(cat, theta, difficulty, discrimination, guessing) {
            exp.portion = exp(cat@D*discrimination*(theta - difficulty))
            prob = guessing + (1 - guessing)*(exp.portion / (1 + exp.portion))
            return(prob)
          }
          )

