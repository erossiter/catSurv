#include <Rcpp.h>
#include "Cat.h"
#include <boost/variant.hpp>
using namespace Rcpp;

/**
 * To enable the usage of R-style methods (i.e. functions that take their object as their first parameter),
 * this file contains a direct mapping from the functions called by RCpp to methods of the Cat class.
 * In other words, every method in this file converts an S4 Cat object to a C++ Cat object, then calls the appropriate
 * method of the C++ Cat object.
 */

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]

//' Probability of Responses to a Question Item or the Left-Cumulative Probability of Responses
//'
//' Calculates the probability of specific responses or the left-cumulative probability of responses to \code{item} conditioned on a respondent's ability (\eqn{\theta}).  
//'
//' @param catObj An object of class \code{Cat}
//' @param theta A numeric or an integer indicating the value for \eqn{\theta_j}
//' @param item An integer indicating the index of the question item
//'
//' @return When the \code{model} slot of the \code{catObj} is \code{"ltm"}, the function \code{probability} returns a numeric vector of length one representing the probability of observing a non-zero response.
//'
//'When the \code{model} slot of the \code{catObj} is \code{"tpm"}, the function \code{probability} returns a numeric vector of length one representing the probability of observing a non-zero response.
//'
//' When the \code{model} slot of the \code{catObj} is \code{"grm"}, the function \code{probability} returns a numeric vector of length k+1, where k is the number of possible responses. The first element will always be zero and the (k+1)th element will always be one. The middle elements are the cumulative probability of observing response k or lower.
//'
//'  When the \code{model} slot of the \code{catObj} is \code{"gpcm"}, the function \code{probability} returns a numeric vector of length k, where k is the number of possible responses. Each number represents the probability of observing response k.
//'
//' @details 
//'  For the \code{ltm} model, the probability of non-zero response for respondent \eqn{j} on item \eqn{i} is
//'  
//'  \deqn{Pr(y_{ij}=1|\theta_j)=\frac{\exp(a_i + b_i \theta_j)}{1+\exp(a_i + b_i \theta_j)}}
//'
//'  where \eqn{\theta_j} is respondent \eqn{j} 's position on the latent scale of interest, \eqn{a_i} is item \eqn{i} 's discrimination parameter,
//'  and \eqn{b_i} is item \eqn{i} 's difficulty parameter. 
//'  
//'  For the \code{tpm} model, the probability of non-zero response for respondent \eqn{j} on item \eqn{i} is
//'  
//'  \deqn{Pr(y_{ij}=1|\theta_j)=c_i+(1-c_i)\frac{\exp(a_i + b_i \theta_j)}{1+\exp(a_i + b_i \theta_j)}}
//'
//'  where \eqn{\theta_j} is respondent \eqn{j} 's position on the latent scale of interest, \eqn{a_i} is item \eqn{i} 's discrimination parameter,
//'  \eqn{b_i} is item \eqn{i} 's difficulty parameter, and \eqn{c_i} is item \eqn{i} 's guessing parameter. 
//'  
//'  For the \code{grm} model, the probability of a response in category \eqn{k} \strong{or lower} for respondent \eqn{j} on item \eqn{i} is
//' 
//'  \deqn{Pr(y_{ij} <  k|\theta_j)=\frac{\exp(\alpha_{ik} - \beta_i \theta_{ij})}{1+\exp(\alpha_{ik} - \beta_i \theta_{ij})}}{Pr(y_ij < k | \theta_j) = (exp(\alpha_ik - \beta_i \theta_ij))/(1 + exp(\alpha_ik - \beta_i \theta_ij))}
//'
//'  where \eqn{\theta_j} is respondent \eqn{j} 's position on the latent scale of interest, \eqn{\alpha_ik} the \eqn{k}-th element of item \eqn{i} 's difficulty parameter, 
//'  \eqn{\beta_i} is discrimination parameter vector for item \eqn{i}. Notice the inequality on the left side and the absence of guessing parameters.
//'
//'  For the \code{gpcm} model, the probability of a response in category \eqn{k} for respondent \eqn{j} on item \eqn{i} is
//' 
//'  \deqn{Pr(y_{ij} =  k|\theta_j)=\frac{\exp(\sum_{t=1}^k \alpha_{i} [\theta_j - (\beta_i - \tau_{it})])}
//'  {\sum_{r=1}^{K_i}\exp(\sum_{t=1}^{r} \alpha_{i} [\theta_j - (\beta_i - \tau_{it}) )}}
//'  
//'  
//'  where \eqn{\theta_j} is respondent \eqn{j} 's position on the latent scale of interest, \eqn{\alpha_i} is the discrimination parameter for item \eqn{i},
//'  \eqn{\beta_i} is the difficulty parameter for item \eqn{i}, and \eqn{\tau_{it}} is the category \eqn{t} threshold parameter for item \eqn{i}, with \eqn{k = 1,...,K_i} response options
//'  for item \eqn{i}.  For identification purposes \eqn{\tau_{i0} = 0} and \eqn{\sum_{t=1}^1 \alpha_{i} [\theta_j - (\beta_i - \tau_{it})] = 0}.
//'
//'@examples
//'## Loading ltm Cat object
//'## Probability for Cat object of the ltm model
//'data(ltm_cat)
//'probability(ltm_cat, theta = 1, item = 1)
//'
//'## Loading tpm Cat object
//'## Probability for Cat object of the tpm model
//'probability(tpm_cat, theta = 1, item = 1)
//'
//'## Loading grm Cat object
//'## Probability for Cat object of the grm model
//'probability(grm_cat, theta = 1, item = 1)
//'
//'## Loading gpcm Cat object
//'## Probability for Cat object of the gpcm model
//'probability(gpcm_cat, theta = -3, item = 2)
//'  
//' @seealso \code{\link{Cat-class}}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' @references 
//' Baker, Frank B. and Seock-Ho Kim. 2004. Item Response Theory: Parameter Estimation Techniques. New York: Marcel Dekker.
//' 
//' Choi, Seung W. and Richard J. Swartz. 2009. ``Comparison of CAT Item Selection Criteria for Polytomous Items." Applied Psychological Measurement 33(6):419-440.
//' 
//' Muraki, Eiji. 1992. ``A generalized partial credit model: Application of an EM algorithm." ETS Research Report Series 1992(1):1-30.
//' 
//' van der Linden, Wim J. 1998. ``Bayesian Item Selection Criteria for Adaptive Testing." Psychometrika 63(2):201-216.
//' 
//'  
//' @export
// [[Rcpp::export]]
std::vector<double> probability(S4 catObj, NumericVector theta, IntegerVector item) {
	Cat cat = Cat(catObj);
	double t = theta[0];
	int q = item[0];
	if(q == 0){
	  throw std::domain_error("Must use an item number applicable to Cat object.");
	}
	return cat.probability(t, q);
}

//' Likelihood of the Specified Response Set
//'
//' Calculates the likelihood of a respondent, with ability parameter \eqn{\theta}, having offered the specific set of responses stored in the \code{Cat} objects \code{answers} slot. All calculations are conditional on the item-level parameters stored in the \code{Cat} object.
//'
//' @param catObj An object of class \code{Cat}
//' @param theta A numeric or an integer indicating the value for \eqn{\theta} 
//' 
//' @return The function \code{likelihood} returns a numeric value of the likelihood of the respondent having offered the provided response profile.
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' @references 
//' Baker, Frank B. and Seock-Ho Kim. 2004. Item Response Theory: Parameter Estimation Techniques. New York: Marcel Dekker.
//' 
//' Choi, Seung W. and Richard J. Swartz. 2009. ``Comparison of CAT Item Selection Criteria for Polytomous Items." Applied Psychological Measurement 33(6):419-440.
//' 
//' Muraki, Eiji. 1992. ``A generalized partial credit model: Application of an EM algorithm." ETS Research Report Series 1992(1):1-30.
//' 
//' van der Linden, Wim J. 1998. ``Bayesian Item Selection Criteria for Adaptive Testing." Psychometrika 63(2):201-216.
//' 
//' 
//'@examples
//'## Loading ltm Cat object
//'## Likelihood for Cat object of the ltm model
//'data(ltm_cat)
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'likelihood(ltm_cat, theta = 1)
//'
//'## Loading grm Cat object
//'## Likelihood for Cat object of the grm model
//'data(grm_cat)
//'setAnswers(grm_cat) <- c(1,1,5,2,5, rep(NA, 13))
//'likelihood(grm_cat, theta = 1)
//'
//'
//'
//' @seealso \code{\link{Cat-class}}, \code{\link{probability}}
//'  
//' @export
// [[Rcpp::export]]
double likelihood(S4 catObj, double theta) {
	return Cat(catObj).likelihood(theta);
}



//' Evaluate the Prior Density Distribution at Position \code{x}
//'
//' Calculates the density at \code{x} of either the normal, Student's t, or uniform distribution.
//'
//' @param x A numeric value at which to evaluate the prior
//' @param dist A string indicating the distribution (slot \code{priorName} of \code{Cat} object)
//' @param params A length two numeric vector indicating the parameters of the distribution (slot \code{priorParams} of \code{Cat} object)
//' 
//' @return The function \code{prior} returns a numeric consisting of prior value, \eqn{\pi(x)}, given the value \code{x}.
//'
//' @details The \code{dist} argument needs to be either \code{"UNIFORM"}, \code{"NORMAL"}, or \code{"STUDENT_T"}.
//' 
//' When \code{dist} is \code{"NORMAL"}, the first element of \code{params} is the mean, 
//' the second element is the standard deviation.
//' 
//' When \code{dist} is \code{"STUDENT_T"}, the first 
//' element of \code{params} is the non-centrality parameters and the second is degrees of freedom.  
//' 
//' When \code{dist} is \code{"UNIFORM"}, the elements of \code{params} are the lower and upper bounds,
//' of the interval, respectively.  Note that the \code{"UNIFORM"} is only applicable for the expected a posteriori (EAP) estimation method.   
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Prior calculation for different distributions
//'ltm_cat@priorName <- "NORMAL"
//'ltm_cat@priorParams <- c(0, 1) ## Parameters are mean and standard deviation
//'prior(x = 1, ltm_cat@priorName, ltm_cat@priorParams)
//'
//'ltm_cat@priorName <- "STUDENT_T"
//'ltm_cat@priorParams <- c(1, 3) ## Parameters are non-centrality param and degrees of freedom
//'prior(x = 1, ltm_cat@priorName, ltm_cat@priorParams)
//'
//'ltm_cat@priorName <- "UNIFORM"
//'ltm_cat@priorParams <- c(-1, 1) ## Parameters are lower bound and upper bound of interval
//'prior(x = 1, ltm_cat@priorName, ltm_cat@priorParams)
//'
//'
//' @seealso
//' 
//' \code{\link{Cat-class}}
//'  
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' This function uses Boost \code{C++} source libraries for the uniform and Student's t
//' distributions and calls \code{dnorm4} written in \code{C} which is identical to that 
//' of \code{dnorm} in \code{R}.
//' 
//'  
//'  
//' @export
// [[Rcpp::export]]
double prior(NumericVector x, CharacterVector dist, NumericVector params) {
  std::string name = Rcpp::as<std::string>(dist);
  std::vector<double> args = Rcpp::as<std::vector<double> >(params);
  return Prior(name, args).prior(x[0]);
}

//' The First Derivative of the Log-Likelihood
//' 
//' Calculates either the first derivative of the log-likelihood or the first derivative
//' of the log-posterior evaluated at point \eqn{\theta}.
//' 
//' @param catObj An object of class \code{Cat}
//' @param theta A numeric or an integer indicating the value for \eqn{\theta_j}
//' @param use_prior A logical indicating whether to calculate based on the log-likelihood or log-posterior
//' 
//' @return The function \code{d1LL} returns a numeric of the derivative of the log-likelihood (or log-posterior) given a respondent's answer profile.
//' 
//' @details
//' When the \code{usePrior} argument is \code{FALSE}, the function \code{d1LL} evaluates the first derivative of the log-likelihood at point \eqn{\theta}.  
//' 
//' When the \code{usePrior} argument is \code{TRUE}, the function \code{d1LL} evaluates the first derivative of the log-posterior at point \eqn{\theta}. 
//' 
//' If the argument \code{use_prior} is \code{TRUE}, the function \code{d1LL} must use the the normal prior distribution.
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## d1LL for Cat object of the ltm model
//'d1LL(ltm_cat, theta = 1, use_prior = FALSE)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' @seealso \code{\link{Cat-class}}, \code{\link{prior}}
//' 
//' @references 
//' Baker, Frank B. and Seock-Ho Kim. 2004. Item Response Theory: Parameter Estimation Techniques. New York: Marcel Dekker.
//' 
//' Choi, Seung W. and Richard J. Swartz. 2009. ``Comparison of CAT Item Selection Criteria for Polytomous Items." Applied Psychological Measurement 33(6):419-440.
//' 
//' Muraki, Eiji. 1992. ``A generalized partial credit model: Application of an EM algorithm." ETS Research Report Series 1992(1):1-30.
//' 
//' van der Linden, Wim J. 1998. ``Bayesian Item Selection Criteria for Adaptive Testing." Psychometrika 63(2):201-216.
//' 
//'  
//' @export
// [[Rcpp::export]]
double d1LL(S4 &catObj, double theta, bool use_prior){
  return Cat(catObj).d1LL(theta, use_prior);
}

//' The Second Derivative of the Log-Likelihood
//' 
//' Calculates either the second derivative of the log-likelihood or the second derivative
//' of the log-posterior evaluated at point \eqn{\theta}.
//' 
//' @param catObj An object of class \code{Cat}
//' @param theta A numeric or an integer indicating the value for \eqn{\theta}
//' @param use_prior A logical indicating whether to calculate based on the log-likelihood or log-posterior
//' 
//' @return The function \code{d2LL} returns a numeric of the second derivative of the log-likelihood (or log-posterior) given a respondent's answer profile.
//' 
//' @details
//' When the \code{usePrior} argument is \code{FALSE}, the function \code{d2LL} evaluates the second derivative of the log-likelihood at point \eqn{\theta}.  
//' 
//' When the \code{usePrior} argument is \code{TRUE}, the function \code{d2LL} evaluates the second derivative of the log-posterior at point \eqn{\theta}. 
//' 
//' If the argument \code{use_prior} is \code{TRUE}, the function \code{d2LL} must use the the normal prior distribution.
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## d2LL for Cat object of the ltm model
//'d2LL(ltm_cat, theta = 1, use_prior = FALSE)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' @seealso
//' \code{\link{Cat-class}}, \code{\link{d1LL}}, \code{\link{prior}}
//' 
//' @references 
//' Baker, Frank B. and Seock-Ho Kim. 2004. Item Response Theory: Parameter Estimation Techniques. New York: Marcel Dekker.
//' 
//' Choi, Seung W. and Richard J. Swartz. 2009. ``Comparison of CAT Item Selection Criteria for Polytomous Items." Applied Psychological Measurement 33(6):419-440.
//' 
//' Muraki, Eiji. 1992. ``A generalized partial credit model: Application of an EM algorithm." ETS Research Report Series 1992(1):1-30.
//' 
//' van der Linden, Wim J. 1998. ``Bayesian Item Selection Criteria for Adaptive Testing." Psychometrika 63(2):201-216.
//' 
//' 
//' @export
// [[Rcpp::export]]
double d2LL(S4 &catObj, double theta, bool use_prior){
  return Cat(catObj).d2LL(theta, use_prior);
}

//' Estimate of the Respondent's Ability Parameter
//'
//' Estimates the expected value of the ability parameter \eqn{\theta}, conditioned on the observed answers, prior, and the item parameters.
//'
//' @param catObj An object of class \code{Cat}
//'
//' @return The function \code{estimateTheta} returns a numeric consisting of the expected value of the respondent's ability parameter.
//'
//' @details
//' 
//' Estimation approach is specified in \code{estimation} slot of \code{Cat} object.
//' 
//' The expected a posteriori approach is used when \code{estimation} slot is \code{"EAP"}.  This method involves integration.  See \strong{Note} for more information.
//' 
//' The modal a posteriori approach is used when \code{estimation} slot is \code{"MAP"}.  This method is only available using the normal prior distribution.
//' 
//' The maximum likelihood approach is used when \code{estimation} slot is \code{"MLE"}.  When the likelihood is undefined,
//' the MAP or EAP method will be used, determined by what is specified in the \code{estimationDefault} slot in \code{Cat} object.
//' 
//' The weighted maximum likelihood approach is used when \code{estimation} slot is \code{"WLE"}.
//' Estimating \eqn{\theta} requires root finding with the ``Brent'' method in the GNU Scientific
//'  Library (GSL) with initial search interval of \code{[-5,5]}.
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Set different estimation procedures and estimate ability parameter
//'setEstimation(ltm_cat) <- "EAP"
//'estimateTheta(ltm_cat)
//'
//'setEstimation(ltm_cat) <- "MAP"
//'estimateTheta(ltm_cat)
//'
//'setEstimation(ltm_cat) <- "MLE"
//'estimateTheta(ltm_cat)
//'
//'setEstimation(ltm_cat) <- "WLE"
//'estimateTheta(ltm_cat)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' This function uses adaptive quadrature methods from the GNU Scientific
//'  Library (GSL) to approximate single-dimensional
//'  integrals with high accuracy.  The bounds of integration are determined by the
//'  \code{lowerBound} and \code{upperBound} slots of the \code{Cat} object.
//' 
//' @seealso \code{\link{Cat-class}}, \code{\link{estimateSE}}
//' 
//' @references
//' 
//' van der Linden, Wim J. 1998. "Bayesian Item Selection Criteria for Adaptive Testing." Psychometrika
//' 63(2):201-216.
//' 
//' Van der Linden, Wim J., and Peter J. Pashley. 2009. "Item Selection and Ability
//'  Estimation in Adaptive Testing." Elements of Adaptive Testing. 
//'  Springer New York, 3-30.
//'  
//' @export
// [[Rcpp::export]]
double estimateTheta(S4 catObj) {
	return Cat(catObj).estimateTheta();
}





//' Estimates of Ability Parameters for a Dataset of Response Profiles
//'
//' Estimates the expected value of the ability parameter \eqn{\theta}, conditioned on the observed answers, prior, and the item parameters
//' for complete response profiles for a dataset of respondents.
//'
//' @param catObj An object of class \code{Cat}
//' @param responses A dataframe of complete response profiles
//'
//' @return The function \code{estimateThetas} returns a vector of the expected values of the respondents' ability parameters.
//'
//' @details
//' 
//' Estimation approach is specified in \code{estimation} slot of \code{Cat} object.
//' 
//' The expected a posteriori approach is used when \code{estimation} slot is \code{"EAP"}.  This method involves integration.  See \strong{Note} for more information.
//' 
//' The modal a posteriori approach is used when \code{estimation} slot is \code{"MAP"}.  This method is only available using the normal prior distribution.
//' 
//' The maximum likelihood approach is used when \code{estimation} slot is \code{"MLE"}.  When the likelihood is undefined,
//' the MAP or EAP method will be used, determined by what is specified in the \code{estimationDefault} slot in \code{Cat} object.
//' 
//' The weighted maximum likelihood approach is used when \code{estimation} slot is \code{"WLE"}.
//' Estimating \eqn{\theta} requires root finding with the ``Brent'' method in the GNU Scientific
//'  Library (GSL) with initial search interval of \code{[-5,5]}.
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Set different estimation procedures and estimate ability parameter
//'data(npi)
//'setEstimation(ltm_cat) <- "EAP"
//'estimateThetas(ltm_cat, responses = npi[1:25, ])
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' This function uses adaptive quadrature methods from the GNU Scientific
//'  Library (GSL) to approximate single-dimensional
//'  integrals with high accuracy.  The bounds of integration are determined by the
//'  \code{lowerBound} and \code{upperBound} slots of the \code{Cat} object.
//' 
//' @seealso \code{\link{Cat-class}}, \code{\link{estimateTheta}}
//' @export
// [[Rcpp::export]]
NumericVector estimateThetas(S4 catObj, DataFrame responses){
	return Cat(catObj).estimateThetas(responses);
}





//' Simulates Estimates of Ability Parameters for a Dataset of Response Profiles
//'
//' Given a set of stopping rules and complete response profiles for a dataset of respondents,
//' simulates the expected value of the ability parameter \eqn{\theta} as though an adaptive 
//' battery were provided
//'
//' @param catObj An object of class \code{Cat} with stopping rule(s) specified
//' @param responses A dataframe of complete response profiles
//'
//' @return The function \code{simulateThetas} returns a vector of the expected values of the respondents' ability parameters
//' as though the respondents were given an adaptive battery.  Given the item selection criterion specified in the \code{Cat} object,
//'  this function selects an item, "administers" the item to the respondent, and records their answer from the dataframe provided in
//'  the \code{response} parameter of the function.  This process continues until stopping rule(s) specified in the \code{Cat} object are met for each respondent.  The function returns a final estimate of the ability parameter \eqn{\theta}
//'  for each respondent.
//'  
//'
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Set estimation, selection, and stopping rule
//'data(npi)
//'setEstimation(ltm_cat) <- "EAP"
//'setSelection(ltm_cat) <- "EPV"
//'setLengthThreshold(ltm_cat) <- 3
//'
//'## Simulate theta by asking 3 questions adaptively for the first 25 respondents
//'simulateThetas(ltm_cat, responses = npi[1:25, ])
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' @seealso \code{\link{Cat-class}}, \code{\link{estimateThetas}}, \code{\link{checkStopRules}}
//' @export
// [[Rcpp::export]]
NumericVector simulateThetas(S4 catObj, DataFrame responses){
	return Cat(catObj).simulateThetas(responses);
}


//' Observed Information
//'
//' Calculates the observed information of the likelihood of a respondent's ability \eqn{\theta} for a given \code{item}.
//'
//' @param catObj An object of class \code{Cat}
//' @param theta A numeric or an integer indicating the value for \eqn{\theta}
//' @param item An integer indicating the index of the question item
//'
//' @return The function \code{obsInf} returns a numeric value of the observed information of the likelihood, given \eqn{\theta}, for a given question item.
//' 
//' @details The observed information is equivalent to the negative second derivative of the log-likelihood evaluated at \eqn{\theta}.
//' This function should never be called when the respondent has answered no questions as the likelihood is not defined.
//'   
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Calculate observed information for different ability parameters and items
//'obsInf(ltm_cat, theta = 1, item = 10)
//'obsInf(ltm_cat, theta = 1, item = 11)
//'obsInf(ltm_cat, theta = 0, item = 10)
//'obsInf(ltm_cat, theta = 0, item = 11)
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//'
//' @seealso \code{\link{estimateTheta}}, \code{\link{expectedObsInf}}
//' 
//' @export
// [[Rcpp::export]]
double obsInf(S4 catObj, double theta, int item) {
  item = item - 1;
  return Cat(catObj).obsInf(theta, item);
}

//' Expected Observed Information
//'
//' Calculates the expected information, which is the observed information attained from a specific response set times the probability of that profile occurring.
//'
//' @param catObj An object of class \code{Cat}
//' @param item An integer indicating the index of the question item
//' 
//' @return The function \code{expectedObsInf} returns a numeric value of the expected information. 
//' 
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Expected observed information for different items
//'expectedObsInf(ltm_cat, item = 10)
//'expectedObsInf(ltm_cat, item = 20)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//'
//' @seealso \code{\link{estimateSE}},\code{\link{obsInf}}, \code{\link{probability}}, \code{\link{selectItem}}
//' 
//' @export
// [[Rcpp::export]]
double expectedObsInf(S4 catObj, int item) {
  item = item - 1;
  return Cat(catObj).expectedObsInf(item);
}

//' Fisher's Information
//'
//' Calculates the expected value of the observed information of the likelihood evaluated at the input value \eqn{\theta}.
//'
//' @param catObj An object of class \code{Cat}
//' @param theta A numeric or an integer indicating the potential value for \eqn{\theta}
//' @param item An integer indicating the index of the question item
//'
//' @return The function \code{fisherInf} returns a numeric of the expected value of the observed information of the likelihood evaluated at the input value \eqn{\theta}.
//' 
//' @details For the dichotomous case, this Fisher's information is equivalent to the observed information.  
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Fisher's information for different items, at ability parameter of 1
//'fisherInf(ltm_cat, theta = 1, item = 10)
//'fisherInf(ltm_cat, theta = 1, item = 20)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//'
//' @seealso \code{\link{fisherTestInfo}}, \code{\link{obsInf}}, \code{\link{selectItem}}
//' 
//' @export
// [[Rcpp::export]]
double fisherInf(S4 catObj, double theta, int item) {
  item = item - 1;
  return Cat(catObj).fisherInf(theta, item);
}

//' Fisher's Test Information
//'
//' Calculates the total information gained for a respondent for all answered items, conditioned on \eqn{\theta}.
//'
//' @param catObj An object of class \code{Cat}
//' 
//' @return The function \code{fisherTestInfo} returns a numeric indicating the total information gained for a respondent,
//'  given a specific answer set and the current estimate of \eqn{\theta}.
//' 
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Fisher's test information for answer profile
//'fisherTestInfo(ltm_cat)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//'
//' @seealso \code{\link{fisherInf}}
//' 
//' @export
// [[Rcpp::export]]
double fisherTestInfo(S4 catObj) {
  return Cat(catObj).fisherTestInfo();
}

//' Standard Error of Ability Parameter Estimate
//'
//' Estimates the standard error for a respondent's ability parameter estimate, \eqn{\theta}.
//'
//' @param catObj An object of class \code{Cat}
//'
//' @return The function \code{estimateSE} returns a numeric for the standard error for \eqn{\theta}.
//'
//' @details 
//' 
//' The function \code{estimateSE} estimates the standard error of the ability estimate
//' given the estimation approach of the \code{Cat} object, specified in \code{estimation} slot of \code{Cat} object.
//' 
//' The expected a posteriori approach is used when \code{estimation} slot is \code{"EAP"}.  This method involves integration. See \strong{Note} for more information.
//' 
//' The modal a posteriori approach is used when \code{estimation} slot is \code{"MAP"}.  This method is only available using the normal prior distribution.
//' 
//' The maximum likelihood approach is used when \code{estimation} slot is \code{"MLE"}.  When the likelihood
//' of the ability estimate is undefined,
//' the MAP or EAP method will be used, determined by what is specified in the \code{estimationDefault} slot in \code{Cat} object.
//' 
//' The weighted maximum likelihood approach is used when \code{estimation} slot is \code{"WLE"}.
//' Estimating \eqn{\theta} requires root finding with the ``Brent'' method in the GNU Scientific
//'  Library (GSL) with initial search interval of \code{[-5,5]}.
//'   
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Set different estimation procedures and calculate ability estimate and its standard error
//'setEstimation(ltm_cat) <- "EAP"
//'estimateTheta(ltm_cat)
//'estimateSE(ltm_cat)
//'
//'setEstimation(ltm_cat) <- "MAP"
//'estimateTheta(ltm_cat)
//'estimateSE(ltm_cat)
//'
//'setEstimation(ltm_cat) <- "MLE"
//'estimateTheta(ltm_cat)
//'estimateSE(ltm_cat)
//'
//'setEstimation(ltm_cat) <- "WLE"
//'estimateTheta(ltm_cat)
//'estimateSE(ltm_cat)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' This function uses adaptive quadrature methods from the GNU Scientific
//'  Library (GSL) to approximate single-dimensional
//'  integrals with high accuracy.  The bounds of integration are determined by the
//'  \code{lowerBound} and \code{upperBound} slots of the \code{Cat} object.
//'
//' @seealso \code{\link{estimateTheta}}
//'  
//' @export
// [[Rcpp::export]]
double estimateSE(S4 catObj) {
	return Cat(catObj).estimateSE();
}

//' Expected Posterior Variance
//'
//' Estimates the expected posterior variance for a respondent's estimated ability parameter for an item yet to be answered based on a respondent's ability parameter estimate from the already-answered items.
//'
//' @param catObj An object of class \code{Cat}
//' @param item An integer indicating the index of the question item
//'
//' @return The function \code{expectedPV} returns a numeric value indicating a respondent's expected posterior variance for an unasked item.
//'
//' 
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Estimate EPV for different unasked items
//'expectedPV(ltm_cat, item = 10)
//'expectedPV(ltm_cat, item = 20)
//'expectedPV(ltm_cat, item = 30)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//'
//' 
//' @seealso \code{\link{estimateSE}}, \code{\link{probability}}, \code{\link{selectItem}}
//'  
//' 
//' @export
// [[Rcpp::export]]
double expectedPV(S4 catObj, int item) {
  item = item - 1.0;
  return Cat(catObj).expectedPV(item);
}

//' Select Next Item
//'
//' Selects the next item in the question set to be administered to respondent based on the specified selection method.
//' 
//' @param catObj An object of class \code{Cat}
//'
//' @return The function \code{selectItem} returns a list with two elements:
//'  
//' \code{estimates}: a data frame with a row for each unasked question and three columns representing 
//' the item index number, the item name, and the item value (calculated by the specified selection method), 
//' and
//' 
//' \code{next_item}: a numeric representing the index of the item that should be asked next.
//'
//' @details Selection approach is specified in the \code{selection} slot of the \code{Cat} object.
//' 
//' The minimum expected posterior variance criterion is used when the \code{selection}
//'  slot is \code{"EPV"}.  This method calls \code{expectedPV} for each unasked item.
//' 
//' The maximum Fisher's information criterion is used when the \code{selection}
//'   slot is \code{"MFI"}.  This method calls \code{fisherInf} for each unasked item.
//'   
//' The maximum likelihood weighted information criterion is used when the \code{selection}
//' slot is \code{"MLWI"}. This method involves integration. See \strong{Note} for more information.
//' 
//' The maximum posterior weighted information criterion is used when the \code{selection}
//' slot is \code{"MPWI"}.  This method involves integration. See \strong{Note} for more information.
//'  
//' The maximum expected information criterion is used when the \code{selection}
//' slot is \code{"MEI"}.  This method calls \code{expectedObsInf} for each unasked item.
//' 
//' The maximum Kullback-Leibler information criterion is used when the \code{selection}
//' slot is \code{"KL"}.  This method calls \code{expectedKL} for each unasked item.
//' 
//' The maximum likelihood weighted Kullback-Leibler information criterion is used when the \code{selection}
//' slot is \code{"LKL"}.  This method calls \code{likelihoodKL} for each unasked item.
//' 
//' The maximum posterior weighted Kullback-Leibler information criterion is used when the \code{selection}
//' slot is \code{"PKL"}.  This method calls \code{posteriorKL} for each unasked item.
//' 
//' The maximum Fisher interval information criterion is used when the \code{selection}
//' slot is \code{"MFII"}. This method involves integration. See \strong{Note} for more information.
//' The bounds of integration are \eqn{\hat{\theta} \pm \delta},
//'  where \eqn{\delta} is \eqn{z} times the square root of the Fisher test information and
//'  \eqn{z} is specified in the \code{z} slot of the \code{Cat} object.
//' 
//' A random number generator is used when the \code{selection}
//' slot is \code{"RANDOM"}.
//' 
//' @references
//' 
//' van der Linden, Wim J. 1998. "Bayesian Item Selection Criteria for Adaptive Testing." Psychometrika
//' 63(2):201-216.
//' 
//' Van der Linden, Wim J., and Peter J. Pashley. 2009. "Item Selection and Ability
//'  Estimation in Adaptive Testing." Elements of Adaptive Testing. 
//'  Springer New York, 3-30.
//'  
//'  Veldkamp, B.P., 2003. Item Selection in Polytomous CAT.
//'   In New Developments in Psychometrics (pp. 207-214). Springer Japan.
//' 
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Set different selection criterion and choose next item
//'setSelection(ltm_cat) <- "EPV"
//'selectItem(ltm_cat)
//'
//'setSelection(ltm_cat) <- "MFI"
//'selectItem(ltm_cat)
//'
//'setSelection(ltm_cat) <- "MLWI"
//'selectItem(ltm_cat)
//'
//'setSelection(ltm_cat) <- "MPWI"
//'selectItem(ltm_cat)
//'
//'setSelection(ltm_cat) <- "MEI"
//'selectItem(ltm_cat)
//'
//'setSelection(ltm_cat) <- "KL"
//'selectItem(ltm_cat)
//'
//'setSelection(ltm_cat) <- "LKL"
//'selectItem(ltm_cat)
//'
//'setSelection(ltm_cat) <- "PKL"
//'selectItem(ltm_cat)
//'
//'setSelection(ltm_cat) <- "MFII"
//'selectItem(ltm_cat)
//'
//'setSelection(ltm_cat) <- "RANDOM"
//'selectItem(ltm_cat)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' This function uses adaptive quadrature methods from the GNU Scientific
//'  Library (GSL) to approximate single-dimensional
//'  integrals with high accuracy.  The bounds of integration are determined by the
//'  \code{lowerBound} and \code{upperBound} slots of the \code{Cat} object unless otherwise noted.
//' 
//' The \code{"RANDOM"} item selection criterion uses the package \code{RcppArmadillo} to randomly
//' choose the next item among unasked questions.  \code{RcppArmadillo} provides an exact reproduction
//' of R's \code{sample} function that can be called from C++.
//' 
//' In the rare instance that item parameters are identical, it may be that that \code{selectItem} must choose
//' between two items with the same value calculated by the selection criterion.  In such an instance, \code{selectItem}
//' will choose the item with the lower question index.
//' 
//' 
//' 
//' @seealso \code{\link{estimateTheta}}, \code{\link{expectedPV}}, \code{\link{fisherInf}}
//'  
//' @export
// [[Rcpp::export]]
List selectItem(S4 catObj) {
  return Cat(catObj).selectItem();
}

//' Expected Kullback-Leibler Information
//'
//' Calculates the expected Kullback-Leibler information for an individual question item.
//' 
//' 
//' @param catObj An object of class \code{Cat}
//' @param item An integer indicating the index of the question item
//'
//' @details The function \code{expectedKL} calculates the expected value of the Kullback-Leibler information
//' for a specified item where the bounds of integration are \eqn{\hat{\theta} \pm \delta},
//'  where \eqn{\delta} is \eqn{z} times the square root of the Fisher test information and
//'  \eqn{z} is specified in the \code{z} slot of the \code{Cat} object.  See \strong{Note} for more information on integration.
//'  
//' @return The function \code{expectedKL} returns a numeric indicating the
//' expected Kullback-Leibler information
//'  for the specified item, given the current answer profile and ability parameter estimate.
//' 
//' 
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Estimate KL for different unasked items
//'expectedKL(ltm_cat, item = 10)
//'expectedKL(ltm_cat, item = 20)
//'expectedKL(ltm_cat, item = 30)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' This function uses adaptive quadrature methods from the GNU Scientific
//'  Library (GSL) to approximate single-dimensional
//'  integrals with high accuracy.
//' 
//' @seealso \code{\link{likelihoodKL}}, \code{\link{posteriorKL}}, \code{\link{selectItem}}
//'
//' @export
// [[Rcpp::export]]
double expectedKL(S4 catObj, int item) {
  item = item - 1;
  return Cat(catObj).expectedKL(item);
}

//' Expected Kullback-Leibler Information, Weighted by Likelihood
//'
//' Calculates the expected Kullback-Leibler information, weighted by likelihood, for a specified item.
//' 
//' 
//' @param catObj An object of class \code{Cat}
//' @param item An integer indicating the index of the question item
//'
//' @details The function \code{likelihoodKL} calculates the expected Kullback-Leibler information 
//' for question \code{item}, where the proposed values of the true ability parameter are weighted by
//' the current likelihood.
//' 
//' 
//' This function involves integration.  See \strong{Note} for more information.
//' 
//' 
//' @return The function \code{likelihoodKL} returns a numeric indicating the
//' expected Kullback-Leibler information weighted by the likelihood
//'  for the specified item, given the current answer profile and ability parameter estimate.
//'  
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Estimate EPV for different unasked items
//'likelihoodKL(ltm_cat, item = 10)
//'likelihoodKL(ltm_cat, item = 20)
//'likelihoodKL(ltm_cat, item = 30)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' This function uses adaptive quadrature methods from the GNU Scientific
//'  Library (GSL) to approximate single-dimensional
//'  integrals with high accuracy.  The bounds of integration are determined by the
//'  \code{lowerBound} and \code{upperBound} slots of the \code{Cat} object.
//' 
//' @seealso \code{\link{expectedKL}}, \code{\link{posteriorKL}}, \code{\link{selectItem}}
//' 
//' @export
// [[Rcpp::export]]
double likelihoodKL(S4 catObj, int item) {
  item = item - 1;
  return Cat(catObj).likelihoodKL(item);
}

//' Expected Kullback-Leibler Information, Weighted by the Prior
//' 
//' Calculates the expected Kullback-Leibler information, weighted by likelihood and prior beliefs, for a specified item.
//'
//' @param catObj An object of class \code{Cat}
//' @param item An integer indicating the index of the question item
//'
//' @details The function \code{posteriorKL} calculates the expected Kullback-Leibler information 
//' for question \code{item}, where the proposed values of the true ability parameter are weighted by
//' the prior.
//' 
//' This function involves integration.  See \strong{Note} for more information.
//' 
//' 
//' @return The function \code{posteriorKL} returns a numeric indicating the
//' expected Kullback-Leibler information weighted by the likelihood
//'  for the specified item, given the current answer profile and ability parameter estimate.
//'  
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Estimate EPV for different unasked items
//'posteriorKL(ltm_cat, item = 10)
//'posteriorKL(ltm_cat, item = 20)
//'posteriorKL(ltm_cat, item = 30)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' This function uses adaptive quadrature methods from the GNU Scientific
//'  Library (GSL) to approximate single-dimensional
//'  integrals with high accuracy.  The bounds of integration are determined by the
//'  \code{lowerBound} and \code{upperBound} slots of the \code{Cat} object.
//' 
//' @seealso \code{\link{expectedKL}}, \code{\link{likelihoodKL}}, \code{\link{selectItem}}
//' @export
// [[Rcpp::export]]
double posteriorKL(S4 catObj, int item) {
  item = item - 1;
  return Cat(catObj).posteriorKL(item);
}

//' Look Ahead to Select Next Item
//'
//' Selects the next item that would be asked for all possible response options to the question the respondent is currently answering.
//'
//' @param catObj  An object of class \code{Cat}
//' @param item A numeric indicating the question item the respondent is currently answering.
//'
//' @return A function \code{lookAhead} returns a list of one element named \code{estimates}, which is itself a data frame.
//' The the first column of the data frame is the possible response option to the question the respondent
//' is currently answering and the second column is the next item that should be asked given each response.
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//' 
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## What should be asked next if respondent is currently answering item 6
//'lookAhead(ltm_cat, 6)
//'
//'## Loading grm Cat object
//'data(grm_cat)
//' 
//'## Store example answers
//'setAnswers(grm_cat) <- c(4,3,5,1,1, rep(NA, 13))
//'
//'## What should be asked next if respondent is currently answering item 6
//'lookAhead(grm_cat, 6)
//'
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' @seealso \code{\link{selectItem}}
//'
//' @export
// [[Rcpp::export]]
List lookAhead(S4 catObj, int item) {
  item = item - 1.0;
  return Cat(catObj).lookAhead(item);
}

//' Check if Stop and/or Override Rules are Met
//'
//' Evaluates the specified stopping and/or override rules to check if respondent should be asked further questions.
//'
//' @param catObj  An object of class \code{Cat}
//'
//'
//' @details The stopping rule thresholds are stored in the following \code{Cat} object slots:
//' \code{lengthThreshold}, \code{seThreshold}, \code{infoThreshold}, and \code{gainThreshold}. The override
//'  thresholds are stored in the following \code{Cat} object slots: \code{lengthOverride}, \code{gainOverride}.  
//'  A value of \code{NA} indicates the rule will not be used in evaluating if further questions should be administered.
//'  A user can specify any combination of stopping rules and/or overrides.  
//'  
//'   \strong{Stopping Rules:}
//'
//'   \code{lengthThreshold}: Number of question's answered \eqn{\geq a}
//'  
//'   \code{seThreshold}: \eqn{SE(\hat{\theta}) < a}
//'  
//'   \code{infoThreshold}: \eqn{FI < a} \eqn{\forall} remaining items
//'
//'   \code{gainThreshold}: \eqn{SE(\hat{\theta}) - \sqrt{EPV} | < a} \eqn{\forall} remaining items
//'
//'   \strong{Overrides:}
//'
//'   \code{lengthOverride}: Number of question's answered \eqn{< a}
//'
//'   \code{gainOverride}: \eqn{|SE(\hat{\theta}) - \sqrt{EPV} | \geq a} \eqn{\forall} remaining items
//' 
//' 
//' 
//' @return The function \code{checkStopRules} returns a boolean.  \code{TRUE} indicates all specified stopping rules are met
//'   and no specified overrides are met.  No further items should be administered.  \code{FALSE} indicates at least one specified
//'    stopping rule is not met, or if any specified override threshold is met.  Additional items should be administered.
//' 
//' 
//' @examples
//'## Loading ltm Cat object
//'data(ltm_cat)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1,0,0,0,1,1, rep(NA, 30))
//'
//'## Stop administering items if standard error of ability
//'## estimate is low enough
//'setSeThreshold(ltm_cat) <- .5
//'checkStopRules(ltm_cat)
//'
//'## Now stop if standard error is low enough, but only if respondent has
//'## answered 11 questions 
//'setLengthOverride(ltm_cat) <- 11
//'checkStopRules(ltm_cat)
//'
//'## When respondent has answered 11 questions and standard error
//'## of ability estimate is below .5, stop administering items
//'setAnswers(ltm_cat) <- c(1,0,1,0,1,0,0,0,1,1,0, rep(NA, 29))
//'checkStopRules(ltm_cat)
//'
//'
//'@references
//'
//'Babcock, Ben, and David J. Weiss. 2009. ``Termination Criteria in Computerized Adaptive Tests: Variable-Length CATs are not Biased." Proceedings of the 2009 GMAC Conference on Computerized Adaptive Testing. Vol. 14.
//' 
//' 
//' @seealso \code{\link{Cat-class}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{fisherInf}}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled \code{C++} code.
//' 
//' @export
// [[Rcpp::export]]
bool checkStopRules(S4 catObj) {
  return Cat(catObj).checkStopRules();
}




