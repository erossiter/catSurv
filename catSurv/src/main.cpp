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
//' @return When the \code{model} slot of the \code{catObj} is \code{"ltm"}, the function \code{probabilty} returns a numeric vector of length one representing the probabilty of observing a non-zero response.
//'
//'When the \code{model} slot of the \code{catObj} is \code{"tpm"}, the function \code{probabilty} returns a numeric vector of length one representing the probabilty of observing a non-zero response.
//'
//' When the \code{model} slot of the \code{catObj} is \code{"grm"}, the function \code{probabilty} returns a numeric vector of length k+1, where k is the number of possible responses. The first element will always be zero and the (k+1)th element will always be one. The middle elements are the cumulative probability of observing response k or lower.
//'
//'  When the \code{model} slot of the \code{catObj} is \code{"gpcm"}, the function \code{probabilty} returns a numeric vector of length k, where k is the number of possible responses. Each number represents the probability of observing response k.
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
//'\dontrun{
//'## Probability for Cat object of the ltm model
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'probability(ltm_cat, theta = 1, item = 1)
//'
//'## Probability for Cat object of the tpm model
//'data(polknow)
//'tpm_cat <- tpmCat(polknow)
//'probability(tpm_cat, theta = 1, item = 1)
//'
//'## Probability for Cat object of the grm model
//'data(nfc)
//'grm_cat <- grmCat(nfc)
//'probability(grm_cat, theta = 1, item = 1)
//'}
//'  
//' @seealso \code{\link{Cat-class}}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
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
//' @param theta A numeric or an integer indicating the value for \eqn{\theta_j} 
//' 
//' @return The function \code{likelihood} returns a numeric value of the likelihood of the respondent having offered the provided response profile.
//'
//' @details
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//' 
//' @references 
//' 
//'@examples
//'\dontrun{
//'## Create Cat object, store example answers, and calculate
//'## likelihood at theta = 1
//'
//'## Likelihood for Cat object of the ltm model
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'likelihood(ltm_cat, theta = 1)
//'
//'## Likelihood for Cat object of the tpm model
//'data(polknow)
//'tpm_cat <- tpmCat(polknow)
//'setAnswers(tpm_cat) <- c(1,0,1,0, rep(NA, 35))
//'likelihood(tpm_cat, theta = 1)
//'
//'## Likelihood for Cat object of the grm model
//'data(nfc)
//'grm_cat <- grmCat(nfc)
//'setAnswers(grm_cat) <- c(1,3,4,5, rep(NA, 13))
//'likelihood(grm_cat, theta = 1)
//'}
//'
//'
//' @seealso \code{\link{probability}}, \code{\link{Cat-class}}
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
//' \dontrun{
//'## Create Cat object
//'data(npi)
//'cat <- ltmCat(npi)
//'
//'## Prior calculation for different distributions
//'cat@priorName <- "NORMAL"
//'cat@priorParams <- c(0, 1) ## Parameters are mean and standard deviation
//'prior(x = 1, cat@priorName, cat@priorParams)
//'
//'cat@priorName <- "STUDENT_T"
//'cat@priorParams <- c(1, 3) ## Parameters are non-centrality param and degrees of freedom
//'prior(x = 1, cat@priorName, cat@priorParams)
//'
//'cat@priorName <- "UNIFORM"
//'cat@priorParams <- c(-1, 1) ## Parameters are lower bound and upper bound of interval
//'prior(x = 1, cat@priorName, cat@priorParams)
//'}
//'
//' @seealso
//' 
//' \code{\link{Cat-class}}
//'  
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//' 
//' This function uses Boost \code{C++} source libraries for the uniform and Student's t
//' distributions and calls \code{dnorm4} written in C which is identical to that 
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
//' @param use_prior A logical indicating whether to calculate baseded on the log-likelihood or log-posterior
//' 
//' @return The function \code{dLL} returns a numeric of the derivative of the log-likelihood (or log-posterior) given a respondent's answer profile.
//' 
//' @details
//' When the \code{usePrior} argument is \code{FALSE}, the function \code{dLL} evaluates the first derivative of the log-likelihood at point \eqn{\theta}.  
//' 
//' When the \code{usePrior} argument is \code{TRUE}, the function \code{dLL} evaluates the first derivative of the log-posterior at point \eqn{\theta}. 
//' 
//' The function \code{dLL} is only available when using the normal prior distribution when the \code{use_prior} argument is \code{TRUE}.
//' 
//' @examples
//' \dontrun{
//'## Create Cat object, store example answers, and calculate
//'## first derivative of log-likelihood at theta = 1
//'
//'## dLL for Cat object of the ltm model
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'dLL(ltm_cat, theta = 1)
//'
//'## dLL for Cat object of the tpm model
//'data(polknow)
//'tpm_cat <- tpmCat(polknow)
//'setAnswers(tpm_cat) <- c(1,0,1,0, rep(NA, 35))
//'dLL(tpm_cat, theta = 1)
//'
//'## dLL for Cat object of the grm model
//'data(nfc)
//'grm_cat <- grmCat(nfc)
//'setAnswers(grm_cat) <- c(1,3,4,5, rep(NA, 13))
//'dLL(grm_cat, theta = 1)
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//' 
//' @seealso
//' 
//' \code{\link{Cat-class}}, \code{\link{prior}}
//'  
//' @export
// [[Rcpp::export]]
double dLL(S4 &catObj, double theta, bool use_prior){
  return Cat(catObj).dLL(theta, use_prior);
}

//' The Second Derivative of the Log-Likelihood
//' 
//' Calculates either the second derivative of the log-likelihood or the second derivative
//' of the log-posterior evaluated at point \eqn{\theta}.
//' 
//' @param catObj An object of class \code{Cat}
//' @param theta A numeric or an integer indicating the value for \eqn{\theta}
//' @param use_prior A logical indicating whether to calculate baseded on the log-likelihood or log-posterior
//' 
//' @return The function \code{d2LL} returns a numeric of the second derivative of the log-likelihood (or log-posterior) given a respondent's answer profile.
//' 
//' @details
//' When the \code{usePrior} argument is \code{FALSE}, the function \code{d2LL} evaluates the second derivative of the log-likelihood at point \eqn{\theta}.  
//' 
//' When the \code{usePrior} argument is \code{TRUE}, the function \code{d2LL} evaluates the second derivative of the log-posterior at point \eqn{\theta}. 
//' 
//' The function \code{dLL2} is only available when using the normal prior distribution when the argument \code{use_prior} is \code{TRUE}.
//' 
//' @examples
//' \dontrun{
//'## Create Cat object, store example answers, and calculate
//'## second derivative of log-likelihood at theta = 1
//'
//'## d2LL for Cat object of the ltm model
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'d2LL(ltm_cat, theta = 1)
//'
//'## d2LL for Cat object of the tpm model
//'data(polknow)
//'tpm_cat <- tpmCat(polknow)
//'setAnswers(tpm_cat) <- c(1,0,1,0, rep(NA, 35))
//'d2LL(tpm_cat, theta = 1)
//'
//'## d2LL for Cat object of the grm model
//'data(nfc)
//'grm_cat <- grmCat(nfc)
//'setAnswers(grm_cat) <- c(1,3,4,5, rep(NA, 13))
//'d2LL(grm_cat, theta = 1)
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//' 
//' @seealso
//' \code{\link{Cat-class}}, \code{\link{prior}}, \code{\link{dLL}} 
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
//' The expected a posteriori approach is used when \code{estimation} slot is \code{"EAP"}.
//' 
//' The modal a posteriori approach is used when \code{estimation} slot is \code{"MAP"}.  This method is only available using the normal prior distribution.
//' 
//' The maximum likelihood approach is used when \code{estimation} slot is \code{"MLE"}.  When the likelihood is undefined,
//' the MAP or EAP method will be used, determined by what is specified in the \code{estimationDefault} slot in \code{Cat} object.
//' 
//' The weighted maximum likelihood approach is used when \code{estimation} slot is \code{"WLE"}. Estimating \eqn{\theta} requires root finding with the ``Brent'' method in the \code{gsl} library.
//' 
//' @examples
//' \dontrun{
//'## Create Cat object
//'data(npi)
//'ltm_cat <- ltmCat(npi)
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
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//' 
//' @seealso
//' 
//' \code{\link{Cat-class}} 
//'  
//' @export
// [[Rcpp::export]]
double estimateTheta(S4 catObj) {
	return Cat(catObj).estimateTheta();
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
//' \dontrun{
//'## Create Cat object, store example answers, and calculate observed information
//'for an ability parameter of 1 for item 10
//'
//'## observed information for Cat object of the ltm model
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'obsInf(ltm_cat, theta = 1, item = 10)
//'
//'## observed information for Cat object of the tpm model
//'data(polknow)
//'tpm_cat <- tpmCat(polknow)
//'setAnswers(tpm_cat) <- c(1,0,1,0, rep(NA, 35))
//'obsInf(tpm_cat, theta = 1, item = 10)
//'
//'## observed information for Cat object of the grm model
//'data(nfc)
//'grm_cat <- grmCat(nfc)
//'setAnswers(grm_cat) <- c(1,3,4,5, rep(NA, 13))
//'obsInf(grm_cat, theta = 1, item = 10)
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//'
//' @seealso
//' 
//' \code{\link{estimateTheta}} for calculation of \eqn{\theta}
//' 
//' \code{\link{expectedObsInf}} for further application of observed information
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
//' @details
//' 
//' @examples
//' \dontrun{
//'## Create Cat object
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Expected observed information for different items
//'expectedObsInf(ltm_cat, item = 10)
//'expectedObsInf(ltm_cat, item = 20)
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//'
//' @seealso \code{\link{estimateTheta}}, \code{\link{obsInf}}
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
//' \dontrun{
//'## Create Cat object
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Fisher's information for different items,
//'## at ability parameter of 1
//'fisherInf(ltm_cat, theta = 1, item = 10)
//'fisherInf(ltm_cat, theta = 1, item = 20)
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//'
//' @seealso \code{\link{fisherTestInfo}}, \code{\link{obsInf}}
//' 
//' @export
// [[Rcpp::export]]
double fisherInf(S4 catObj, double theta, int item) {
  item = item - 1;
  return Cat(catObj).fisherInf(theta, item);
}

//' Fisher's Test Information
//'
//' Calculates the total information gained for a respondent for all answered items, conditioned on \eqn{theta}.
//'
//' @param catObj An object of class \code{Cat}
//' 
//' @return The function \code{fisherTestInfo} returns a numeric indicating the total information gained for a respondent,
//'  given a specific answer set and the current estimate of \eqn{theta}.
//' 
//' @details
//' 
//' @examples
//' \dontrun{
//'## Create Cat object
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Fisher's test information for answer profile
//'fisherTestInfo(ltm_cat)
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
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
//' Estimation approach is specified in \code{estimation} slot of \code{Cat} object.  The
//' options are \code{"EAP"} for the expected a posteriori approach, \code{"MAP"} for the modal a posteriori
//' approach, \code{"MLE"} for the maximum likelihood approach, and \code{"WLE"} for the weighted maximum likelihood
//' approach.  The function \code{estimateSE} will calculate the standard error of the ability estimate
//' given the estimation approach.
//'   
//' @examples
//' \dontrun{
//'## Create Cat object
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## Set different estimation procedures and calculate
//'## ability estimate and its standard error
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
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
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
//' Estimates the expected posterior variance for a respondent's estimated ability parameter for an item yet to be answered based on a respondent's ability parameter estimate from the already-answered items
//'
//' @param catObj An object of class \code{Cat}
//' @param item An integer indicating the index of the question item
//'
//' @return A numeric value indicating a respondent's expected posterior variance for a yet to be asked question item
//'
//' @details 
//' 
//' @examples
//' \dontrun{
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//'
//' 
//' @seealso
//' 
//' \code{\link{estimateTheta}}, \code{\link{probability}}
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
//' Selects the next item in the question set to be adminstered to respondent based on the specified selection method.
//' 
//' @param catObj An object of class \code{Cat}
//'
//' @return The function \code{selectItem} returns a \code{list} with two elements:
//'  
//' \code{estimates}: a \code{data.frame} with a row for each unasked question and three columns representing 
//' the item index number, the item name, and the item value (calculated by the specified selection method), 
//' and
//' 
//' \code{next_item}: a numeric representing the index of the item that should be asked next.
//'
//' @details Selection approach is specified in the \code{selection} slot of the \code{Cat} object.
//' 
//' The minimum expected posterior variance criterion is used when the \code{selection}
//'  slot is \code{"EPV"}.
//' 
//' The maximum Fisher's information criterion is used when the \code{selection}
//'   slot is \code{"MFI"}.
//'   
//' The maximum likelihood weighted information criterion is used when the \code{selection}
//' slot is \code{"MLWI"}.
//' 
//' The maximum posterior weighted information criterion is used when the \code{selection}
//' slot is \code{"MPWI"}.
//'  
//' The maximum expected information criterion is used when the \code{selection}
//' slot is \code{"MEI"}.
//' 
//' The maximum Kullback-Leibler information criterion is used when the \code{selection}
//' slot is \code{"KL"}.
//' 
//' The maximum likelihood weighted Kullback-Leibler information criterion is used when the \code{selection}
//' slot is \code{"LKL"}.
//' 
//' The maximum posterior weighted Kullback-Leibler information criterion is used when the \code{selection}
//' slot is \code{"PKL"}.
//' 
//' The ??????????? criterion is used when the \code{selection}
//' slot is \code{"MFII"}.
//' 
//' A random number generator is used when the \code{selection}
//' slot is \code{"RANDOM"}.
//' 
//' 
//' @examples
//' \dontrun{
//'## Create Cat object
//'data(npi)
//'ltm_cat <- ltmCat(npi)
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
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//' 
//' The \code{"RANDOM"} item selection criterion uses the package \code{RcppArmadillo} to randomly
//' choose the next item among unasked questions.  \code{RcppArmadillo} provides an exact reproduction
//' of R's \code{sample} function that can be called from C++.
//' 
//' 
//' @seealso \code{\link{estimateTheta}}, \code{\link{expectedPV}}, \code{\link{fisherInf}}
//'  
//' @export
// [[Rcpp::export]]
List selectItem(S4 catObj) {
  return Cat(catObj).selectItem();
}

//' Expected Kullback-Leibeler information
//'
//' Calculates the expected Kullback-Leibeler information for an individual question item
//' 
//' @return The function returns a numeric indicating the KL information for the desired item, given the current answer profile and ability parameter estimate
//' 
//' @param catObj An object of class \code{Cat}
//' @param item An integer indicating the index of the question item
//'
//' @details 
//'  Binary details (Due to the conditional independence assumption, we only need to calculate the expected value for potential new items.)
//' 
//' @examples
//' \dontrun{
//'## Create Cat object
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//' 
//' @seealso \code{\link{likelihoodKL}} and/or \code{\link{posteriorKL}} for alternative KL methods
//'
//' @export
// [[Rcpp::export]]
double expectedKL(S4 catObj, int item) {
  item = item - 1;
  return Cat(catObj).expectedKL(item);
}

//' Expected Kullback-Leibeler information, weighted by the likelihood
//'
//' Calculate the expected Kullback-Leibeler information, weighted by the likelihood
//' 
//' @return A value indicating the LKL information for the desired item, given the current answer profile and ability estimate.
//' 
//' @param catObj An object of class \code{Cat}
//' @param item An integer indicating the index of the question item
//'
//' @details The LKL calculation follows the same procedure as \code{expectedKL}, except it requires weighting the different potential values of \eqn{\theta_0} by the likelihood.
//'  Thus, the equation is
//'
//'  Binary details:
//'  
//'  Due to the conditional independence assumption, we only need to calculate the expected value for potential new items.
//'  
//' @examples
//' \dontrun{
//'## Create Cat object
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//' 
//'  
//' @seealso \code{\link{expectedKL}} and/or \code{\link{posteriorKL}} for alternative KL methods 
//'  
//' @export
// [[Rcpp::export]]
double likelihoodKL(S4 catObj, int item) {
  item = item - 1;
  return Cat(catObj).likelihoodKL(item);
}

//' Expected Kullback-Leibeler information, weighted by the posterior
//'
//' Calculate the expected Kullback-Leibeler information, weighted by the posterior
//' 
//' @return A value indicating the posterior KL information for the desired item, given the current answer profile and ability estimate.
//' 
//' @param catObj An object of class \code{Cat}
//' @param item An integer indicating the index of the question item
//'
//' @details We will follow the same procedure as \code{expectedKL}, except we will weight the different potential values of \eqn{\theta_0} by the posterior.
//'
//'Due to the conditional independence assumption, we only need to calculate the expected value for potential new items.
//'
//' @examples
//' \dontrun{
//'## Create Cat object
//'data(npi)
//'ltm_cat <- ltmCat(npi)
//'
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
//' 
//' @seealso \code{\link{likelihoodKL}} and/or \code{\link{expectedKL}} for alternative KL methods
//' 
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
//' @return A function \code{lookAhead} returns a \code{list} of one element, \code{estimates}, where the first column is the possible response option to the question the respondent
//' is currently answering and the second column is the next item that should be asked given each response.
//' 
//' @examples
//' \dontrun{
//' ## Create Cat object
//' data(npi)
//' ltm_cat <- ltmCat(npi)
//' 
//'## Store example answers
//'setAnswers(ltm_cat) <- c(1,0,1,0,1, rep(NA, 35))
//'
//'## What should be asked next if respondent is currently answering item 6
//'lookAhead(ltm_cat, 6)
//'}
//' 
//' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery,
//'  Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
//'  
//' @note This function is to allow users to access the internal functions of the package. During item selection, all calculations are done in compiled C++ code.
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
//' @details The stopping rule thresholds are stored in the following Cat object slots:
//' \code{lengthThreshold}, \code{seThreshold}, \code{infoThreshold}, and \code{gainThreshold}. The override
//'  thresholds are stored in the following Cat object slots: \code{lengthOverride}, \code{gainOverride}.  
//'  A value of \code{NA} indicates the rule will not be used in evaluating if further questions should be administered.
//'  A user can specify any combination of stopping rules and/or overrides.  
//' 
//' 
//' @return The function \code{checkStopRules} returns a boolean.  \code{TRUE} indicates all specified stopping rules are met
//'   and no specified overrides are met.  No further items should be adminstered.  \code{FALSE} indicates at least one specified
//'    stopping rule is not met, or if any specified override threshold is met.  Additional items should be administered.
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
//' @examples
//' \dontrun{
//'## Create Cat object
//'data(npi)
//'ltm_cat <- ltmCat(npi)
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
//'}
//'
//'@references
//'
//'Babcock, Ben, and David J. Weiss. 2009. ``Termination Criteria in Computerized Adaptive Tests: Variable-Length CATs are not Biased." Proceedings of the 2009 GMAC Conference on Computerized Adaptive Testing. Vol. 14.
//' 
//' 
//' @seealso \code{\link{Cat-class}}, \code{\link{estimateSE}}, \code{\link{expectedPV}}, \code{\link{fisherInf}}
//' 
//' @export
// [[Rcpp::export]]
bool checkStopRules(S4 catObj) {
	std::vector<bool> answer = Cat(catObj).checkStopRules();
  return answer[0];
}




