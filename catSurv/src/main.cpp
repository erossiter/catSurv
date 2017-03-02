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

//' Probability of a Response, conditional on \eqn{\theta}.
//'
//' \code{probability} calculates the probability of an observed response to a specific question, conditional on \eqn{\theta}.
//'
//' @param cat_df An object of class \code{Cat}
//' @param theta A double indicating the value for \eqn{\theta_j}
//' @param question An integer indicating the index of the question
//'
//' @return A numeric vector consisting of the probability of a correct response (binary) or the probability of being less than each element of item \eqn{i} 's difficulty parameter.
//'
//' @details 
//'  Note: this function is overloaded, due to different output types of dichotomous vs. polytomous implementations (outputs single value for binary implementation,
//'  vector of values for polytomous implementation)
//'
//'  For the binary case, the probability of a correct response for respondent \eqn{j} on item \eqn{i} is
//'  
//'  \deqn{Pr(y_{ij}=1|\theta_j)=c_i+(1-c_i)\frac{\exp(a_i + b_i \theta_j)}{1+\exp(a_i + b_i \theta_j)}}{Pr(y_ij = 1 | \theta_j) = c_i + (1 - c_i)(exp(a_i + b_i \theta_j))/(1 + exp(a_i + b_i \theta_j))}
//'
//'  where \eqn{\theta_j} is respondent \eqn{j} 's position on the latent scale of interest, \eqn{a_i} is item \eqn{i} 's discrimination parameter,
//'  \eqn{b_i} is item \eqn{i} 's difficulty parameter, and \eqn{c_i} is item \eqn{i} 's guessing parameter.
//'
//'  For categorical questions, the probability of a response in category \eqn{k} or lower for respondent \eqn{j} on item \eqn{i} is
//' 
//'  \deqn{Pr(y_{ij} <  k|\theta_j)=\frac{\exp(\alpha_{ik} - \beta_i \theta_{ij})}{1+\exp(\alpha_{ik} - \beta_i \theta_{ij})}}{Pr(y_ij < k | \theta_j) = (exp(\alpha_ik - \beta_i \theta_ij))/(1 + exp(\alpha_ik - \beta_i \theta_ij))}
//'
//'  where \eqn{\theta_j} is respondent \eqn{j} 's position on the latent scale of interest, \eqn{\alpha_ik} the \eqn{k}-th element of item \eqn{i} 's difficulty parameter, 
//'  \eqn{\beta_i} is discrimination parameter vector for item \eqn{i}. Notice (1) the inequality on the left side and (2) the absence of guessing parameters.
//'
//'  Note: the function for polytomous implementation does not return values, but rather alters the object \code{ret_prob} in memory
//'
//' @examples
//'  
//' @seealso \code{\link{Cat}} for information on the item parameters: discrimination, difficulty, and guessing.
//'  
//' @export
// [[Rcpp::export]]
std::vector<double> probability(S4 cat_df, NumericVector theta, IntegerVector question) {
	Cat cat = Cat(cat_df);
	double t = theta[0];
	int q = question[0];
	if(q == 0){
	  throw std::domain_error("Must use a question number applicable to Cat object.");
	}
	return cat.probability(t, q);
}

//' Likelihood of the Specified Response Set
//'
//' The likelihood of a respondent, with ability parameter \eqn{\theta}, having offered the specific set of responses stored in the \code{Cat} objects \code{answers} slot, conditional on the item-level parameters.
//'
//' @param cat_df An object of class \code{Cat}
//' @param t A numeric for the value of theta (position on the latent scale of interest)
//'
//' @return A numeric value of the likelihood of the respondent having offered the provided response profile.
//'
//' @details For binary \code{Cats}, letting \deqn{q_i(\theta_j) = 1-p_i(\theta_j)}{q_i(\theta_j) = 1 - p_i(\theta_j)}, the likelihood function associated with the responses profile \eqn{y_j} is
//'
//'  \deqn{L(\theta_j|\mathbf{y}_{j})=\prod^{J}_{i=1}\Big(p_i(\theta_j)^{y_{ij}}q_i(\theta_j)^{(1-y_{ij})}\Big)= \exp\Big[ \sum^{J}_{i=1}\Big(y_{ij} \log(p_i(\theta_j)) + (1-y_{ij})\log(q_i(\theta_j))\Big) \Big]}{L(\theta_j| y_j) = \prod^{J}_{i = 1}(p_i(\theta_j)^{y_ij} q_i(\theta_j)^{1 - y_ij}) = exp(\sum^{J}_{i = 1} (y_ij log(p_i(\theta_j)) + (1 - y_ij) log(q_i(\theta_j))))}
//'
//'  where \eqn{y_j} is evaluated based only on the questions the respondent has answered.
//'
//'  In the polytomous implementation, for each item \eqn{i} there are \eqn{K_i} response options, which may differ across items.  
//'  There is therefore a vector of threshold parameters defined as \deqn{\mathbf{\alpha}_i=(\alpha_{i,0}, \alpha_{i,1}, \ldots, \alpha_{i,K_i})}{\alpha_i = \alpha_{i,0}, \alpha_{i,1}, ... , \alpha_{i,K_i}}, 
//'  with \deqn{\alpha_{i,0} < \alpha_{i,1} \le \alpha_{i,2} \le \ldots, < \alpha_{i,K_i}}{\alpha_{i,0} < \alpha_{i,1} \le \alpha_{i,2} \le \ldots, < \alpha_{i,K_i}} , \deqn{\alpha_{i,0} = -\infty}{\alpha_{i,0} = -\infty}, and \deqn{\alpha_{i,K_i} = \infty}{\alpha_{i,K_i} = \infty}.  
//'  Thus for a categorical question with \eqn{K} possible answers, \eqn{K-1} difficulty parameters are calculated (since \eqn{\alpha_{i,0}} and \eqn{\alpha_{i,K}} are known).  
//'  In addition, each item is associated with a discrimination parameter \eqn{\beta_i} indicating how well each item corresponds to the underlying trait in question. 
//'  To calculate the likelihood function, estimate \deqn{P^*_{ijk} = Pr(y_{ij} < k| \theta_j)}{P*_ijk = Pr(y_ij < k| \theta_j)} for each response option. The probability of observing each possible response can then be calculated as
//'                                                                                  
//'  \deqn{P_{ijk} = P^\ast_{ij, k} - P^\ast_{ij, k-1}}{P_ijk = P*_{ij,k} - P*_{ij,k-1}}. 
//'
//'  Note that \deqn{P^\ast_{ij0} =0 }{P*_{ij,0} = 0} and \deqn{P^\ast_{ijK+1} = 1}{P*_{ij,K+1} = 1} in all cases. The likelihood function is therefore,
//'
//'  \deqn{L(\theta_j) = \prod_{i=1}^n\prod_{k=1}^{K_i} P_{ijk}^{I(y_{ij}=k)} = \exp \Big[ \sum_{i=1}^n\sum_{k=1}^{K_i} \log \Big( P_{ijk}^{I(y_{ij}=k)} \Big) \Big]}{L(\theta_j) = \prod_{i = 1}^n \prod_{k = 1}^{K_i} P_ijk^{I(y_ij = k)} = exp[\sum_{i = 1}^n \sum_{k = 1}^{K_i} log(P_ijk^{I(y_ij = k)})]},
//'
//'  where \deqn{I(\cdot)}{I(.)} is an indicator function that evaluates to 1 when the equatliy holds and zero otherwise.
//'  
//' @examples
//' 
//' 
//'  
//' @seealso \code{\link{probability}} for individual probability calculations
//'  
//' @export
// [[Rcpp::export]]
double likelihood(S4 cat_df, double t) {
	return Cat(cat_df).likelihood(t);
}

//' The prior value for the respondent's position on the latent trait scale
//'
//' Calculates the prior value for a respondent's position on the latent scale of interest
//'
//' @param x A numeric value at which to evaluate the prior
//' @param c The \code{priorName} slot of a \code{Cat} object; indicates the distribution
//' @param p The \code{priorParams} slot of a \code{Cat} object
//'
//' @return A vector consisting of prior value, \eqn{\pi(x)}, given the value \eqn{x}
//'
//' @details Note: \eqn{x} needs to be either UNIFORM, NORMAL, or STUDENT_T parameters, which control the shape of the prior.
//' @examples
//' 
//'  
//'  
//' @seealso \code{\link{Cat}} for additional information on priors; 
//'  \code{\link{dLL}} and/or \code{\link{d2LL}} for application of priors
//'  
//' @export
// [[Rcpp::export]]
double prior(NumericVector x, CharacterVector c, NumericVector p) {
  std::string name = Rcpp::as<std::string>(c);
  std::vector<double> params = Rcpp::as<std::vector<double> >(p);
  return Prior(name, params).prior(x[0]);
}

//' The first derivative of the log-likelihood
//' 
//' When \code{usePrior = FALSE}, this function evaluates the first derivative of the log-likelihood evaluated at point \eqn{\theta}.  
//' When \code{usePrior = TRUE}, this function evaluates the first derivative of the log-posterior evaluated at point \eqn{\theta}. 
//' 
//' @param cat_df An object of class \code{Cat}
//' @param theta A double indicating the value for \eqn{\theta_j}
//' @param use_prior A logical indicating whether to use the prior parameters in estimation
//' 
//' @return A double value of the derivative of the log-likelihood (or log-posterior) for a respondent's answer profile.
//' 
//' @details For the dichotomous case, \deqn{P_{ij}}{P_ij} is the probability that respondent \eqn{j} will answer question \eqn{i} correctly, conditioned on the respondent's ability parameter \eqn{\theta_j}.  
//'   Let \eqn{\mu_\theta} be the prior mean and \eqn{\sigma_\theta} be the prior standard deviation.  
//'   Further, let \deqn{Q_{ij} = 1 - P_{ij}}{Q_ij = 1 - P_ij}. Using this notation, the first derivative of the log-likelihood is given by:
//' 
//'   \deqn{L_\theta = \sum_{i=1}^n b_i\Big(\frac{P_{ij} - c_i}{P_{ij}(1-c_i)} \Big)(y_{ij} - P_{ij}) }{L_\theta = \sum_{i = 1}^n b_i ((P_ij - c_i)/(P_ij (1 - c_i)))(y_ij - P_ij)} 
//' 
//'   The first derivative of the log-posterior is:
//' 
//'   \deqn{L_\theta = \sum_{i=1}^n \Big [ b_i\Big(\frac{P_{ij} - c_i}{P_{ij}(1-c_i)} \Big)(y_{ij} - P_{ij}) \Big ] -  \Big(\frac{\theta_j - \mu_\theta}{\sigma^2_\theta} \Big)}{L_\theta = \sum_{i = 1}^n [ b_i ((P_ij - c_i)/(P_ij (1 - c_i)))(y_ij - P_ij)] - ((\theta_j - \mu_\theta)(\sigma^2_\theta))}
//'
//'   For the polytomous case, \deqn{P_{ijk} = P^*_{ij,k} - P^*_{ij,k-1}}{P_ijk = P*_{ij,k} - P*_{ij,k-1}} and \deqn{P^*_{ijk} = Pr(y_{ij}<k|\theta_j)}{P*_ijk = Pr(y_ij < k | \theta_j)} and \deqn{Q_{ijk}^*= 1- P^*_{ijk}}{Q*_ijk = 1 - P*_ijk}. The log-likelihood is then given by:
//'
//'   \deqn{L = \sum^n_{i=1}\sum^{m_i}_{k=1}I(y_{ijk}=k)\log P_{ijk}}{L = \sum^n_{i = 1} \sum^{m_i}_{k = 1} I(y_ijk = k) log P_ijk},
//'
//'   where \deqn{I(\cdot)}{I(.)} is an indicator function that evaluates to 1 when the condition is met and 0 otherwise.  
//'   Calculating the estimate of \eqn{\theta} requires both the first and the second derivatives of the log-likelihood function with respect to \eqn{\theta_j}. Therefore, the first derivative of \eqn{L} with respect to \eqn{\theta_j} is:
//'
//'   \deqn{\frac{\partial L}{\partial \theta_j} &= \sum_{i=1}^n\sum^{m_i}_{k=1} I(y_{ijk}=k)\Big[- \beta_i \Big ( \frac{w_{ik}-w_{i,k-1}}{P_{ik}} \Big )\Big]}{{\partial L}/{\partial \theta_j} = \sum_{i = 1}^n \sum^{m_i}_{k = 1} I(y_ijk = k)[-\beta_i ((w_{ik} - w_{i,k-1})/(P_ik))]}
//'
//'   where \deqn{w_{i,k-1} = P^*_{i,k-1}Q^*_{i,k-1}}{w_{i,k-1} = P*_{i,k - 1}Q*_{i,k - 1}} and \deqn{w_{ik}=P^*_{ik}Q^*_{ik}}{w_ik = P*_ik Q*_ik}.
//'
//'   The log posterior is:
//'
//'   \deqn{\frac{\partial L}{\partial \theta_j} &= \sum_{i=1}^n\sum^{m_i}_{k=1} I(y_{ijk}=k)\Big[- \beta_i \Big ( \frac{w_{ik}-w_{i,k-1}}{P_{ik}} \Big ) \Big] - \Big(\frac{\theta_j - \mu_\theta}{\sigma^2_\theta} \Big) \Big]}{(\partial L)/(\partial \theta_j) = \sum_{i = 1}^n \sum^{m_i}_{k = 1} I(y_ijk = k)[-\beta_i ((w_{ik} - w_{i,k-1})/(P_{ik})) - ((\theta_j - \mu_\theta)/(\sigma^2_\theta))]}
//'
//' Note: This method is only be available using the normal prior distribution 
//'
//' @examples
//' 
//'  
//'  
//' @seealso \code{\link{Cat}} and/or \code{\link{prior}} for information on priors 
//'  
//' @export
// [[Rcpp::export]]
double dLL(S4 &cat_df, double theta, bool use_prior){
  return Cat(cat_df).dLL(theta, use_prior);
}

//' The second derivative of the log likelihood
//'
//'When \code{usePrior = FALSE}, this function evaluates the second derivative of the log-likelihood evaluated at point \eqn{\theta}.  
//'When \code{usePrior = TRUE}, this function evaluates the second derivative of the log-posterior evaluated at point \eqn{\theta}. 
//'
//' @param cat_df An object of class \code{Cat}
//' @param theta A double indicating the potential value for \eqn{\theta_j}
//' @param use_prior A logical indicating whether to use the prior parameters in estimation
//' 
//' @return A double value of the second derivative of the log-likelihood (or log-posterior) for a respondent's answer profile.
//' 
//' @details For the dichotomous case, the  value (in terms of \deqn{y_{ij}}{y_ij}) of the second derivative of the log-likelihood is:
//'
//'   \deqn{\lambda_{\theta \theta} = - \sum_{i=1}^n b_i^2\Big(\frac{P_{ij} - c_i}{1-c_i} \Big)^2\frac{Q_{ij}}{P_{ij}}}{\lambda_{\theta \theta} = - \sum_{i = 1}^n b_i^2 ((P_ij - c_i)/(1 - c_i))^2 (Q_ij)/(P_ij)}
//'
//'   The second derivative of the log-posterior is:
//'
//'   \deqn{\lambda_{\theta \theta} = - \sum_{i=1}^n \Big [ b_i^2\Big(\frac{P_{ij} - c_i}{1-c_i} \Big)^2\frac{Q_{ij}}{P_{ij}} \Big ] - \frac{1}{\sigma^2_\theta}}{\lambda_{\theta \theta} = - \sum_{i = 1}^n [b_i^2 ((P_ij - c_i)/(1 - c_i))^2 (Q_ij)/(P_ij)] - 1/(\sigma^2_\theta)}
//'
//'   Note: In the binary case, the observed and expected information are identical.
//' 
//'   For the polytomous case, the second derivative of \eqn{L} with respect to \eqn{\theta_j} is:
//'
//'   \deqn{\frac{\partial^2L}{\partial\theta_j^2} = \sum_{i=1}^n -\beta_i^2 \sum_{k=1}^{m_i} I(y_{ijk}=k) \Big [ \frac{-w_{i,k-1}(Q^*_{i,k-1}-P^*_{i,k-1}) + w_{ik}(Q^*_{ik}-P^*_{ik})}{P_{i}}-\frac{(w_{ik}-w_{i,k-1})^2}{P_{ik}^2} \Big ]}{(\partial^2L)/(\partial\theta_j^2) = \sum_{i = 1}^n -\beta_i^2 \sum_{k = 1}^{m_i} I(y_ijk = k) [ (-w_{i,k-1}(Q*_{i,k-1} - P*_{i,k-1}) + w_{ik}(Q*_{ik} - P*_{ik}))/(P_i) - ((w_{ik} - w_{i,k-1})^2)/(P_{ik}^2)]}
//'
//'   The second derivative of the log-posterior is:
//'
//'   \deqn{\frac{\partial^2L}{\partial\theta_j^2} = \sum_{i=1}^n -\beta_i^2 \sum_{k=1}^{m_i} I(y_{ijk}=k)  \Big [ \frac{-w_{i,k-1}(Q^*_{i,k-1}-P^*_{i,k-1}) + w_{ik}(Q^*_{ik}-P^*_{ik})}{P_{i}}-\frac{(w_{ik}-w_{i,k-1})^2}{P_{ik}^2} \Big ] - \frac{1}{\sigma_\theta^2} }{(\partial^2L)/(\partial\theta_j^2) = \sum_{i = 1}^n -\beta_i^2 \sum_{k = 1}^{m_i} I(y_ijk = k) [(-w_{i,k-1}(Q*_{i,k-1} - P*_{i,k-1}) + w_{ik}(Q*_{ik} - P*_{ik}))/(P_i) - ((w_{ik} - w_{i,k-1})^2)/(P_{ik}^2)] - 1/(\sigma_\theta^2)} 
//' 
//'   Note: This method is only be available using the normal prior distribution
//'
//' @examples
//' 
//'  
//'  
//' @seealso \code{\link{Cat}} and/or \code{\link{prior}} for information on priors 
//'  
//' @export
// [[Rcpp::export]]
double d2LL(S4 &cat_df, double theta, bool use_prior){
  return Cat(cat_df).d2LL(theta, use_prior);
}

//' Estimate of the respondent's ability parameter
//'
//' This function takes a \code{Cat} object and returns the expected value of the ability parameter conditioned on the observed answers and the item calibrations.
//'
//' @param cat_df An object of class \code{Cat}
//'
//' @return A vector consisting of the expected value of the ability parameter
//'
//' @details 
//'   The expected a posteriori (EAP) approach:
//'
//'   The expected value of \eqn{\theta_j} is:
//'
//'   \deqn{\frac{\int_{-\infty}^\infty \theta_j \mbox{L}(\theta_j) \pi(\theta_j) d\theta_j}{\int_{-\infty}^\infty \mbox{L}(\theta_j) \pi(\theta_j) d\theta_j}}{(\int_{-\infty}^\infty \theta_j L(\theta_j) \pi(\theta_j) d\theta_j)/(\int_{-\infty}^\infty L(\theta_j) \pi(\theta_j) d\theta_j)}
//'
//'   where \eqn{L(\theta_j)} is defined in \code{Likelihood}, and \eqn{\pi(\theta_j)} is the prior distribution for \eqn{\theta_j}.  
//'
//'   The modal a posteriori (MAP) approach:
//'
//'   Note: This method is only available using the normal prior distribution.
//'
//'   To estimate \eqn{\theta_j}, the following equation is iteratively solved, beginning with an initial value of \eqn{\theta_j}, and continuing until the change in the estimate is below the pre-specified tolerance. Note that \eqn{t} indexes the iteration.
//'
//'   \deqn{[\hat{\theta_j}]_{(t+1)} = [\hat{\theta_j}]_{(t)} - \Big [ \frac{\frac{\partial L}{\partial\theta_j}}{\frac{\partial^2L}{\partial\theta_j^2}}\Big ]}{\hat{\theta_j}_{(t + 1)} = \hat{\theta_j}_{(t)} - ((\partial L)/(\partial\theta_j))/((\partial^2L)/(\partial\theta_j^2))}
//'
//'   where the derivatives are evaluated at \eqn{\hat{\theta_j}_{(t)}}.
//' 
//'   The maximum likelihood (MLE) approach:
//'
//'   Note: When MLE will not work, \code{estimateTheta} is calculated according to the \code{estimationDefault} slot in \code{Cat} object.
//'
//'   To estimate \eqn{\theta_j}, the following equation is iteratively solved, beginning with an initial value of \eqn{\theta_j}, and continuing until the change in the estimate is below the pre-specified tolerance. Note that \eqn{t} indexes the iteration.
//'
//'   \deqn{[\hat{\theta_j}]_{(t+1)} = [\hat{\theta_j}]_{(t)} - \Big [ \frac{\frac{\partial L}{\partial\theta_j}}{\frac{\partial^2L}{\partial\theta_j^2}}\Big ]}{\hat{\theta_j}_{(t + 1)} = \hat{\theta_j}_{(t)} - ((\partial L)/(\partial\theta_j))/((\partial^2L)/(\partial\theta_j^2))}
//'
//'   where the derivatives are evaluated at \eqn{\hat{\theta_j}_{(t)}}.
//' 
//'   The weighted maximum likelihood approach (WLE) approach:
//'
//'   Estimating \eqn{\theta_j} requires finding the root of the following function using the ``Brent'' method in the \code{gsl} library.
//'
//'   \deqn{W = L_\theta + \frac{B(\theta)}{2I(\theta)}}{W = L_\theta + B(\theta)/(2 I(\theta))}
//'
//'   Where \eqn{L_\theta} is defined without use of the prior, and \eqn{I(\theta)} is the test information for respondent \eqn{j}.
//'
//'   For the dichotomous case, 
//'
//'   \deqn{B(\theta)=\sum_i \frac{P_{ij}^\prime P_{ij}^{\prime\prime} }{P_{ij}Q_{ij}}}{B(\theta) = \sum_i (P'_ij P''_ij)/(P_ij Q_ij)}
//'
//'   \deqn{P_{ij}}{P_ij} and \deqn{Q_{ij}}{Q_ij} are as defined in \code{Probability} and \code{Likelihood}, and
//'
//'   \deqn{P^{\prime}_{ij} = b_i (1 - c_i) \frac{\exp{(a_i+b_i\theta_j)}}{(1 + \exp{(a_i+b_i\theta_j)})^2}}{P'_ij = b_i (1 - c_i) (exp(a_i + b_i \theta_j))/(1 + exp(a_i + b_i\theta_j))^2}
//'   
//'   \deqn{P^{\prime\prime}_{ij}=b^2  \exp{(a_i+b_i\theta_j)}  (1 - \exp{(a_i+b_i\theta_j)})  \frac{(1 - c)}{(1 + \exp{(a_i+b_i\theta_j)})^3}}{P''_ij = b^2  exp(a_i + b_i \theta_j)(1 - exp(a_i + b_i\theta_j)) ((1 - c)/(1 + exp(a_i + b_i \theta_j))^3)}
//'  
//'   For the categorical case, 
//'
//'   \deqn{B=\sum_i \sum_k \frac{P_{ijk}^\prime P_{ijk}^{\prime\prime}}{P_{ijk}}}{B = \sum_i \sum_k (P'_ijk P''_ijk)/(P_ijk)}
//'
//'   where, \deqn{P_{ijk}}{P_ijk} is defined as above and \deqn{P^\ast_{ijk}}{P*_ijk} is defined as above with one and zero at the extremes.
//'
//'
//'   \deqn{P^{\ast \prime}_{ijk}=-\beta P^\ast_{ijk}  Q^\ast_{ijk}}{P*'_ijk = -\beta P*_ijk  Q*_ijk}
//'   
//'   \deqn{P^{\ast \prime \prime}_{ijk} = -\beta  (P^{\ast \prime}_{ijk} - 2  P^{\ast} _{ijk}  P^{\ast \prime}_{ijk})}{P*''_ijk = -\beta  (P*'_ijk - 2  P* _ijk  P*'_ijk)}
//'
//'   Finally,
//'
//'   \deqn{P^{\prime}_{ijk}=P^{\ast \prime}_{ij,k+1}-P^{\ast \prime}_{ij,k}}{P'_{ijk} = P*'_{ij,k+1} - P*'_{ij,k}}
//'                     
//'   \deqn{P^{\prime\prime}_{ijk}=P^{\ast \prime\prime}_{ij,k+1}-P^{\ast \prime\prime}_{ij,k}}{P''_{ijk} = P*''_{ij,k+1} - P*''_{ij,k}}
//'
//' @examples
//' 
//'  
//' @seealso \code{\link{probability}} and/or \code{\link{likelihood}} for calculation of P and P*  
//'  
//' @export
// [[Rcpp::export]]
double estimateTheta(S4 cat_df) {
	return Cat(cat_df).estimateTheta();
}

//' Observed Information
//'
//' This function calculates the observed information of the likelihood evaluated at the input value \eqn{\theta} for a specific item.
//'
//' @param cat_df An object of class \code{Cat}
//' @param theta A double indicating the value of \eqn{\theta_j}
//' @param item An integer indicating the index of the question
//'
//' @return The value of the observed information of the likelihood, given \eqn{\theta}, for a specific question.
//' 
//' @details The observed information is equivalent to the negative second derivative of the log-likelihood.
//'   Note: This function should never be called when the respondent has answered no questions.
//'
//'   For the binary case:
//'
//'   \deqn{\lambda_{\theta \theta} = b_i^2\Big(\frac{P_{ij} - c_i}{1-c_i} \Big)^2\frac{Q_{ij}}{P_{ij}}}{\lambda_{\theta \theta} = b_i^2 ((P_ij - c_i)/(1 - c_i))^2 (Q_ij)/(P_ij)}
//'
//'   For the categorical case:
//'
//'   \deqn{P_{ijk} = P^*_{ijk} - P^*_{ij,k-1}}{P_{ijk} = P*_{ijk} - P*_{ij,k-1}} and \deqn{P^*_{ijk} = Pr(y_{ij}<k|\theta_j)}{P*_ijk = Pr(y_ij < k | \theta_j)} as defined in \code{Likelihood}. 
//'   \deqn{Q_{ijk}^*= 1- P^*_{ijk}}{Q*_ijk = 1 - P*_ijk}, \deqn{w_{i,k-1} = P^*_{i,k-1}Q^*_{i,k-1}}{w_{i,k-1} = P*_{i,k-1} Q*_{i,k-1}}, and \deqn{w_{ik}=P^*_{ik}Q^*_{ik}}{w_ik = P*_ik Q*_ik} as defined in \code{dLL}.
//'
//'   To calculate the categorical observed information requires evaluating the below equation for question \eqn{i}, assuming response \eqn{k}:  
//'
//'   \deqn{\frac{\partial^2L}{\partial\theta_j^2} & -\beta_i^2 \Big [ \frac{-w_{i,k-1}(Q^*_{i,k-1}-P^*_{i,k-1}) + w_{ik}(Q^*_{ik}-P^*_{ik})}{P_{ik}}-\frac{(w_{ik}-w_{i,k-1})^2}{P_{ik}^2} \Big ]}{(\partial^2L)/(\partial\theta_j^2) = -\beta_i^2 [(-w_{i,k-1}(Q*_{i,k-1} - P*_{i,k-1}) + w_{ik}(Q*_{ik} - P*_{ik}))/(P_{ik}) - ((w_{ik} - w_{i,k-1})^2)/(P_{ik}^2)]}
//'
//' @examples
//' 
//' 
//'  
//' @seealso \code{\link{probability}} and/or \code{\link{likelihood}} for calculation of P and P*;  
//'   \code{\link{estimateTheta}} for calculation of \eqn{\theta};  
//'   \code{\link{expectedObsInf}} for further application of observed information
//'  
//' @export
// [[Rcpp::export]]
double obsInf(S4 cat_df, double theta, int item) {
  item = item - 1;
  return Cat(cat_df).obsInf(theta, item);
}

//' Expected Observed Information
//'
//' This function calculates the expected information, which is the observed information attained from a specific response set times the probability of that profile occurring.
//'
//' @param cat_df An object of class \code{Cat}
//' @param item An integer indicating the index of the question
//' 
//' @return A value of the expected information 
//' 
//' @details Binary details:
//'
//'   \deqn{EI=P(y_{ij} = 1)J_{y_{ij}=1} + P(y_{ij} = 0)J_{y_{ij}=0}}{EI = P(y_ij = 1)J_{y_ij = 1} + P(y_ij = 0) J_{y_ij = 0}} 
//'
//'   Categorical details:
//'
//'   \deqn{EI = \sum_k^K P(y_{ij} = k)J_{y_{ij}=k}}{EI = \sum_k^K P(y_ij = k) J_{y_ij = k}}
//'
//' @examples
//' 
//'  
//'  
//' @seealso \code{\link{probability}} and/or \code{\link{likelihood}} for calculation of P and P*;  
//'   \code{\link{estimateTheta}} for calculation of \eqn{\theta};  
//'   \code{\link{obsInf}} for observed information calculation
//'  
//' @export
// [[Rcpp::export]]
double expectedObsInf(S4 cat_df, int item) {
  item = item - 1;
  return Cat(cat_df).expectedObsInf(item);
}

//' Fisher's Information
//'
//' This function calculates the expected value of the observed information of the likelihood evaluated at the input value \eqn{\theta}.
//'
//' @param cat_df An object of class \code{Cat}
//' @param theta A double indicating the potential value for \eqn{\theta_j}
//' @param item An integer indicating the index of the question
//'
//' @return The expected value of the observed information.
//' 
//' @details For the dichotomous case, this is equivalent to the observed information.  
//'   The graded response model requires additional calculations.
//'
//'   The equation for the likelihood is given in \code{Likelihood}, where \deqn{P_{ijk} = P^*_{ijk} - P^*_{ij,k-1}}{P_ijk = P*_{ijk} - P*_{ij,k-1}} and \deqn{P^*_{ijk} = Pr(y_{ij}<k|\theta_j)}{P*_ijk = Pr(y_ij < k | \theta_j)}. 
//'   \code{dLL} also defines \deqn{Q_{ijk}^\ast= 1- P^*_{ijk}}{Q*_ijk = 1 - P*_ijk}, and \deqn{w_{ij,k}=P^\ast_{ij,k}Q^\ast_{ij,k}}{w_{ij,k} = P*_{ij,k} Q*_{ij,k}}.
//'   Fisher information is therefore calculated (by equation 8.23 in Baker and Kim):
//'
//'   \deqn{I_i(\theta_j) = \sum_k^K\beta^2_i\frac{(w_{ijk} - w_{i,k-1})^2}{P^\ast_{ijk} - P^\ast_{ij,k-1}}}{I_i(\theta_j) = \sum_k^K \beta^2_i ((w_{ijk} - w_{i,k-1})^2)/(P*_{ijk} - P*_{ij,k-1})}
//' 
//' @examples
//' 
//'  
//'  
//' @seealso \code{\link{probability}} and/or \code{\link{likelihood}} for calculation of P and P*;  
//'   \code{\link{estimateTheta}} for calculation of \eqn{\theta};  
//'   \code{\link{obsInf}} for observed information calculation;
//'   \code{\link{fisherTestInfo}} for further application of Fisher's information
//'  
//' @export
// [[Rcpp::export]]
double fisherInf(S4 cat_df, double theta, int item) {
  item = item - 1;
  return Cat(cat_df).fisherInf(theta, item);
}

//' Fisher's Test Information
//'
//' This function calculates the total information gained for a respondent \eqn{j} for all answered items, conditioned on \eqn{theta}.
//'
//' @param cat_df An object of class \code{Cat}
//' 
//' @return The total information gained for a respondent, given a specific answer set and a value of \eqn{theta}.
//' 
//' @details \deqn{I_i(\theta_j)}{I_i(\theta_j)} is the Fisher's information for item \eqn{i} specific to \eqn{\theta_j} (see \code{FisherInf}). 
//'   Let \deqn{i\in[1, \ldots, n]}{i \in [1,...,n]} index the questions the respondent has already answered. The test information is then defined as:
//'
//' \deqn{I(\theta_j) = \sum_i^n I_i(\theta_j)}{I(\theta_j) = \sum_i^n I_i(\theta_j)}
//' 
//' @examples
//' 
//'  
//' 
//' @seealso \code{\link{probability}} and/or \code{\link{likelihood}} for calculation of P and P*;  
//'   \code{\link{estimateTheta}} for calculation of \eqn{\theta};
//'   \code{\link{fisherInf}} for further calculation of Fisher's information
//'  
//' 
//' @export
// [[Rcpp::export]]
double fisherTestInfo(S4 cat_df) {
  return Cat(cat_df).fisherTestInfo();
}

//' Estimate of the standard error for the posterior estimate
//'
//' This function estimates the standard error for the posterior estimate for the position \eqn{\theta_j} of a person on the latent scale
//'
//' @param cat An object of class \code{Cat}
//'
//' @return The posterior standard error for \eqn{\theta_j}
//'
//' @details 
//'   The EAP estimator:
//'   
//'   The posterior variance for \deqn{\theta^{(EAP)}_j}{\theta^{EAP}_j} is:
//'
//'   \deqn{\mbox{Var}(\hat{\theta}_j) = E[(\theta_j-\hat{\theta_j})^2]=\frac{\int(\theta_j-\hat{\theta_j})^2\pi(\theta_j)L(\theta_j)d\theta_j}{\int\pi(\theta_j)L(\theta_j)d\theta_j}}{Var(\hat{\theta}_j) = E[(\theta_j - \hat{\theta_j})^2] = (\int(\theta_j - \hat{\theta_j})^2 \pi(\theta_j) L(\theta_j) d\theta_j)/(\int \pi(\theta_j) L(\theta_j) d\theta_j)}
//'
//'   where \eqn{\hat{\theta}_j} is the chosen point estimate for the respondent on the latent scale. The standard error is then 
//' 
//'   \deqn{SE=\sqrt{\mbox{Var}(\hat{\theta}_j)}}{SE = \sqrt{Var(\hat{\theta}_j)}}
//'
//'   The MAP estimator:
//'
//'   Note: This is implemented only for the normal prior.
//'
//'   The variance of \deqn{\theta^{(MAP)}_j}{\theta^{MAP}_j} is defined as:
//'
//'   \deqn{\mbox{Var}(\hat{\theta}_j)=\frac{1}{I(\hat{\theta_j})+\frac{1}{\sigma_\theta^2}}}{Var(\hat{\theta}_j) = 1/(I(\hat{\theta_j}) + 1/(\sigma_\theta^2))}
//'
//'   where \eqn{I(\hat{\theta_j})} is defined as the test information evaluated at \eqn{\hat{\theta}}.The standard error is then 
//'
//'   \deqn{SE=\sqrt{\mbox{Var}(\hat{\theta}_j)}}{SE = \sqrt{Var(\hat{\theta}_j)}}
//'
//'   The MLE  estimator:
//'
//'   The variance of \deqn{\theta^{(MLE)}_j}{\theta^{MLE}_j} is defined as:
//'
//'   \deqn{\mbox{Var}(\hat{\theta}_j)=\frac{1}{I(\hat{\theta_j})}}{Var(\hat{\theta}_j) = 1/(I(\hat{\theta_j}))}
//'
//'   where \eqn{T(\hat{\theta_j})} is defined as the test information evaluated at \eqn{\hat{\theta}}.The standard error is then 
//'
//'   \deqn{SE=\sqrt{\mbox{Var}(\hat{\theta}_j)}}{SE = \sqrt{Var(\hat{\theta}_j)}}
//'
//'   The WLE estimator:
//'
//'   The variance of \deqn{\theta^{(WLE)}_j}{\theta^{WLE}_j} is defined as:
//'
//'   \deqn{\mbox{Var}(\hat{\theta}_j)\approx\frac{1}{I(\hat{\theta})}}{Var(\hat{\theta}_j)\approx 1/(I(\hat{\theta}))}
//'
//'   where \deqn{I(\hat{\theta_j})}{I(\hat{\theta_j})} is defined as the test information evaluated at \eqn{\hat{\theta}} and \eqn{B(\theta)} is defined in \code{estimateTheta}.The standard error is then 
//'
//'  \deqn{SE=\sqrt{\mbox{Var}(\hat{\theta}_j)}}{SE = \sqrt{Var(\hat{\theta}_j)}}
//'
//' @examples
//' 
//'  
//'  
//' @seealso \code{\link{estimateTheta}} for calculation of \eqn{\theta}
//'  
//' @export
// [[Rcpp::export]]
double estimateSE(S4 cat_df) {
	return Cat(cat_df).estimateSE();
}

//' Expected Posterior Variance
//'
//' This function estimates the expected posterior variance for a respondent's estimated position on the latent trait for an item yet to be answered based on a respondent's position on the latent trait from the already-answered items.
//'
//' @param cat_df An object of class \code{Cat}
//' @param item An integer indicating the index of the question
//'
//' @return A numeric value indicating a respondent's expected posterior variance
//'
//' @details For a binary question:
//'
//'   \deqn{P(y_{ij} = 1 | \hat{\theta})Var(\hat{\theta} | y_{ij} = 1) + P(y_{ij} = 0 | \hat{\theta})Var(\hat{\theta} | y_{ij} = 0)}{P(y_ij = 1 | \hat{\theta}) Var(\hat{\theta} | y_ij = 1) + P(y_ij = 0 | \hat{\theta}) Var(\hat{\theta} | y_ij = 0)}
//'
//'   For a categorical question:
//'
//'   \deqn{\sum_{i=k}^k P(y_{ij} = k | \hat{\theta})Var(\hat{\theta} | y_{ij} = k)}{\sum_{i = k}^k P(y_ij = k | \hat{\theta}) Var(\hat{\theta} | y_ij = k)}
//'
//' @examples
//' 
//' 
//' @seealso \code{\link{probability}} for calculation of P;  
//'   \code{\link{estimateTheta}} for calculation of \eqn{\theta}
//'  
//' 
//' @export
// [[Rcpp::export]]
double expectedPV(S4 cat_df, int item) {
  item = item - 1.0;
  return Cat(cat_df).expectedPV(item);
}

//' Select the next item in the question set
//'
//' Select the next item in the question set based on the specified method
//' 
//' @param cat_df An object of class \code{Cat}
//'
//' @return It returns a list with two elements: 
//' (1) A dataframe containing a column with the indexes of unasked questions and a column with the values (calculated by the specified selection method) for those items, 
//' and (2) a numeric containing the index of the question that should be asked next.
//'
//' @details This function takes in a \code{Cat} object from \code{R} and constructs the \code{C++} representation.
//'
//'   The EPV method:
//'   
//'   This function takes in a \code{Cat} object from \code{R} and constructs the \code{C++} representation. It then calculates the expected posterior variance for each unanswered item. 
//'   
//'   The function returns a list with the following two elements:
//'   \code{all.estimates}: A data fame with two columns. The first column ("questions") should be the index of the question items and the second column ("EPV") of the expected posterior variance for that corresponding item. 
//'   There are as many rows in this data frame as there are unsanswered questions in the battery.
//'   \code{next.item}: A numeric vector with the index of the item with the lowest EPV value.
//'   
//'   See \code{expectedPV} for mathematical details.
//'   
//'   The MFI method:
//'   
//'   This function takes a \code{Cat} object and calculates Fisher's information for each unanswered item. It then finds the one item that maximizes Fisher's information, based on the respondent's position on the latent trait from the answered items. 
//'   
//'   The function returns a list with the following two elements:
//'   \code{all.estimates}: A data fame with two columns. The first column ("questions") should be the index of the question items and the second column of the expected posterior variance for that corresponding item.
//'   There are as many rows in this data frame as there are unsanswered questions in the battery.
//'   \code{next.item}: A numeric vector with the index of the item with the highest Fisher's information value.
//'
//'   See \code{fisherInf} for mathematical details.
//'   
//'   The MLWI method:
//'   
//'   This function calculates the likelihood for each value of X at the input value of \eqn{\theta}.
//'   Evaluates the integral over a measure of the plausibility of possible values of \eqn{\theta} by weighting Fisher's information with the likelihood function and selecting the next question according to:
//'  
//'  \deqn{i_j = \underset{j}{\operatorname{argmax}}\int_{-\infty}^{\infty} L(\theta_j)I_i(\theta_j)d\theta_j}{i_j = argmax_j \int_{-\infty}^{\infty} L(\theta_j) I_i(\theta_j) d\theta_j}
//'   
//'   The MPWI method:
//'   
//'   This function calculates the likelihood for each value of X at the input value of \eqn{\theta}.
//'   Evaluates the integral over a measure of the plausibility of possible values of \eqn{\theta} by weighting Fisher's information with the likelihood function and selecting the next question according to:
//'
//'   \deqn{i_j = \underset{j}{\operatorname{argmax}}\int_{-\infty}^{\infty} \pi(\theta) L(\theta_j)I_i(\theta_j)d\theta_j}{i_j = argmax_j \int_{-\infty}^{\infty} \pi(\theta) L(\theta_j) I_i(\theta_j) d\theta_j}
//' 
//'   The MEI method:
//'   
//'   This function estimates the expected observed information for a respondent’s estimated
//'   position on the latent trait on an unanswered item based on the
//'   respondent’s position on the latent trait calculated from answered items.
//'   
//'   The output should be a single numeric value.
//'
//'   Binary details:
//'
//'   \deqn{i_j = \underset{j}{\operatorname{argmax}}\{P(y_{ij} = 1)J_{y_{ij}=1} + P(y_{ij} = 0)J_{y_{ij}=0} \}}{i_j = argmax_j {P(y_ij = 1) J_{y_ij = 1} + P(y_ij = 0) J_(y_ij = 0)}}
//'
//'   Categorical details:
//'   
//'   \deqn{i_j = \underset{j}{\operatorname{argmax}}\{\sum_k^K P(y_{ij} = k)J_{y_{ij}=k} \}}{i_j = argmax_j {\sum_k^K P(y_ij = k) J_{y_ij = k}}}
//'   
//'   The KL method:
//'   
//'   This procedure chooses the next item with the largest KL value.
//'   
//'   See \code{expectedKL}, \code{likelihoodKL}, and/or \code{posteriorKL} for mathematical details.
//'   
//'   The MFII method:
//'   
//'   This approach chooses items based on the Fisher's information in an interval near the current estimate \eqn{\hat{\theta}}.
//'   
//'   \deqn{FII_i = \int^{\hat{\theta}+\delta}_{\hat{\theta}-\delta}I_i(\theta_0) d\theta_0}{FII_i = \int^{\hat{\theta} + \delta}_{\hat{\theta} - \delta} I_i(\theta_0) d\theta_0}
//'
//'   where \deqn{\delta = z(I(\hat{\theta}))^{-1/2}}{\delta = z(I(\hat{\theta}))^{-1/2}},  \eqn{I(\hat{\theta})} is the test information for respondent \eqn{j} evaluated at \eqn{\hat{\theta}},  \deqn{I_i(\cdot)}{I_i(.)} is the Fisher's information for item \eqn{i}, and \eqn{z} is a user specified z-value.
//'   
//'   The random method:
//'   
//'   This routine serves as a baseline for comparison. The routine simply selects an unanswered question at random.
//'
//' @examples
//' 
//'  
//' @seealso \code{\link{estimateTheta}} for calculation of \eqn{\theta};  
//'   \code{\link{obsInf}} for observed information calculation;
//'   \code{\link{fisherTestInfo}} for Fisher's information calculation;
//'   \code{\link{expctedKL}} for expected Kullback-Leibeler calculation;
//'   \code{\link{likelihoodKL}} for likelihood Kullback-Leibeler calculation;  
//'   \code{\link{posteriorKL}} for posterior Kullback-Leibeler calculation; 
//'  
//' @export
// [[Rcpp::export]]
List selectItem(S4 cat_df) {
  return Cat(cat_df).selectItem();
}

//' Expected Kullback-Leibeler information
//'
//' Calculate the expected Kullback-Leibeler information
//' 
//' @return A value indicating the KL information for the desired item, given the current answer profile and ability estimate.
//' 
//' @param cat_df An object of class \code{Cat}
//' @param item An integer indicating the index of the question
//'
//' @details The Kullback-Leibeler information is defined as follows.  Let \eqn{\theta_0} be the true value of the parameter and \eqn{\hat{\theta}} be our current best guess based on the data we have collected so far \deqn{\mathbf{y}_{k-1}}{y_{k-1}}.
//' The KL item information for item \eqn{k} is:
//' 
//' \deqn{K_k(\hat{\theta} ; \theta_0) \equiv E_{\theta_0}\log\Big[\frac{L(\theta_0|\mathbf{y}_{k-1}, y_k)}{L(\hat{\theta}|\mathbf{y}_{k-1})} \Big]}{K_k(\hat{\theta} ; \theta_0) \equiv E_{\theta_0}log[(L(\theta_0| y_{k-1}, y_k)/(L(\hat{\theta}| y_{k-1}))]}
//'
//'  Since \eqn{\theta_0} is unknow, the expected value of the KL information is estimated for each item:
//'
//'  \deqn{K_k(\hat{\theta};\theta_0) = \int^{\hat{\theta}+\delta}_{\hat{\theta}-\delta}\log\Big[\frac{L(\theta_0|\mathbf{y}_{k-1}, y_k)}{L(\hat{\theta}|\mathbf{y}_{k-1})} \Big] d\theta_0}{K_k(\hat{\theta};\theta_0) = \int^{\hat{\theta} + \delta}_{\hat{\theta} - \delta} log[(L(\theta_0|y_{k-1}, y_k)/(L(\hat{\theta}|y_{k-1}))] d\theta_0}
//'
//'  where \deqn{\delta = z(I(\hat{\theta}))^{-1/2}}{\delta = z(I(\hat{\theta}))^{-1/2}},  \eqn{I(\hat{\theta})} is the test information evaluated at \eqn{\hat{\theta}} for respondent \eqn{j}, and \eqn{z} is a user specified z-value.  
//'  
//'  Binary details:
//' 
//'  Due to the conditional independence assumption, we only need to calculate the expected value for potential new items.
//' 
//'
//'  \deqn{\int_{\hat{\theta}-\delta}^{\hat{\theta}+\delta} \Big( P(y_{ij} =
//'   1|\theta_0)\log\Big[\frac{P(y_{ij}=1|\theta_0)}{P(y_{ij}=1|\hat{\theta})} \Big] + P(y_{ij} =
//'   0|\theta_0)\log\Big[\frac{P(y_{ij}=0|\theta_0)}{P(y_{ij}=0|\hat{\theta})} \Big] \Big) d\theta_0}{\int_{\hat{\theta} - \delta}^{\hat{\theta} + \delta} (P(y_ij =
//'   1| \theta_0) log[(P(y_ij = 1| \theta_0))/(P(y_ij = 1| \hat{\theta}))] + P(y_ij =
//'   0| \theta_0) log[(P(y_ij = 0| \theta_0))/(P(y_ij = 0| \hat{\theta}))]) d\theta_0}
//' 
//' \deqn{\int_{\hat{\theta}-\delta}^{\hat{\theta}+\delta} \Big( P(y_{ij} =
//'   1|\theta_0)\big(\log(P(y_{ij}=1|\theta_0)) - \log(P(y_{ij}=1|\hat{\theta}))\big) \Big) \\~~~~~~~ + \Big( P(y_{ij} =
//'   0|\theta_0)\big(\log(P(y_{ij}=0|\theta_0)) - \log(P(y_{ij}=0|\hat{\theta}))\big) \Big) d\theta_0}{\int_{\hat{\theta} - \delta}^{\hat{\theta} + \delta} (P(y_ij =
//'   1| \theta_0) (log(P(y_ij = 1| \theta_0)) - log(P(y_ij = 1| \hat{\theta})))) + (P(y_ij =
//'   0| \theta_0) (log(P(y_ij = 0| \theta_0)) - log(P(y_ij = 0| \hat{\theta})))) d\theta_0}
//'
//'  Categorical details:
//'
//'  \deqn{\int_{\hat{\theta}-\delta}^{\hat{\theta}+\delta} \sum_k\Big(
//'     P(y_{ij}=k|\theta_0)\big(\log(P(y_{ij}=k|\theta_0))- \log(P(y_{ij}=k|\hat{\theta})) \big)\Big) d\theta_0}{\int_{\hat{\theta} - \delta}^{\hat{\theta} + \delta} 
//'     \sum_k(P(y_ij = k| \theta_0) (log(P(y_ij = k| \theta_0)) - log(P(y_ij = k| \hat{\theta})))) d\theta_0}
//'
//' @examples
//' 
//'
//' @seealso \code{\link{estimateTheta}} for calculation of \eqn{\theta};
//'   \code{\link{likelihoodKL}} and/or \code{\link{posteriorKL}} for alternative KL methods
//'
//' @export
// [[Rcpp::export]]
double expectedKL(S4 cat_df, int item) {
  item = item - 1;
  return Cat(cat_df).expectedKL(item);
}

//' Expected Kullback-Leibeler information, weighted by the likelihood
//'
//' Calculate the expected Kullback-Leibeler information, weighted by the likelihood
//' 
//' @return A value indicating the LKL information for the desired item, given the current answer profile and ability estimate.
//' 
//' @param cat_df An object of class \code{Cat}
//' @param item An integer indicating the index of the question
//'
//' @details The LKL calculation follows the same procedure as \code{expectedKL}, except it requires weighting the different potential values of \eqn{\theta_0} by the likelihood.
//'  Thus, the equation is
//'
//'  \deqn{KL_k(\hat{\theta};\theta_0) = \int L(\theta_0|\mathbf{y}_{k-1}) \log\Big[\frac{L(\theta_0|\mathbf{y}_{k-1}, y_k)}{L(\hat{\theta}|\mathbf{y}_{k-1})} \Big] d\theta_0}{KL_k(\hat{\theta};\theta_0) = \int L(\theta_0| \mathbf{y}_{k-1}) log[(L(\theta_0| \mathbf{y}_{k-1}, y_k))/(L(\hat{\theta}| \mathbf{y}_{k-1}))] d\theta_0}
//'
//'  Binary details:
//'  
//'  Due to the conditional independence assumption, we only need to calculate the expected value for potential new items.
//'
//' \deqn{\int L(\theta_0|\mathbf{y}_{k-1})\Big[ \Big( P(y_{ij} =
//'  1|\theta_0)\big(\log(P(y_{ij}=1|\theta_0)) - \log(P(y_{ij}=1|\hat{\theta}))\big) \Big) \\~~~~~~~ + \Big( P(y_{ij} =
//'  0|\theta_0)\big(\log(P(y_{ij}=0|\theta_0)) - \log(P(y_{ij}=0|\hat{\theta}))\big) \Big) \Big]d\theta_0}{\int L(\theta_0| \mathbf{y}_{k-1})[(P(y_ij =
//'  1| \theta_0)(log(P(y_ij = 1| \theta_0)) - log(P(y_ij = 1| \hat{\theta})))) + (P(y_ij =
//'  0| \theta_0)(log(P(y_ij = 0| \theta_0)) - log(P(y_ij = 0| \hat{\theta}))))] d\theta_0}
//'
//'  Categorical details:
//'
//'   \deqn{\int \sum_k\Big(L(\theta_0|\mathbf{y}_{k-1})P(y_{ij}=k|\theta_0)\big(\log(P(y_{ij}=k|\theta_0))- \log(P(y_{ij}=k|\hat{\theta})) \big)\Big) d\theta_0}{\int \sum_k(L(\theta_0| \mathbf{y}_{k-1})P(y_ij = k| \theta_0) (log(P(y_ij = k| \theta_0)) - log(P(y_ij = k| \hat{\theta})))) d\theta_0}
//'
//' @examples
//' 
//'  
//' @seealso \code{\link{estimateTheta}} for calculation of \eqn{\theta};
//'   \code{\link{expectedKL}} and/or \code{\link{posteriorKL}} for alternative KL methods 
//'  
//' @export
// [[Rcpp::export]]
double likelihoodKL(S4 cat_df, int item) {
  item = item - 1;
  return Cat(cat_df).likelihoodKL(item);
}

//' Expected Kullback-Leibeler information, weighted by the posterior
//'
//' Calculate the expected Kullback-Leibeler information, weighted by the posterior
//' 
//' @return A value indicating the posterior KL information for the desired item, given the current answer profile and ability estimate.
//' 
//' @param cat_df An object of class \code{Cat}
//' @param item An integer indicating the index of the question
//'
//' @details We will follow the same procedure as \code{expectedKL}, except we will weight the different potential values of \eqn{\theta_0} by the posterior.
//'
//'   \deqn{KL_k(\hat{\theta};\theta_0) = \int \pi(\theta_0) L(\theta_0|\mathbf{y}_{k-1}) \log\Big[\frac{L(\theta_0|\mathbf{y}_{k-1}, y_k)}{L(\hat{\theta}|\mathbf{y}_{k-1})} \Big] d\theta_0}{KL_k(\hat{\theta};\theta_0) = \int \pi(\theta_0) L(\theta_0| \mathbf{y}_{k-1}) log[(L(\theta_0| {y}_{k-1}, y_k)/(L(\hat{\theta}| \mathbf{y}_{k-1}))] d\theta_0}
//'   
//'   Binary details:
//'
//'   Due to the conditional independence assumption, we only need to calculate the expected value for potential new items.
//'
//'   \deqn{\int \pi(\theta_0) L(\theta_0|\mathbf{y}_{k-1})\Big[ \Big( P(y_{ij} =
//'   1|\theta_0)\big(\log(P(y_{ij}=1|\theta_0)) - \log(P(y_{ij}=1|\hat{\theta}))\big) \Big) \\~~~~~~~ + \Big( P(y_{ij} =
//'   0|\theta_0)\big(\log(P(y_{ij}=0|\theta_0)) - \log(P(y_{ij}=0|\hat{\theta}))\big) \Big) \Big]d\theta_0}{\int \pi(\theta_0) L(\theta_0| \mathbf{y}_{k-1}) [(P(y_ij =
//'   1| \theta_0) (log(P(y_ij = 1| \theta_0)) - log(P(y_ij = 1| \hat{\theta})))) + (P(y_ij = 0| \theta_0) (log(P(y_ij = 0| \theta_0)) - log(P(y_ij = 0| \hat{\theta}))))] d\theta_0}
//'
//'   Categorical details:
//'
//'   \deqn{\int \sum_k\Big(\pi(\theta_0) L(\theta_0|\mathbf{y}_{k-1})P(y_{ij}=k|\theta_0)\big(\log(P(y_{ij}=k|\theta_0))- \log(P(y_{ij}=k|\hat{\theta})) \big)\Big) d\theta_0}{\int \sum_k(\pi(\theta_0) L(\theta_0| \mathbf{y}_{k-1})P(y_ij = k| \theta_0) (log(P(y_ij = k| \theta_0)) - log(P(y_ij = k| \hat{\theta})))) d\theta_0}
//' 
//' @examples
//' 
//' 
//' @seealso \code{\link{estimateTheta}} for calculation of \eqn{\theta};
//'   \code{\link{likelihoodKL}} and/or \code{\link{expectedKL}} for alternative KL methods
//' 
//' @export
// [[Rcpp::export]]
double posteriorKL(S4 cat_df, int item) {
  item = item - 1;
  return Cat(cat_df).posteriorKL(item);
}

//' Look Ahead to Select Next Item
//'
//' This function returns the next item that should be asked for all possible response options of the question the respondent is currently answering.
//'
//' @param cat_df  An object of class \code{Cat}
//' @param item A numeric indicating the item the respondent is currently answering.
//'
//' @return A vector of values indicating the possible subsequent questions
//' 
//' @examples
//' 
//'  
//'
//' @seealso \code{\link{selectItem}} for selection method information
//'
//' @export
// [[Rcpp::export]]
List lookAhead(S4 cat_df, int item) {
  item = item - 1.0;
  return Cat(cat_df).lookAhead(item);
}

//' Check if Stop and/or Override Rules are Met
//'
//' This function returns a boolean indicating if the respondent should not be asked futher questions after evaluating the specified stopping and/or override rules
//'
//' @param cat_df  An object of class \code{Cat}
//'
//' @return A boolean, where TRUE indicates the the stopping rules are met and FALSE indicates the stoppings rules are not met
//'
//' @details The stopping rule thresholds are stored in the following Cat object slots: lengthThreshold, seThreshold, infoThreshold, and gainThreshold.
//'   The override thresholds are stored in the following Cat object slots: lengthOverride, gainOverride. A value of NA indicates the rule should not be used.
//' 
//'   A return value of TRUE indicates that additional questions should be asked; FALSE indicates no additional questions should be asked.
//' 
//'   A user can specify any combination of stopping rules and/or overrides.  The function returns TRUE if all specified stopping rules are met
//'   and no specified overrides are met. The function returns FALSE if at least one specified stopping rule is not met, or if any specified override threshold is met.
//' 
//'   Stopping Rules:
//'
//'   lengthThreshold: Number of question's answered >= a
//'  
//'   seThreshold: \eqn{SE(\hat{\theta}) < a}
//'  
//'   infoThreshold: \deqn{FI < a \forall}{FI < a \forall} remaining items
//'
//'   gainThreshold: \deqn{SE(\hat{\theta}) - \sqrt{EPV} | < a \forall}{SE(\hat{\theta}) - \sqrt{EPV} | < a \forall} remaining items
//'
//'   Overrides:
//'
//'   lengthOverride: Number of question's answered < a
//'
//'   gainOverride: \deqn{|SE(\hat{\theta}) - \sqrt{EPV} | >= a \forall}{|SE(\hat{\theta}) - \sqrt{EPV} | >= a \forall} remaining items
//' 
//' 
//' @export
// [[Rcpp::export]]
bool checkStopRules(S4 cat_df) {
	std::vector<bool> answer = Cat(cat_df).checkStopRules();
  return answer[0];
}




