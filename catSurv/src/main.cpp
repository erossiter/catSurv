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

//' Probabilities of the responses to a question given theta
//'
//' This function calculates the probabilities of a specific set of responses to a specific question for a specific value of \eqn{\theta}.
//'
//' @param cat_df An object of \code{Cat} class
//' @param t A double indicating the potential value for \eqn{\theta_j}
//' @param q An integer indicating the index of the question
//' @param ret_prob (For polytonomous implementation only) A double-vector where the calculations carried out by this function will be stored.
//'
//' @return A vector consisting of the probability of a correct response for each respondent on item \eqn{i}.
//'
//' @details The probability of a correct response for respondent \eqn{j} on item \eqn{i} is ....
//' where \eqn{\theta_j} is respondent \eqn{j}'s position on the latent scale of interest, \eqn{a_i} is item \eqn{i}'s discrimination parameter,
//'  \eqn{b_i} is item i's difficulty parameter, and \eqn{c_i} is item \eqn{i}'s guessing parameter.
//'
//'  Note: this function is overloaded, due to different output types of binary vs polytomous implementations (outputs single value for binary implementation,
//'  vector of values for polytomous implementation)
//'
//'  Note: the function for polytomous implementation does not return values, but rather alters the object ret_prob in memory
//'
//' @export
// [[Rcpp::export]]
List probability(S4 cat_df, NumericVector t, IntegerVector q) {
	Cat cat = Cat(cat_df);
	double theta = t[0];
	int question = q[0];
	DataFrame question_probs = DataFrame::create(Named("probabilities") = cat.probability(theta, question));
	return List::create(Named("all.probabilities") = question_probs);
}


//' @export
// [[Rcpp::export]]
void showCppCat(S4 cat_df) {
	return Cat(cat_df).showCppCat();
}

//' Likelihood of offering specific response
//'
//' This function returns the value of likelihood of a person with ability parameter \eqn{\theta} having offered the specific response profile stored in answers conditional on the item-level parameters.
//'
//' @param cat_df An object of \code{Cat} class
//' @param t A numeric for possible value of theta (position on the latent scale of interest)
//'
//' @return A value of the likelihood of each respondent having offered the provided response profile
//'
//' @details Letting \eqn{q_i(\theta_j)=1-p_i(\theta_j)}, the likelihood function associated with the responses profile \eqn{y_j} is..
//' \eqn{L(\theta_j|\mathbf{y}_{j})=\prod^{j}_{i=1}p_i(\theta_j)^{y_{ij}}q_i(\theta_j)^{(1-y_{ij}}}, where \eqn{y_j} is evaluated based only on the questions the respondent has actually had the opportunity to answer
//'
//' @export
// [[Rcpp::export]]
double likelihood(S4 cat_df, double t) {
	return Cat(cat_df).likelihood(t);
}

//' Estimate of the ability parameter
//'
//' This function takes a Cat object and returns the expected value of the ability parameter conditioned on the observed answers and the item calibrations.
//'
//' @param cat An object of \code{Cat} class
//'
//' @return A vector consisting of the expected value of the ability parameter
//'
//' @details The expected value of \eqn{\theta_j} is:
//' \eqn{\frac{\int_{-\infty}^\infty \theta_j \eqn{L}(\theta_j) \pi(\theta_j) d\theta_j}{\int_{-\infty}^\infty \eqn{L}(\theta_j) \pi(\theta_j) d\theta_j}},
//' where \eqn{L}(\theta_j) is the likelihood function and \pi(\theta_j) is the prior distribution for \eqn{\theta_j}
//'
//'
//'  Note: When MLE would not work, estimateTheta is calculated according to the estimationDefault slot in Cat object.
//'
//'  Note: This method will only be available using the normal prior distribution
//'
//' @export
// [[Rcpp::export]]
double estimateTheta(S4 cat_df) {
	return Cat(cat_df).estimateTheta();
}

//' Estimate of standard error for the posterior estimate
//'
//' This function estimates the standard error for the posterior estimate for the position \eqn{\theta_j} of a person on the latent scale
//'
//' @param cat An object of \code{Cat} class
//'
//' @return The posterior standard error for \eqn{\theta_j}
//'
//' @details The posterior variance for \eqn{\theta_j} is:
//' \eqn{\hat{\theta}_j = E[(\theta_j-\hat{\theta_j})^2]=\frac{\int(\theta_j-\hat{\theta_j})^2\pi(\theta_j)L(\theta_j)d\theta_j}{}\int\pi(\theta_j)L(\theta_j)d\theta_j},
//' where \eqn{\hat{\theta}_j} is the chosen point estimate for the respondent on the latent scale. The standard error is then \sqrt{\mbox{Var}(\hat{\theta}_j)}
//'
//'
//' @export
// [[Rcpp::export]]
double estimateSE(S4 cat_df) {
	return Cat(cat_df).estimateSE();
}


//' The possible prior distribution functions
//'
//' This function returns the prior value for each respondent's position on the latent scale of interest
//'
//' @param x A numeric value where we want to evaluate the prior name
//'
//' @return A vector consisting of prior value, \eqn{\pi(x)}, given the value \eqn{x}
//'
//' @details \eqn{x} needs to be either NORMAL or STUDENT_T params, which are parameters controlling the shape of the prior
//'
//' @export
// [[Rcpp::export]]
double prior(NumericVector x, CharacterVector c, NumericVector p) {
	std::string name = Rcpp::as<std::string>(c);
	std::vector<double> params = Rcpp::as<std::vector<double> >(p);
	return Prior(name, params).prior(x[0]);
}


//' @export
// [[Rcpp::export]]
double dLL(S4 &cat_df, double theta, bool use_prior){
	return Cat(cat_df).dLL(theta, use_prior);
}

//' @export
// [[Rcpp::export]]
double d2LL(S4 &cat_df, double theta, bool use_prior){
	return Cat(cat_df).d2LL(theta, use_prior);
}

// //' @export
// // [[Rcpp::export]]
// List selectItem(S4 cat_df, int strata_choice = 1) {
//   return Cat(cat_df).selectItem(strata_choice);
// }

//' @export
// [[Rcpp::export]]
List selectItem(S4 cat_df) {
  return Cat(cat_df).selectItem();
}

//' @export
// [[Rcpp::export]]
List lookAhead(S4 cat_df, int item) {
  item = item - 1.0;
  return Cat(cat_df).lookAhead(item);
}


//' @export
// [[Rcpp::export]]
double expectedPV(S4 cat_df, int item) {
  item = item - 1.0;
	return Cat(cat_df).expectedPV(item);
}

//' @export
// [[Rcpp::export]]
double obsInf(S4 cat_df, double theta, int item) {
  item = item - 1;
	return Cat(cat_df).obsInf(theta, item);
}

//' @export
// [[Rcpp::export]]
double fisherInf(S4 cat_df, double theta, int item) {
  item = item - 1;
	return Cat(cat_df).fisherInf(theta, item);
}

//' @export
// [[Rcpp::export]]
double expectedObsInf(S4 cat_df, int item) {
  item = item - 1;
	return Cat(cat_df).expectedObsInf(item);
}

//' @export
// [[Rcpp::export]]
double expectedKL(S4 cat_df, int item) {
  item = item - 1;
	return Cat(cat_df).expectedKL(item);
}

//' @export
// [[Rcpp::export]]
double likelihoodKL(S4 cat_df, int item) {
  item = item - 1;
	return Cat(cat_df).likelihoodKL(item);
}

//' @export
// [[Rcpp::export]]
double posteriorKL(S4 cat_df, int item) {
  item = item - 1;
	return Cat(cat_df).posteriorKL(item);
}


//' @export
// [[Rcpp::export]]
double fisherTestInfo(S4 cat_df) {
	return Cat(cat_df).fisherTestInfo();
}


//' @export
// [[Rcpp::export]]
double findRoot(S4 cat_df) {
	return Cat(cat_df).findRoot();
}

//' @export
// [[Rcpp::export]]
bool checkStopRules(S4 cat_df) {
	std::vector<bool> answer = Cat(cat_df).checkStopRules();
  return answer[0];
}





