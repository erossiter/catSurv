#include <Rcpp.h>
#include "Cat.h"
using namespace Rcpp;


//' Find the weighted mean of several normal distributions
//' 
//' @param x A vector of values where the weighted normal is being evaluated
//' @param means The means of the vaious normal distributions
//' @param weights The weight assigned to each normal distribution
//' @param sd A single number for the common standard deviation
//' @export
// [[Rcpp::export]]
List probability(S4 cat_df, NumericVector t, IntegerVector q) {
	Cat cat(cat_df);

	double theta = t[0];
	int question = q[0];

	std::vector<double> probs = cat.probability(theta, question);

	DataFrame question_probs = DataFrame::create(Named("probabilities") = probs);
	return List::create(Named("all.probabilities") = question_probs);
}