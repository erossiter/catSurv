#include <Rcpp.h>
#include "Cat.h"
using namespace Rcpp;


double probability(question_data question, double difficulty, double theta) {
	double exp_prob = exp(question.D * question.discrimination * (theta - difficulty));
	return question.guessing + (1 - question.guessing) * (exp_prob) / (1 + exp_prob);
}

std::vector<double> probability(Cat &cat, double theta, int question_number) {
	std::vector<double> probabilities;
	auto question = cat.get_question(question_number);
	
	for (auto term : question.difficulty) {
		probabilities.push_back(probability(question, term, theta));
	}
	return probabilities;
}




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

	std::vector<double> probs = probability(cat, theta, question);

	DataFrame question_probs = DataFrame::create(Named("probabilities") = probs);
	return List::create(Named("all.probabilities") = question_probs);
}