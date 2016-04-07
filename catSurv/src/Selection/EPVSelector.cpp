#include <Rcpp>
#include "EPVSelector.h"
#include "../probability.h"

Rcpp::List virtual EPVSelector::nextItem(QuestionSet questionSet) {
	// For every unanswered item, calculate the epv of that item
	std::vector<double> epvs;
	int min_item = -1;
	double min_epv = std::numeric_limits<double>::max();

	for (int row : questionSet.nonapplicable_rows) {
		double epv = expectedPV(questionSet, row - 1);
		epvs.push_back(epv);

		if (epv < min_epv) {
			min_item = row;
			min_epv = epv;
		}
	}
	DataFrame all_estimates = Rcpp::DataFrame::create(Named("questions") = questionSet.nonapplicable_rows,
	                                                  Named("EPV") = epvs);
	return Rcpp::List::create(Named("all.estimates") = all_estimates,
	                          Named("next.item") = wrap(min_item));
}

double EPVSelector::expectedPV(QuestionSet questionSet, int item) {
	double sum = 0.0;

	questionSet.applicable_rows.push_back(item); // add item to set of answered items
	if (questionSet.poly) {
		std::vector<double> variances;
		for (unsigned int i = 0; i < questionSet.difficulty[item].size() + 1; ++i) {
			questionSet.answers[item] = i + 1;
			variances.push_back(estimateSE(questionSet));
			variances[i] *= variances[i];
		}
		questionSet.answers[item] = NA_INTEGER;
		questionSet.applicable_rows.pop_back();
		std::vector<double> question_cdf;
		question_cdf.push_back(1.0);
		probability(questionSet, estimateTheta(questionSet), item, question_cdf);
		question_cdf.push_back(0.0);
		for (unsigned int i = 0; i < question_cdf.size() - 1; ++i) {
			sum += variances[i] * (question_cdf[i] - question_cdf[i + 1]);
		}
	} else {

		questionSet.answers[item] = 0;
		double variance_zero = estimateSE(questionSet);
		variance_zero *= variance_zero;
		questionSet.answers[item] = 1;
		double variance_one = estimateSE(questionSet);
		variance_one *= variance_one;
		questionSet.applicable_rows.pop_back();
		questionSet.answers[item] = NA_INTEGER; // remove answer
		double prob_zero = probability(questionSet, estimateTheta(questionSet), item);
		double prob_one = 1.0 - prob_zero;
		sum = (prob_zero * variance_zero) + (prob_one * variance_one);
	}
	return sum;
}