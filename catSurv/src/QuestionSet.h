#pragma once
#include <Rcpp.h>
#include <vector>

/**
 * Contains the various lists of values necessary for a Cat.
 */
struct QuestionSet {
  std::vector<std::string> question_names;
	std::vector<std::vector<double> > difficulty;
	/**
	 * The questions which have not yet been answered;
	 */
	std::vector<int> applicable_rows;
	/**
	 * The questions which have been answered.
	 */
	std::vector<int> nonapplicable_rows;
	std::vector<double> guessing;
	std::vector<double> discrimination;
	std::vector<double> z;
	std::vector<int> strata;
	
	/**
	 * The user's answer to each question.
	 */
	std::vector<int> answers;
	std::vector<bool> poly;
	/**
	 * Keeping track of extreme answers for MLEEstimator.
	 */	
	bool all_extreme;
	
	double lowerBound;
	double upperBound;

	QuestionSet(Rcpp::S4 &cat_df);
};