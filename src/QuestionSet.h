#pragma once
#include <Rcpp.h>
#include <vector>

/**
 * Contains the various lists of values necessary for a Cat.
 */
struct QuestionSet {
  std::vector<std::string> question_names;
	std::vector<std::vector<double> > difficulty;

	std::vector<int> applicable_rows;
	std::vector<int> nonapplicable_rows;
	std::vector<int> skipped;
	
	std::vector<double> guessing;
	std::vector<double> discrimination;
	std::vector<double> z;
	
	/**
	 * The user's answer to each question.
	 */
	std::vector<int> answers;
	std::string model;
	/**
	 * Keeping track of extreme answers for MLEEstimator.
	 */	
	bool all_extreme;
	/**
	 * Bounds for integration.
	 */		
	double lowerBound;
	double upperBound;

	QuestionSet(Rcpp::S4 &cat_df);

	void reset_answers(Rcpp::DataFrame& responses, size_t row);
	void reset_answer(size_t question, int answer);
	void reset_answers(std::vector<int> const& source);
private:
	void reset_all_extreme();
	void reset_applicables();
};