#pragma once
#include <Rcpp.h>
#include <vector>

struct QuestionSet {
	std::vector<std::vector<double> > difficulty;
	std::vector<int> applicable_rows;
	std::vector<int> nonapplicable_rows;
	std::vector<double> guessing;
	std::vector<double> discrimination;
	std::vector<int> answers;
	bool poly;

	QuestionSet(Rcpp::S4 &cat_df);
};