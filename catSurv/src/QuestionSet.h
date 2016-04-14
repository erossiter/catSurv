#pragma once
#include <vector>

class QuestionSet {
public:
	std::vector<std::vector<double> > difficulty;
	std::vector<int> applicable_rows;
	std::vector<int> nonapplicable_rows;
	std::vector<double> guessing;
	std::vector<double> discrimination;
	std::vector<int> answers;
	std::vector<double> X;
	bool poly;
};