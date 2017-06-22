#include "EPVSelector.h"

using namespace std;

Selection EPVSelector::selectItem() {
  
	// For every unanswered item, calculate the epv of that item
	Selection selection = Selection();
	selection.name = getSelectionName();
	selection.questions = questionSet.nonapplicable_rows;

	// wrapper to get epv for passing to transform
	auto epv = [&](int row){return this->estimator.expectedPV(row, prior);};

	selection.values.resize(selection.questions.size());
	std::transform(selection.questions.begin(),selection.questions.end(),selection.values.begin(), epv);

	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};

	selection.question_names.resize(selection.questions.size());
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	auto min_itr = std::min_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions[std::distance(selection.values.begin(),min_itr)];

	return selection;
}

SelectionType  EPVSelector::getSelectionType() {
	return SelectionType::EPV;
}

EPVSelector::EPVSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                      priorModel) { }

std::string EPVSelector::getSelectionName() {
	return "EPV";
}
