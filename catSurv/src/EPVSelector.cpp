#include "EPVSelector.h"

#include <iostream>
 
using namespace std;

Selection EPVSelector::selectItem() {
  
	// For every unanswered item, calculate the epv of that item
	Selection selection = Selection();
	selection.name = getSelectionName();
	selection.values.reserve(questionSet.nonapplicable_rows.size());
	selection.questions = questionSet.nonapplicable_rows;
	
	// Adding 1 to each row index so it prints the correct 
	// question number for user
	transform(selection.questions.begin(), selection.questions.end(),
           selection.questions.begin(), bind2nd(std::plus<int>(), 1.0));
	
	int min_item = -1;
	double min_epv = INFINITY;

	for (int row : questionSet.nonapplicable_rows) {
		double epv = estimator.expectedPV(row, prior);
		selection.values.push_back(epv);

		if (epv < min_epv) {
			min_item =  row;
			min_epv = epv;
		}
	}
	selection.item = min_item + 1; // +1 so that it prints the right number for user
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
