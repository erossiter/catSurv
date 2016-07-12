#include "RANDOMSelector.h"
#include <stdlib.h> 

SelectionType RANDOMSelector::getSelectionType() {
	return SelectionType::RANDOM;
}

Selection RANDOMSelector::selectItem() {
	Selection selection;
	selection.name = "RANDOM";
	selection.questions = questionSet.nonapplicable_rows;
	selection.values.reserve(questionSet.nonapplicable_rows.size());
	
	for (int item : questionSet.nonapplicable_rows) {
	  item = 0;
		selection.values.push_back(item);
	}
	
	int random_index = rand() % selection.questions.size();
	
	selection.item = selection.questions[random_index];
		std::cout << random_index << std::endl;
	return selection;
}

RANDOMSelector::RANDOMSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }