#include "PKLSelector.h"


SelectionType PKLSelector::getSelectionType() {
	return SelectionType::PKL;
}

Selection PKLSelector::selectItem() {
	Selection selection;
	selection.name = "PKL";
	selection.questions = questionSet.nonapplicable_rows;
	selection.values.reserve(questionSet.nonapplicable_rows.size());
	
	double max_pkl = 0.0;
	int max_item = -1;
	
	for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
	  int question = questionSet.nonapplicable_rows.at(i);
	  selection.values.push_back(estimator.posteriorKL(question, prior));

		if (selection.values[i] > max_pkl) {
			max_item = question;
			max_pkl = selection.values[i];
		}
	}
	
	selection.item = max_item;
	selection.item = selection.item;
	return selection;
}

PKLSelector::PKLSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }