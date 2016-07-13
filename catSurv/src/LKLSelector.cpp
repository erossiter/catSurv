#include "LKLSelector.h"


SelectionType LKLSelector::getSelectionType() {
	return SelectionType::LKL;
}

Selection LKLSelector::selectItem() {
	Selection selection;
	selection.name = "LKL";
	selection.questions = questionSet.nonapplicable_rows;
	selection.values.reserve(questionSet.nonapplicable_rows.size());
	selection.question_names.reserve(questionSet.nonapplicable_rows.size());
	
	double max_lkl = 0.0;
	int max_item = -1;
	
	for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
	  int question = questionSet.nonapplicable_rows.at(i);
	  selection.values.push_back(estimator.likelihoodKL(question, prior));
	  selection.question_names.push_back(questionSet.question_names[question]);

		if (selection.values[i] > max_lkl) {
			max_item = question;
			max_lkl = selection.values[i];
		}
	}
	
	selection.item = max_item;
	selection.item = selection.item;
	return selection;
}

LKLSelector::LKLSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }