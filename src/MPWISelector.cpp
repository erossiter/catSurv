#include "MPWISelector.h"


SelectionType MPWISelector::getSelectionType() {
	return SelectionType::MPWI;
}

Selection MPWISelector::selectItem() {
	Selection selection;
	selection.name = "MPWI";
	selection.questions = questionSet.nonapplicable_rows;
	selection.values.reserve(questionSet.nonapplicable_rows.size());
	selection.question_names.reserve(questionSet.nonapplicable_rows.size());
	
	double max_pwi = 0.0;
	int max_item = -1;
	
	for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
	  int question = questionSet.nonapplicable_rows.at(i);
	  selection.question_names.push_back(questionSet.question_names.at(question));
	  selection.values.push_back(estimator.pwi(question, prior));

		if (selection.values.at(i) > max_pwi) {
			max_item = question;
			max_pwi = selection.values.at(i);
		}
	}
	
	selection.item = max_item;
	selection.item = selection.item;
	return selection;
}

MPWISelector::MPWISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }