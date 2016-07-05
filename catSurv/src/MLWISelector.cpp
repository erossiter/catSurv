#include "MLWISelector.h"


SelectionType MLWISelector::getSelectionType() {
	return SelectionType::MLWI;
}

Selection MLWISelector::selectItem() {
	Selection selection;
	selection.name = "MLWI";
	selection.questions = questionSet.nonapplicable_rows;
	selection.values.reserve(questionSet.nonapplicable_rows.size());
	
	double max_lwi = 0.0;
	int max_item = -1;
	
	for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
	  int question = questionSet.nonapplicable_rows.at(i);
	  selection.values.push_back(estimator.lwi(question));

		//double current_lwi = 0;

		if (selection.values[i] > max_lwi) {
			max_item = question;
			max_lwi = selection.values[i];
		}
	}
	
	selection.item = max_item;
	selection.item = selection.item;
	return selection;
}

MLWISelector::MLWISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }