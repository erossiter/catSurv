#include "MEISelector.h"

MEISelector::MEISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions,
                                                                                                      estimation,
                                                                                                      priorModel) { }

SelectionType MEISelector::getSelectionType() {
	return SelectionType::MEI;
}

Selection MEISelector::selectItem() {
	Selection selection;
	selection.questions = questionSet.nonapplicable_rows;
	selection.values.reserve(questionSet.nonapplicable_rows.size());
	selection.name = "MEI";
	selection.question_names.reserve(questionSet.nonapplicable_rows.size());

	double max_EI = 0.0;
	int max_item = -1;

	for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
		int item = questionSet.nonapplicable_rows.at(i);
	  selection.question_names.push_back(questionSet.question_names.at(item));

		double this_EI = estimator.expectedObsInf(item, prior);
		if (this_EI > max_EI) {
			max_EI = this_EI;
			max_item = item;
		}
		selection.values.push_back(this_EI);
	}

	selection.item = max_item;

	return selection;
}
