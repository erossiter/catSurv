//
// Created by Alex Weil on 4/26/16.
//

#include "MFISelector.h"

SelectionType MFISelector::getSelectionType() {
	return SelectionType::MFI;
}

Selection MFISelector::nextItem() {
	Selection selection;
	selection.questions = questionSet.nonapplicable_rows;
	selection.values.reserve(questionSet.nonapplicable_rows.size());
	selection.name = "MFI";

	double max_mfi = 0.0;
	int max_item = -1;

	double theta = estimator.estimateTheta(prior);
	for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
		int question = questionSet.nonapplicable_rows.at(i);
		selection.values.push_back(estimator.fisherInf(theta, question));

		if (selection.values[i] > max_mfi) {
			max_item = question;
			max_mfi = selection.values[i];
		}
	}

	selection.item = max_item;
	return selection;

}

MFISelector::MFISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions,
                                                                                                      estimation,
                                                                                                      priorModel) { }
