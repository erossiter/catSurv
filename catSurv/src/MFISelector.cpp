#include "MFISelector.h"

using namespace std;

SelectionType MFISelector::getSelectionType() {
	return SelectionType::MFI;
}

Selection MFISelector::selectItem() {
	Selection selection;
	selection.questions = questionSet.nonapplicable_rows;
	selection.values.reserve(questionSet.nonapplicable_rows.size());
	selection.name = "MFI";
	selection.question_names.reserve(questionSet.nonapplicable_rows.size());

	double max_mfi = 0.0;
	int max_item = -1;

	double theta = estimator.estimateTheta(prior);
	for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
		int question = questionSet.nonapplicable_rows.at(i);
	  selection.question_names.push_back(questionSet.question_names.at(question));
		selection.values.push_back(estimator.fisherInf(theta, question));

		if (selection.values.at(i) > max_mfi) {
			max_item = question;
			max_mfi = selection.values.at(i);
		}
	}

	selection.item = max_item;
	selection.item = selection.item;
	return selection;

}

MFISelector::MFISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions,
                                                                                                      estimation,
                                                                                                      priorModel) { }
