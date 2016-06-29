#include "MWPISelector.h"


SelectionType MWPISelector::getSelectionType() {
	return SelectionType::MWPI;
}

Selection MWPISelector::selectItem() {
	Selection selection;
	selection.name = "MWPI";
	selection.questions = questionSet.nonapplicable_rows;
	selection.values.reserve(questionSet.nonapplicable_rows.size());
//
//	double max_pwi = 0.0;
//	int max_item = -1;
//
//
//	for(int item : questionSet.nonapplicable_rows){
//		auto prior_like_fi = [&](size_t j) {
//			double likelihood = estimator.likelihood(X, questionSet.applicable_rows);
//			double prior_value = prior.prior(questionSet.X[j]);
//			return prior_value * likelihood * estimator.fisherInf(item - 1, questionSet.X[j]);
//		};
//
//		double current_pwi = 0; // integrate prior_like_fi
//
//		if (current_pwi > max_pwi){
//			max_pwi = current_pwi;
//			max_item = item;
//		}
//		selection.values.push_back(current_pwi);
//	}
	return selection;
}

MWPISelector::MWPISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }