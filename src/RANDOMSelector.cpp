#include <RcppArmadilloExtensions/sample.h>
#include "RANDOMSelector.h"

SelectionType RANDOMSelector::getSelectionType() {
	return SelectionType::RANDOM;
}

Selection RANDOMSelector::selectItem() {
	Selection selection;
	selection.name = "RANDOM";
	selection.questions = questionSet.nonapplicable_rows;
	selection.values.reserve(questionSet.nonapplicable_rows.size());
	selection.question_names.reserve(questionSet.nonapplicable_rows.size());
	
	for (int item : questionSet.nonapplicable_rows) {
	  selection.question_names.push_back(questionSet.question_names.at(item));
	  item = 0;
		selection.values.push_back(item);
	}

	std::vector<int> sample_vec = Rcpp::RcppArmadillo::sample(selection.questions, 1, false, NULL);
	selection.item = sample_vec.at(0);

	return selection;
}

RANDOMSelector::RANDOMSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }