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

	if(questionSet.model == "grm")
	{
		for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
			int item = questionSet.nonapplicable_rows.at(i);
			selection.values.push_back(estimator.expectedObsInf_grm(item, prior));
		}
	}
	else if(questionSet.model == "gpcm")
	{
		for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
			int item = questionSet.nonapplicable_rows.at(i);
			selection.values.push_back(estimator.expectedObsInf_gpcm(item, prior));
		}

	}
	else
	{
		for (size_t i = 0; i < questionSet.nonapplicable_rows.size(); ++i) {
			int item = questionSet.nonapplicable_rows.at(i);
			selection.values.push_back(estimator.expectedObsInf_rest(item, prior));
		}

	}
	

	auto max_itr = std::max_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions.at(std::distance(selection.values.begin(),max_itr));

	selection.question_names.resize(selection.questions.size());

	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	return selection;
}
