#include "MLWISelector.h"
#include "ParallelUtil.h"

struct MLWI : public mpl::FunctionCaller<double>
{
	using Base = mpl::FunctionCaller<double>;

	MLWI(Estimator& e, double& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.lwi(question);
	}
};

SelectionType MLWISelector::getSelectionType() {
	return SelectionType::MLWI;
}

Selection MLWISelector::selectItem() {
	Selection selection;
	selection.name = "MLWI";
	selection.questions = questionSet.nonapplicable_rows;
	
	double dummy = 0;

	selection.values.resize(selection.questions.size());

	mpl::ParallelHelper<MLWI> helper(selection.questions, selection.values, estimator, dummy);
   	// call parallelFor to do the work
  	RcppParallel::parallelFor(0, selection.questions.size(), helper);

	auto max_itr = std::max_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions.at(std::distance(selection.values.begin(),max_itr));

	selection.question_names.resize(selection.questions.size());

	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	return selection;
}

MLWISelector::MLWISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }