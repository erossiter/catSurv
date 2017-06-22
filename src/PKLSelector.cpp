#include "PKLSelector.h"
#include "ParallelUtil.h"

struct PKL : public mpl::FunctionCaller<Prior>
{
	using Base = mpl::FunctionCaller<Prior>;

	PKL(Estimator& e, Prior& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.posteriorKL(question, arg);
	}
};


SelectionType PKLSelector::getSelectionType() {
	return SelectionType::PKL;
}

Selection PKLSelector::selectItem() {
	Selection selection;
	selection.name = "PKL";
	selection.questions = questionSet.nonapplicable_rows;
	
	selection.values.resize(selection.questions.size());

	mpl::ParallelHelper<PKL> helper(selection.questions, selection.values, estimator, prior);
   	// call parallelFor to do the work
  	RcppParallel::parallelFor(0, selection.questions.size(), helper);

	auto max_itr = std::max_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions.at(std::distance(selection.values.begin(),max_itr));

	selection.question_names.resize(selection.questions.size());

	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	return selection;
}

PKLSelector::PKLSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }