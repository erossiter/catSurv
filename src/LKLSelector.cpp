#include "LKLSelector.h"
#include "ParallelUtil.h"


struct LikelihoodKL : public mpl::FunctionCaller<Prior>
{
	using Base = mpl::FunctionCaller<Prior>;

	LikelihoodKL(Estimator& e, Prior& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.likelihoodKL(question, arg);
	}
};

SelectionType LKLSelector::getSelectionType() {
	return SelectionType::LKL;
}

Selection LKLSelector::selectItem() {
	Selection selection;
	selection.name = "LKL";
	selection.questions = questionSet.nonapplicable_rows;

	selection.values.resize(selection.questions.size());

	mpl::ParallelHelper<LikelihoodKL> helper(selection.questions, selection.values, estimator, prior);
   	// call parallelFor to do the work
  	RcppParallel::parallelFor(0, selection.questions.size(), helper);

	auto max_itr = std::max_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions.at(std::distance(selection.values.begin(),max_itr));

	selection.question_names.resize(selection.questions.size());

	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	return selection;
}

LKLSelector::LKLSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }