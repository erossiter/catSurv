#include "KLSelector.h"
#include "ParallelUtil.h"


struct ExpectedKL : public mpl::FunctionCaller<Prior>
{
	using Base = mpl::FunctionCaller<Prior>;

	ExpectedKL(Estimator& e, Prior& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.expectedKL(question, arg);
	}
};


SelectionType KLSelector::getSelectionType() {
	return SelectionType::KL;
}

Selection KLSelector::selectItem() {
	if (questionSet.applicable_rows.empty()) {
		throw std::domain_error("KL method of selectItem() not applicable when no questions asked");
	}
  
	Selection selection;
	selection.name = "KL";
	selection.questions = questionSet.nonapplicable_rows;

	selection.values.resize(selection.questions.size());

	//auto func = [&](int question){return this->estimator.expectedKL(question, prior);};
	//std::transform(selection.questions.begin(),selection.questions.end(),selection.values.begin(), func);

	mpl::ParallelHelper<ExpectedKL> helper(selection.questions, selection.values, estimator, prior);
   	// call parallelFor to do the work
  	RcppParallel::parallelFor(0, selection.questions.size(), helper);

	auto max_itr = std::max_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions.at(std::distance(selection.values.begin(),max_itr));

	selection.question_names.resize(selection.questions.size());

	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	return selection;
}

KLSelector::KLSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }