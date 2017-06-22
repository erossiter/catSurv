#include "MPWISelector.h"
#include "ParallelUtil.h"

struct MPWI : public mpl::FunctionCaller<Prior>
{
	using Base = mpl::FunctionCaller<Prior>;

	MPWI(Estimator& e, Prior& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.pwi(question, arg);
	}
};

SelectionType MPWISelector::getSelectionType() {
	return SelectionType::MPWI;
}

Selection MPWISelector::selectItem() {
	Selection selection;
	selection.name = "MPWI";
	selection.questions = questionSet.nonapplicable_rows;
	
	selection.values.resize(selection.questions.size());

	mpl::ParallelHelper<MPWI> helper(selection.questions, selection.values, estimator, prior);
   	// call parallelFor to do the work
  	RcppParallel::parallelFor(0, selection.questions.size(), helper);

	auto max_itr = std::max_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions.at(std::distance(selection.values.begin(),max_itr));

	selection.question_names.resize(selection.questions.size());

	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	return selection;
}

MPWISelector::MPWISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }