#include "MEISelector.h"
#include "ParallelUtil.h"

struct EObsInf_grm : public mpl::FunctionCaller<Prior>
{
	using Base = mpl::FunctionCaller<Prior>;

	EObsInf_grm(Estimator& e, Prior& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.expectedObsInf_grm(question, arg);
	}
};

struct EObsInf_gpcm: public mpl::FunctionCaller<Prior>
{
	using Base = mpl::FunctionCaller<Prior>;

	EObsInf_gpcm(Estimator& e, Prior& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.expectedObsInf_gpcm(question, arg);
	}
};

struct EObsInf_rest: public mpl::FunctionCaller<Prior>
{
	using Base = mpl::FunctionCaller<Prior>;

	EObsInf_rest(Estimator& e, Prior& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.expectedObsInf_rest(question, arg);
	}
};

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

	selection.values.resize(selection.questions.size());

	if(questionSet.model == "grm")
	{
		mpl::ParallelHelper<EObsInf_grm> helper(selection.questions, selection.values, estimator, prior);
   		// call parallelFor to do the work
  		RcppParallel::parallelFor(0, selection.questions.size(), helper);
	}
	else if(questionSet.model == "gpcm")
	{
		mpl::ParallelHelper<EObsInf_gpcm> helper(selection.questions, selection.values, estimator, prior);
   		// call parallelFor to do the work
  		RcppParallel::parallelFor(0, selection.questions.size(), helper);

	}
	else
	{
		mpl::ParallelHelper<EObsInf_rest> helper(selection.questions, selection.values, estimator, prior);
   		// call parallelFor to do the work
  		RcppParallel::parallelFor(0, selection.questions.size(), helper);
	}

	auto max_itr = std::max_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions.at(std::distance(selection.values.begin(),max_itr));

	selection.question_names.resize(selection.questions.size());

	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	return selection;
}
