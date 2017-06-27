#include "EPVSelector.h"
#include "ParallelUtil.h"

struct EPV_ltm_tpm : public mpl::FunctionCaller<Prior>
{
	using Base = mpl::FunctionCaller<Prior>;

	EPV_ltm_tpm(Estimator& e, Prior& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.expectedPV_ltm_tpm(question, arg);
	}
};

struct EPV_grm: public mpl::FunctionCaller<Prior>
{
	using Base = mpl::FunctionCaller<Prior>;

	EPV_grm(Estimator& e, Prior& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.expectedPV_grm(question, arg);
	}
};

struct EPV_gpcm: public mpl::FunctionCaller<Prior>
{
	using Base = mpl::FunctionCaller<Prior>;

	EPV_gpcm(Estimator& e, Prior& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.expectedPV_gpcm(question, arg);
	}
};

using namespace std;

Selection EPVSelector::selectItem() {
  
	// For every unanswered item, calculate the epv of that item
	Selection selection = Selection();
	selection.name = getSelectionName();
	selection.questions = questionSet.nonapplicable_rows;

	selection.values.resize(selection.questions.size());

	/**
	if ((questionSet.model == "ltm") || (questionSet.model == "tpm"))
	{
	  	auto epv = [&](int row){return this->estimator.expectedPV_ltm_tpm(row, prior);};
		std::transform(selection.questions.begin(),selection.questions.end(),selection.values.begin(), epv);
	}
	else // if(questionSet.model == "grm" || questionSet.model == "gpcm")
	{
		auto epv = [&](int row){return this->estimator.expectedPV_grm_gpcm(row, prior);};
		std::transform(selection.questions.begin(),selection.questions.end(),selection.values.begin(), epv);
	}
	**/

	if((questionSet.model == "ltm") || (questionSet.model == "tpm"))
	{
		mpl::ParallelHelper<EPV_ltm_tpm> helper(selection.questions, selection.values, estimator, prior);
  		RcppParallel::parallelFor(0, selection.questions.size(), helper);
	}
	else if (questionSet.model == "grm")
	{
		mpl::ParallelHelper<EPV_grm> helper(selection.questions, selection.values, estimator, prior);
  		RcppParallel::parallelFor(0, selection.questions.size(), helper);
	}
	else
	{
		mpl::ParallelHelper<EPV_gpcm> helper(selection.questions, selection.values, estimator, prior);
  		RcppParallel::parallelFor(0, selection.questions.size(), helper);
	}

	
	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};

	selection.question_names.resize(selection.questions.size());
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	auto min_itr = std::min_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions[std::distance(selection.values.begin(),min_itr)];

	return selection;
}

SelectionType  EPVSelector::getSelectionType() {
	return SelectionType::EPV;
}

EPVSelector::EPVSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                      priorModel) { }

std::string EPVSelector::getSelectionName() {
	return "EPV";
}
