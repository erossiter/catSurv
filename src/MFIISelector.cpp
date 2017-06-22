#include "MFIISelector.h"
#include "ParallelUtil.h"

struct MFII : public mpl::FunctionCaller<Prior>
{
	using Base = mpl::FunctionCaller<Prior>;

	MFII(Estimator& e, Prior& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.fii(question, arg);
	}
};

using namespace std;

SelectionType MFIISelector::getSelectionType() {
	return SelectionType::MFI;
}

Selection MFIISelector::selectItem() {
  if (questionSet.applicable_rows.empty()) {
		throw std::domain_error("MFII method of selectItem() not applicable when no questions asked");
	}
  	
	Selection selection;
	selection.questions = questionSet.nonapplicable_rows;
	selection.name = "MFII";

	selection.values.resize(selection.questions.size());

	mpl::ParallelHelper<MFII> helper(selection.questions, selection.values, estimator, prior);
   	// call parallelFor to do the work
  	RcppParallel::parallelFor(0, selection.questions.size(), helper);

	auto max_itr = std::max_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions.at(std::distance(selection.values.begin(),max_itr));

	selection.question_names.resize(selection.questions.size());

	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	return selection;
}

MFIISelector::MFIISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions,
                                                                                                      estimation,
                                                                                                      priorModel) { }
