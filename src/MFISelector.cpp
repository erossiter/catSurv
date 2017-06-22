#include "MFISelector.h"
#include "ParallelUtil.h"

struct MFI : public mpl::FunctionCaller<double>
{
	using Base = mpl::FunctionCaller<double>;

	MFI(Estimator& e, double& p):Base{e,p}{}

	double operator()(int question)
	{
		return estimator.fisherInf(arg, question);
	}
};

using namespace std;

SelectionType MFISelector::getSelectionType() {
	return SelectionType::MFI;
}

Selection MFISelector::selectItem() {
	Selection selection;
	selection.questions = questionSet.nonapplicable_rows;
	selection.name = "MFI";

	double theta = estimator.estimateTheta(prior);

	selection.values.resize(selection.questions.size());

	mpl::ParallelHelper<MFI> helper(selection.questions, selection.values, estimator, theta);
   	// call parallelFor to do the work
  	RcppParallel::parallelFor(0, selection.questions.size(), helper);

	auto max_itr = std::max_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions.at(std::distance(selection.values.begin(),max_itr));

	selection.question_names.resize(selection.questions.size());

	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	return selection;
}

MFISelector::MFISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions,
                                                                                                      estimation,
                                                                                                      priorModel) { }
