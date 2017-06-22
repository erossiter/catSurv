#include "KLSelector.h"

 // [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>


using namespace RcppParallel;

struct ExpectedKL
{
	Estimator &estimator;
	Prior &prior;

	ExpectedKL(Estimator& e, Prior& p)
	: estimator(e)
	, prior(p)
	{}

	double operator()(int question)
	{
		return estimator.expectedKL(question, prior);
	}
};

struct ParallelExpectedKL : public Worker
{
   const std::vector<int>& input; // source vector
   std::vector<double>& output; // destination vector
   ExpectedKL expectedKL;
   
   // initialize with source and destination
   template<typename T1, typename T2>
   ParallelExpectedKL(const T1& input, T2& output, Estimator& e, Prior& p) 
      : input(input)
      , output(output)
      , expectedKL{e,p}
      {}
   
   // take the range of elements requested
   void operator()(std::size_t begin, std::size_t end)
   {
      std::transform(input.begin() + begin, input.begin() + end, output.begin() + begin, expectedKL);
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

	ParallelExpectedKL parallelExpectedKL(selection.questions, selection.values, estimator,prior);
   	// call parallelFor to do the work
  	parallelFor(0, selection.questions.size(), parallelExpectedKL);

	auto max_itr = std::max_element(selection.values.begin(), selection.values.end());
	selection.item = selection.questions.at(std::distance(selection.values.begin(),max_itr));

	selection.question_names.resize(selection.questions.size());

	auto qn_name = [&](int question){return this->questionSet.question_names.at(question);};
	std::transform(selection.questions.begin(),selection.questions.end(),selection.question_names.begin(), qn_name);

	return selection;
}

KLSelector::KLSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel) : Selector(questions, estimation,
                                                                                                        priorModel) { }