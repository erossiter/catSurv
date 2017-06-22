#include "EPVSelector.h"

#include <iostream>
 // [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>


using namespace RcppParallel;

struct ExpectedPV
{
	Estimator &estimator;
	Prior &prior;

	ExpectedPV(Estimator& e, Prior& p)
	: estimator(e)
	, prior(p)
	{}

	double operator()(int row)
	{
		return estimator.expectedPV(row, prior);
	}
};

struct ParallelExpectedPV : public Worker
{
   //const RVector<int> input; // source vector
   //RVector<double> output; // destination vector
   const std::vector<int>& input; // source vector
   std::vector<double>& output; // destination vector
   ExpectedPV expectedPV;
   
   // initialize with source and destination
   template<typename T1, typename T2>
   ParallelExpectedPV(const T1& input, T2& output, Estimator& e, Prior& p) 
      : input(input)
      , output(output)
      , expectedPV{e,p}
      {}
   
   // take the range of elements requested
   void operator()(std::size_t begin, std::size_t end)
   {
      std::transform(input.begin() + begin, input.begin() + end, output.begin() + begin, expectedPV);
   }
};

using namespace std;

Selection EPVSelector::selectItem() {
  
	// For every unanswered item, calculate the epv of that item
	Selection selection = Selection();
	selection.name = getSelectionName();
	selection.values.reserve(questionSet.nonapplicable_rows.size());
	selection.questions = questionSet.nonapplicable_rows;

	//for (int row : questionSet.nonapplicable_rows) {
	//	double epv = estimator.expectedPV(row, prior);
	//	selection.values.push_back(epv);
	//}

	auto func = [&](int row){return this->estimator.expectedPV(row, prior);};

	selection.values.resize(questionSet.nonapplicable_rows.size());
	//std::transform(questionSet.nonapplicable_rows.begin(),questionSet.nonapplicable_rows.end(),selection.values.begin(), func);

	ParallelExpectedPV parallelExpectedPV(questionSet.nonapplicable_rows, selection.values,estimator,prior);
  
  	// call parallelFor to do the work
  	parallelFor(0, questionSet.nonapplicable_rows.size(), parallelExpectedPV);

	selection.question_names.resize(questionSet.nonapplicable_rows.size());
	std::copy(questionSet.question_names.begin(),questionSet.question_names.end(),selection.question_names.begin());

	auto min_itr = std::min_element(selection.values.begin(), selection.values.end());
	selection.item = questionSet.nonapplicable_rows[std::distance(selection.values.begin(),min_itr)];

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
