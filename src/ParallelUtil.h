#pragma once
#include "Estimator.h"

#include <RcppParallel.h>


//using namespace RcppParallel;

namespace mpl
{
	template<typename Arg>
	struct FunctionCaller
	{
		Estimator &estimator;
		Arg &arg;

		FunctionCaller(Estimator& e, Arg& a)
		: estimator(e)
		, arg(a)
		{}

		double operator()(int question); // virtual not needed as we dont need dynamic dispatch
	};

	template<typename Function>
	struct ParallelHelper : public RcppParallel::Worker
	{
	   const std::vector<int>& input; // source vector
	   std::vector<double>& output; // destination vector
	   Function f;
	   
	   // initialize with source and destination
	   template<typename T1, typename T2, typename Arg>
	   ParallelHelper(const T1& input, T2& output, Estimator& e, Arg& a) 
	      : input(input)
	      , output(output)
	      , f{e,a}
	      {}
	   
	   // take the range of elements requested
	   void operator()(std::size_t begin, std::size_t end)
	   {
	      std::transform(input.begin() + begin, input.begin() + end, output.begin() + begin, f);
	   }
	};
}