#pragma once
#include <Rcpp.h>
#include <memory>
#include "Prior.h"
#include "QuestionSet.h"
#include "Estimator.h"
#include "Selector.h"
#include "CheckRules.h"
#include "MAPEstimator.h"
using namespace Rcpp;

/**
 * This is the primary class for Cat objects, tying together all the other functionality provided in this package.
 * All public methods of this class, with the exception of the constructor, map directly to identically named
 * functions in main.cpp.
 */
class Cat {
public:

	Cat(S4 cat_df);

	double estimateTheta();

	double estimateSE();

	double likelihood(double theta);

	double expectedPV(int item);

	double d1LL(double theta, bool use_prior);

	double d2LL(double theta, bool use_prior);

	double obsInf(double theta, int item);
	
	double fisherInf(double theta, int item);

	double expectedObsInf(int item);
	
	Rcpp::List selectItem();
	
	Rcpp::List lookAhead(int item);
	
	std::vector<bool> checkStopRules();
	
	void showCppCat();

	std::vector<double> probability(double theta, int question);
	
	double expectedKL(int item);
	
	double likelihoodKL(int item);
	
	double posteriorKL(int item);
	
	double fisherTestInfo();

private:

	QuestionSet questionSet;
	Integrator integrator;
	Prior prior;
	CheckRules checkRules;


	/**
	 * In C++, an object of abstract type may not be used an an instance variable. This is because, by virtue of
	 * being abstract, those objects cannot be directly instantiated, and any subclasses that implement all required
	 * methods may have additional fields or methods which cause them to be larger than the space that would be
	 * allocated for the parent. Because the size of the object cannot be determined at runtime, the usage is invalid.
	 * A pointer to an abstract class, however, is allowed, because the pointer is of constant size.
	 *
	 * In this case, subclasses of Estimator and Selector are used, but, because of the above, the instance variables
	 * cannot simply be of type Estimator and Selector. Instead, a pointer to each is stored, and, using createEstimator
	 * and createSelector, instances of the appropriate subtypes are created on the heap.
	 *
	 * Because manual heap management is less than convenient in an Rcpp context (which can make object lifetimes
	 * less than clear), std::unique_ptr, which wraps a pointer and automatically deallocates it when it goes out of
	 * scope, is used.
	 */
	std::unique_ptr<Estimator> estimator;
	std::unique_ptr<Selector> selector;

	/**
	 * These methods are used to create the proper instances for estimator and selector. Ideally, they would be members
	 * of their respective classes, but, because they currently use a naive, string-comparison-based method of
	 * determining which subtype to instantiate, that task is harder than it should be. In the future, this would be
	 * a good refactoring to do.
	 */
	static std::unique_ptr<Estimator> createEstimator(S4 &cat_df, Integrator &integrator, QuestionSet &questionSet);
	static std::unique_ptr<Selector> createSelector(std::string selection_type, QuestionSet &questionSet,
	                                                Estimator &estimator,
	                                                Prior &prior);


};


