#include "EAPEstimator.h"
#include "GSLFunctionWrapper.h"
#include <limits>

double Estimator::likelihood(double theta) {
	return questionSet.poly[0] ? polytomous_likelihood(theta) : binary_likelihood(theta);
}

std::vector<double> Estimator::probability(double theta, size_t question) {
  // double check this is the eps value Jacob wants
  double eps = std::numeric_limits<double>::min();
  
	auto calculate = [&](double difficulty) {
		double guess = questionSet.guessing.at(question);
		double exp_prob_bi = exp(difficulty + (questionSet.discrimination.at(question) * theta));
		double exp_prob_poly = exp(difficulty - (questionSet.discrimination.at(question) * theta));
		double result = questionSet.poly[0] ? exp_prob_poly / (1 + exp_prob_poly) : guess + (1 - guess) * exp_prob_bi / (1 + exp_prob_bi);
		//if(result < pow(eps, 1.0/3.0)){
		if(result == 0.0){
		  result = 0.1; //sqrt(eps);
		}
		if(result == 1.0){
		  result = 0.999; //1.0 - sqrt(eps);
		}
		return result;
	};

	std::vector<double> probabilities;
	for (auto term : questionSet.difficulty.at(question)) {
		probabilities.push_back(calculate(term));
	}

	return probabilities;
}

double Estimator::polytomous_likelihood(double theta) {
	double L = 0.0;

	for (auto question : questionSet.applicable_rows) {
		size_t unsigned_question = (size_t) question;
		auto question_cdf = paddedProbability(theta, unsigned_question);

		// TODO: Determine what should happen when a negative answer is given
		// TODO: that is, when a user doesn't respond, the value will be negative
		// TODO: Which will result in an out-of-bounds array access

		// TODO: Absolute value and reserve array, if needed.
		int index = questionSet.answers.at(unsigned_question);
		L += log(question_cdf.at((size_t) index) - question_cdf.at(((size_t) index) - 1)) ;
	}
	return exp(L);
}

double Estimator::binary_likelihood(double theta) {
	double L = 0.0;
	for (auto question : questionSet.applicable_rows) {
		size_t index = (size_t) question;
		double prob = probability(theta, index)[0];
		int this_answer = questionSet.answers.at(index);
		L += (this_answer * log(prob)) + ((1 - this_answer) * log(1 - prob));
	}
	return exp(L);
}

Estimator::Estimator(Integrator &integration, QuestionSet &question) : integrator(integration), questionSet(question) { }

double Estimator::estimateSE(Prior prior) {
	const double theta_hat = estimateTheta(prior);

	integrableFunction denominator = [&](double theta) {
		return likelihood(theta) * prior.prior(theta);
	};

	integrableFunction numerator = [&](double theta) {
		const double theta_difference = theta - theta_hat;
		return theta_difference * theta_difference * denominator(theta);
	};

	return sqrt(integralQuotient(numerator, denominator));

}

double Estimator::integralQuotient(integrableFunction const &numerator,
                                   integrableFunction const &denominator) {

	/*
	 * Because GSL is a C library, not a C++ library, it is not easy to pass arbitrary
	 * C++ functions to GSL's integration routine. To solve this, wrap the arbitrary functions
	 * in a GSLFunctionWrapper, which is integrable.
	 */
	gsl_function *numeratorFunction = GSLFunctionWrapper(numerator).asGSLFunction();
	gsl_function *denominatorFunction = GSLFunctionWrapper(denominator).asGSLFunction();

	const double top = integrator.integrate(numeratorFunction, integrationSubintervals);
	const double bottom = integrator.integrate(denominatorFunction, integrationSubintervals);
	return top / bottom;
}

double Estimator::polytomous_posterior_variance(int item, Prior &prior) {
	std::vector<double> variances;
	for (size_t i = 0; i <= questionSet.difficulty[item].size() - 1; ++i) { //Erin added -1 here.
		questionSet.answers[item] = (int) i+1; //Erin added +1 here
		variances.push_back(pow(estimateSE(prior), 2));
    //std::cout << i+1 << std::endl;
	}

	auto question_cdf = paddedProbability(estimateTheta(prior), (size_t) item);

	double sum = 0;
	for (size_t i = 0; i < question_cdf.size() - 1; ++i) {
		sum += variances[i] * (question_cdf[i] - question_cdf[i + 1]);
	}
	return sum;
}

double Estimator::binary_posterior_variance(int item, Prior &prior) {
  const double probability_incorrect = probability(estimateTheta(prior), (size_t) item)[0];
  
	questionSet.answers[item] = 1; //Erin added -1
	double variance_correct = pow(estimateSE(prior), 2);

	questionSet.answers[item] = 0; //Erin added -1
	double variance_incorrect = pow(estimateSE(prior), 2);

	return (probability_incorrect * variance_correct) + ((1.0 - probability_incorrect) * variance_incorrect);
}

double Estimator::expectedPV(int item, Prior &prior) {
	questionSet.applicable_rows.push_back(item); // add item to set of answered items

	double result = questionSet.poly[0] ? polytomous_posterior_variance(item, prior) : binary_posterior_variance(item,
	                                                                                                          prior);
	questionSet.answers[item] = NA_INTEGER; // remove answer
	questionSet.applicable_rows.pop_back();
	return result;
}

double Estimator::partial_second_derivative(double theta, size_t question) {
	size_t answer_k = (size_t) questionSet.answers.at(question);

	auto probabilities = paddedProbability(theta, question);

	double P_star1 = probabilities.at(answer_k);
	double P_star2 = probabilities.at(answer_k - 1);
	double P = P_star2 - P_star1;

	double Q_star1 = 1 - P_star1;
	double Q_star2 = 1 - P_star2;

	double w2 = P_star2 * Q_star2;
	double w1 = P_star1 * Q_star1;
	double w = w2 - w1;

	double first_term = (-w1 * (Q_star1 - P_star1) + w2 * (Q_star2 - P_star2)) / P;
	double second_term = pow(w, 2) / pow(P, 2);

	return first_term - second_term;
}

double Estimator::obsInf(double theta, int item) {
	if (questionSet.applicable_rows.empty()) {
		throw std::domain_error("ObsInf should not be called if no items have been answered.");
	}

	size_t index = (size_t) item;
	double discrimination = questionSet.discrimination.at(index);

	if (questionSet.poly[0]) {
		return -pow(discrimination, 2) * partial_second_derivative(theta, index);
	}

	double guess = questionSet.guessing.at(index);
	double P = probability(theta, index)[0];
	double Q = 1 - P;
	double temp = pow((P - guess) / (1 - guess), 2);
	return pow(discrimination, 2) * temp * (Q / P);
}

double Estimator::fisherInf(double theta, int item) {

	if (!questionSet.poly[0]) {
		return obsInf(theta, item);
	}

	double output = 0.0;
	auto probabilities = paddedProbability(theta, (size_t) item);


	double discrimination_squared = pow(questionSet.discrimination[item], 2);
	for (size_t i = 1; i <= questionSet.difficulty[item].size(); ++i) {
		double P_star1 = probabilities[i];
		double P_star2 = probabilities[i - 1];
		double w1 = P_star1 * (1.0 - P_star1);
		double w2 = P_star2 * (1.0 - P_star2);

		output += discrimination_squared * (pow(w2 - w1, 2) / (P_star2 - P_star1));
	}
	return output;
}

/**
 * Computes the probability for a given theta and question number,
 * then pads it with a 1.0 at the beginning, and a 0.0 at the end.
 */
// Erin changed this so it has a 0.0 at beginning and 1.0 at end
std::vector<double> Estimator::paddedProbability(double theta, size_t question) {
	std::vector<double> probabilities = probability(theta, question);
	std::vector<double> padded{0.0};
	padded.insert(padded.end(), probabilities.begin(), probabilities.end());
	padded.push_back(1.0);
	return padded;
}

double Estimator::expectedObsInf(int item, Prior &prior) {
	questionSet.applicable_rows.push_back(item);
	if (questionSet.poly[0]){
		double sum = 0.0;
		std::vector<double> obsInfs;
		for (size_t i = 0; i <= questionSet.difficulty[item].size(); ++i){
			questionSet.answers[item] = (int) i + 1;
			obsInfs.push_back(obsInf(estimateTheta(prior), item));
		}

		questionSet.answers[item] = NA_INTEGER;
		questionSet.applicable_rows.pop_back();

		std::vector<double> question_cdf = paddedProbability(estimateTheta(prior), (size_t) item);
		for(size_t i = 0; i < question_cdf.size() -1; ++i){
			sum += obsInfs[i] * (question_cdf[i] - question_cdf[i + 1]);
		}
		return sum;
	}

		questionSet.answers[item] = 0;
		double obsInfZero = obsInf(estimateTheta(prior), item);
		questionSet.answers[item] = 1;
		double obsInfOne = obsInf(estimateTheta(prior), item);
		questionSet.applicable_rows.pop_back();
		questionSet.answers[item] = NA_INTEGER;

		double prob_one = probability(estimateTheta(prior), (size_t) item)[0];
		return prob_one + obsInfZero - (prob_one * obsInfZero) + obsInfOne;
}






