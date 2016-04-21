#include "EAPEstimator.h"
#include "GSLFunctionWrapper.h"
#include "NotImplementedException.h"

double Estimator::likelihood(double theta) {
	return questionSet.poly ? polynomial_likelihood(theta) : binary_likelihood(theta);
}

std::vector<double> Estimator::probability(double theta, int question) {

	auto calculate = [&](double difficulty) {
		double exp_prob = exp(questionSet.discrimination[question] * (theta - difficulty));
		double base_probability = exp_prob / (1 + exp_prob);
		double poly_answer = questionSet.guessing[question] + (1 - questionSet.guessing[question]);
		return questionSet.poly ? base_probability : poly_answer * base_probability;
	};

	std::vector<double> probabilities;
	for (auto term : questionSet.difficulty[question]) {
		probabilities.push_back(calculate(term));
	}
	return probabilities;
}

double Estimator::polynomial_likelihood(double theta) {
	double L = 1.0;

	for (auto question : questionSet.applicable_rows) {
		auto probabilities = probability(theta, question);
		std::vector<double> question_cdf{1.0};
		question_cdf.insert(question_cdf.end(), probabilities.begin(), probabilities.end());
		question_cdf.push_back(0.0);

		size_t index = (size_t) questionSet.answers.at((size_t) question);
		L *= question_cdf.at(index - 1) - question_cdf.at(index);
	}
	return L;
}

double Estimator::binary_likelihood(double theta) {
	double L = 1.0;
	for (auto question : questionSet.applicable_rows) {
		double prob = probability(theta, question)[0];
		int this_answer = questionSet.answers.at((size_t) question);
		L *= pow(prob, this_answer) * pow(1 - prob, 1 - this_answer);
	}
	return L;
}

Estimator::Estimator(Integrator integration, QuestionSet question) : integrator(integration),
                                                                     questionSet(question) { }

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

	gsl_function *numeratorFunction = GSLFunctionWrapper(numerator).asGSLFunction();
	gsl_function *denominatorFunction = GSLFunctionWrapper(denominator).asGSLFunction();

	const double top = integrator.integrate(numeratorFunction, integrationSubintervals);
	const double bottom = integrator.integrate(denominatorFunction, integrationSubintervals);
	return top / bottom;
}

double Estimator::polytomous_posterior_variance(int item, Prior &prior) {
	std::vector<double> variances;
	for (size_t i = 0; i <= questionSet.difficulty[item].size(); ++i) {
		questionSet.answers[item] = (int) i;
		variances.push_back(pow(estimateSE(prior), 2));
	}

	auto probabilities = probability(estimateTheta(prior), item);
	std::vector<double> question_cdf{1.0};
	question_cdf.insert(question_cdf.end(), probabilities.begin(), probabilities.end());
	question_cdf.push_back(0.0);

	double sum = 0;
	for (size_t i = 0; i < question_cdf.size() - 1; ++i) {
		sum += variances[i] * (question_cdf[i] - question_cdf[i + 1]);
	}
	return sum;
}

double Estimator::binary_posterior_variance(int item, Prior &prior) {

	questionSet.answers[item] = 0;
	double variance_zero = pow(estimateSE(prior), 2);

	questionSet.answers[item] = 1;
	double variance_one = pow(estimateSE(prior), 2);

	const double prob_zero = probability(estimateTheta(prior), item)[0];
	return prob_zero * (variance_zero - variance_one) + variance_one;
}

double Estimator::expectedPV(int item, Prior &prior) {
	questionSet.applicable_rows.push_back(item); // add item to set of answered items

	double result = questionSet.poly ? polytomous_posterior_variance(item, prior) : binary_posterior_variance(item,
	                                                                                                          prior);
	questionSet.answers[item] = NA_INTEGER; // remove answer
	questionSet.applicable_rows.pop_back();
	return result;
}

double Estimator::estimateTheta(Prior prior) {
	(void) prior;
	throw NotImplementedException("This function is not implemented in the base class.");
}

EstimationType Estimator::getEstimationType() const {
	return EstimationType::NONE;
}
