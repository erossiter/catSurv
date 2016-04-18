#include "EAPEstimator.h"
#include "GSLFunctionWrapper.h"

double EAPEstimator::estimateTheta(Prior prior) {

	integrableFunction denominator = [&](double theta) {
		return likelihood(theta) * prior.prior(theta);
	};

	integrableFunction numerator = [&](double theta) {
		return theta * denominator(theta);
	};

	return integralQuotient(numerator, denominator);
}

double EAPEstimator::estimateSE(Prior prior) {
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

double EAPEstimator::integralQuotient(integrableFunction const &numerator,
                                      integrableFunction const &denominator) {

	gsl_function *numeratorFunction = GSLFunctionWrapper(numerator).asGSLFunction();
	gsl_function *denominatorFunction = GSLFunctionWrapper(denominator).asGSLFunction();

	const double top = integrator.integrate(numeratorFunction, 10);
	const double bottom = integrator.integrate(denominatorFunction, 10);
	return top / bottom;
}

EstimationType EAPEstimator::getIntegrationType() const {
	return EstimationType::EAP;
}

EAPEstimator::EAPEstimator(const Integrator &integrator, const QuestionSet &questionSet) : Estimator(integrator,
                                                                                                     questionSet) { }

double EAPEstimator::polytomous_posterior_variance(int item, Prior &prior) {
	std::vector<double> variances;
	for (size_t i = 0; i <= questionSet.difficulty[item].size(); ++i) {
		questionSet.answers[item] = (int) i + 1;
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

double EAPEstimator::binary_posterior_variance(int item, Prior &prior) {

	questionSet.answers[item] = 0;
	double variance_zero = pow(estimateSE(prior), 2);

	questionSet.answers[item] = 1;
	double variance_one = pow(estimateSE(prior), 2);

	const double prob_zero = probability(estimateTheta(prior), item)[0];
	return prob_zero * (variance_zero - variance_one) + variance_one;
}

double EAPEstimator::expectedPV(int item, Prior &prior) {
	questionSet.applicable_rows.push_back(item); // add item to set of answered items

	double result = questionSet.poly ? polytomous_posterior_variance(item, prior) : binary_posterior_variance(item,
	                                                                                                          prior);
	questionSet.answers[item] = NA_INTEGER; // remove answer
	questionSet.applicable_rows.pop_back();
	return result;
}