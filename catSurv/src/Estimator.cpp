#include "EAPEstimator.h"
#include "GSLFunctionWrapper.h"
#include "NotImplementedException.h"

double Estimator::likelihood(double theta) {
	return questionSet.poly ? polynomial_likelihood(theta) : binary_likelihood(theta);
}


std::vector<double> Estimator::probability(double theta, size_t question) {

	auto calculate = [&](double difficulty) {
		double guess = questionSet.guessing.at(question);
		double exp_prob = exp(questionSet.discrimination.at(question) * (theta - difficulty));
		double base_probability = exp_prob / (1 + exp_prob);
		return questionSet.poly ? base_probability : guess + (1 - guess) * base_probability;
	};

	std::vector<double> probabilities;
	for (auto term : questionSet.difficulty.at(question)) {
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

		// TODO: Determine what should happen when a negative answer is given
		// TODO: that is, when a user doesn't respond, the value will be negative
		// TODO: Which will result in an out-of-bounds array access
		int index = questionSet.answers.at(question);
		L *= question_cdf[index - 1] - question_cdf[index];
	}
	return L;
}

double Estimator::binary_likelihood(double theta) {
	double L = 1.0;
	for (auto question : questionSet.applicable_rows) {
		size_t index = (size_t) question;
		double prob = probability(theta, index)[0];
		int this_answer = questionSet.answers.at(index);
		L *= pow(prob, this_answer) * pow(1 - prob, 1 - this_answer);
	}
	return L;
}

Estimator::Estimator(Integrator &integration, QuestionSet &question) : integrator(integration),
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

	auto probabilities = probability(estimateTheta(prior), (size_t) item);
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

	const double prob_zero = probability(estimateTheta(prior), (size_t) item)[0];
	return prob_zero * (variance_zero - variance_one) + variance_one;
}

double Estimator::expectedPV(int item, Prior &prior) {
	questionSet.applicable_rows.push_back((size_t) item); // add item to set of answered items

	double result = questionSet.poly ? polytomous_posterior_variance(item, prior) : binary_posterior_variance(item,
	                                                                                                          prior);
	questionSet.answers[item] = NA_INTEGER; // remove answer
	questionSet.applicable_rows.pop_back();
	return result;
}

double Estimator::partial_second_derivative(double theta, size_t question) {
	size_t answer_k = (size_t) questionSet.answers.at(question);

	auto probabilities = probability(theta, question);
	std::vector<double> probs{1.0};
	probs.insert(probs.end(), probabilities.begin(), probabilities.end());
	probs.push_back(0.0);

	const double P_star1 = probs.at(answer_k);
	const double P_star2 = probs.at(answer_k - 1);
	const double P = P_star2 - P_star1;

	const double Q_star1 = 1 - P_star1;
	const double Q_star2 = 1 - P_star2;

	const double w2 = P_star2 * Q_star2;
	const double w1 = P_star1 * Q_star1;
	const double w  = w2 - w1;

	const double first_term = (-w1 * (Q_star1 - P_star1) + w2 * (Q_star2 - P_star2)) / P;
	const double second_term = pow(w, 2) / pow(P, 2);

	return first_term - second_term;
}

double Estimator::obsInf(double theta, int item) {
	if (questionSet.applicable_rows.empty()) {
		throw std::domain_error("ObsInf should not be called if no items have been answered.");
	}

	size_t index = (size_t) item;
	double discrimination = questionSet.discrimination.at(index);

	if (questionSet.poly) {
		return -pow(discrimination, 2) * partial_second_derivative(theta, index);
	}

	double guess = questionSet.guessing.at(index);
	double P = probability(theta, index)[0];
	double Q = 1 - P;
	double temp = pow((P - guess) / (1 - guess), 2);
	return pow(discrimination, 2) * temp * (Q / P);
}