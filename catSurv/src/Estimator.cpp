#include "Estimator.h"

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