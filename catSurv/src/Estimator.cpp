#include "Estimator.h"

double Estimator::likelihood(double theta) {
	return questionSet.poly ? polynomial_likelihood(theta) : binary_likelihood(theta);
}

std::vector<double> Estimator::probability(double theta, int question) {
	return calculate_probability(questionSet.guessing[question - 1], questionSet.discrimination[question - 1],
	                             questionSet.difficulty[question - 1], questionSet.poly, theta);
}

std::vector<double> Estimator::calculate_probability(double guessing, double discrimination,
                                                     std::vector<double> difficulty_level, bool poly,
                                                     double theta) {

	auto calculate = [&](int difficulty) {
		double exp_prob = exp(discrimination * (theta - difficulty));
		double base_probability = exp_prob / (1 + exp_prob);
		return poly ? base_probability : guessing + (1 - guessing) * base_probability;
	};

	std::vector<double> probabilities;
	for (auto term : difficulty_level) {
		probabilities.push_back(calculate(term));
	}
	return probabilities;
}

double Estimator::estimateSE(Prior prior) {
	std::vector<double> fx;
	std::vector<double> fx_theta;

	const double theta_hat = estimateTheta(prior);

	for (size_t i = 0; i < questionSet.X.size(); ++i) {
		fx.push_back(
				likelihood(questionSet.X[i]) *
				prior.values[i]);
		fx_theta.push_back((questionSet.X[i] - theta_hat) * (questionSet.X[i] - theta_hat) * fx[i]);
	}
	return sqrt(integrator.integrate(questionSet.X, fx_theta) / integrator.integrate(questionSet.X, fx));
}

double Estimator::polynomial_likelihood(double theta) {
	double L = 1.0;
	for (auto question : questionSet.applicable_rows) {
		auto question_cdf = probability(theta, question);
		question_cdf.insert(question_cdf.begin(), 1.0);
		question_cdf.push_back(0.0);
		int index = questionSet.answers[question] - 1;
		L *= question_cdf[index - 1] - question_cdf[index];
	}
	return L;
}

double Estimator::binary_likelihood(double theta) {
	double L = 1.0;
	for (auto question : questionSet.applicable_rows) {
		double prob = probability(theta, question)[0];
		int this_answer = questionSet.answers[question];
		L *= pow(prob, this_answer) * pow(1 - prob, 1 - this_answer);
	}
	return L;
}

Estimator::Estimator(Integrator integrator, QuestionSet questionSet) : integrator(integrator),
                                                                       questionSet(questionSet) { }