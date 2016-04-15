#include "Estimator.h"

double Estimator::likelihood(double theta) {
	return questionSet.poly ? polynomial_likelihood(theta) : binary_likelihood(theta);
}

std::vector<double> Estimator::probability(double theta, int question) {

	auto calculate = [&](int difficulty) {
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

double Estimator::estimateSE(Prior prior) {
	throw std::runtime_error("Not yet implemented");
//	std::vector<double> fx;
//	std::vector<double> fx_theta;
//
//	const double theta_hat = estimateTheta(prior);
//
//	for (size_t i = 0; i < questionSet.X.size(); ++i) {
//		fx.push_back(
//				likelihood(questionSet.X[i]) *
//				prior.values[i]);
//		fx_theta.push_back((questionSet.X[i] - theta_hat) * (questionSet.X[i] - theta_hat) * fx[i]);
//	}
//	return sqrt(integrator.integrate(questionSet.X, fx_theta) / integrator.integrate(questionSet.X, fx));

}

double Estimator::polynomial_likelihood(double theta) {
	double L = 1.0;

	for (auto question : questionSet.applicable_rows) {
		std::vector<double> question_cdf{1.0};
		for (auto i : probability(theta, question)) {
			question_cdf.push_back(i);
		}
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