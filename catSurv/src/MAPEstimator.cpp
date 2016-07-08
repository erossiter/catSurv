#include "MAPEstimator.h"

double MAPEstimator::polytomous_d2LL(double theta) {
	double lambda_theta = 0.0;
	for (auto question : questionSet.applicable_rows) {
		const double question_discrimination = pow(questionSet.discrimination[question], 2);
		const double second_derivative = partial_second_derivative(theta, (size_t) question);

		lambda_theta += question_discrimination * second_derivative;
	}
	return lambda_theta;
}

double MAPEstimator::binary_d2LL(double theta) {
	double lambda_theta = 0.0;
	for (auto question : questionSet.applicable_rows) {
		const double P = probability(theta, (size_t) question)[0];
		const double guess = questionSet.guessing[question];
		const double Q = 1.0 - P;
		const double lambda_temp = (P - guess) / (1.0 - guess);
		const double discrimination = questionSet.discrimination[question];

		lambda_theta += pow(discrimination, 2) * pow(lambda_temp, 2) * (Q / P);
	}
	return -lambda_theta;
}


double MAPEstimator::polytomous_dLL(double theta) {
	double l_theta = 0.0;
	for (auto question : questionSet.applicable_rows) {
		const int answer_k = questionSet.answers[question];

		auto probabilities = probability(theta, (size_t) question);
		std::vector<double> probs{0.0};
		probs.insert(probs.end(), probabilities.begin(), probabilities.end());
		probs.push_back(1.0);

		double P_star1 = probs[answer_k];
		double Q_star1 = 1.0 - P_star1;
		double P_star2 = probs[answer_k - 1];
		double Q_star2 = 1 - P_star2;
		double P = P_star1 - P_star2;
		double w2 = P_star2 * Q_star2;
		double w1 = P_star1 * Q_star1;

		l_theta += (-1*questionSet.discrimination[question] * ((w1 - w2) / P));
	}
	return l_theta;
}

double MAPEstimator::binary_dLL(double theta) {
	double l_theta = 0;
	for (auto question : questionSet.applicable_rows) {
		const double P = probability(theta, question)[0];
		const double guess = questionSet.guessing[question];
		const double answer = questionSet.answers[question];
		const double discrimination = questionSet.discrimination[question];
		l_theta += discrimination * ((P - guess) / (P * (1 - guess))) * (answer - P);
	}
	return l_theta;
}

double MAPEstimator::dLL(double theta, bool use_prior, Prior &prior) {
	const double prior_shift = (theta - prior.parameters[0]) / pow(prior.parameters[1], 2);
	if (questionSet.applicable_rows.empty()) {
		return prior_shift;
	}
	double l_theta = questionSet.poly[0] ? polytomous_dLL(theta) : binary_dLL(theta);
	return use_prior ? l_theta - prior_shift : l_theta;
}

double MAPEstimator::d2LL(double theta, bool use_prior, Prior &prior) {
	const double prior_shift = 1.0 / pow(prior.parameters[1], 2);
	if (questionSet.applicable_rows.empty()) {
		return -prior_shift;
	}
	double lambda_theta = questionSet.poly[0] ? polytomous_d2LL(theta) : binary_d2LL(theta);
	return use_prior ? lambda_theta - prior_shift : lambda_theta;
}

double MAPEstimator::estimateTheta(Prior prior) {
  int iter = 0;
  int max_iter = 100;
  
	double theta_hat_old = 0.0;
	double theta_hat_new = 1.0;

	const double tolerance = 0.0000001;

	double difference = std::abs(theta_hat_new - theta_hat_old);
	
	while (difference > tolerance && iter < max_iter) {
	  iter++;
		theta_hat_new = theta_hat_old - dLL(theta_hat_old, true, prior) / d2LL(theta_hat_old, true, prior);
		difference = std::abs(theta_hat_new - theta_hat_old);
		theta_hat_old = theta_hat_new;
	}
	
	return theta_hat_new;
}

double MAPEstimator::estimateSE(Prior prior) {
  double var = 1.0 / (fisherTestInfo(prior)); // minus 1/sigma squared?
  return pow(var, 0.5);
}

EstimationType MAPEstimator::getEstimationType() const {
	return EstimationType::MAP;
}

MAPEstimator::MAPEstimator(Integrator &integrator, QuestionSet &questionSet) : Estimator(integrator, questionSet) { }