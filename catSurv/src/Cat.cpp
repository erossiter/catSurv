#include "Rcpp.h"
#include "Cat.h"

using namespace Rcpp;

Cat::Cat(QuestionSet &questions, Prior &prior)
		: questionSet(questions), integrator(Integrator()), estimator(integrator, questionSet), prior(prior) {
}

Cat::Cat(S4 cat_df) : questionSet(initialize_questionSet(cat_df)),
                      prior(cat_df),
                      integrator(Integrator()),
                      estimator(EAPEstimator(integrator, QuestionSet())){
	theta_est = Rcpp::as<std::vector<double> >(cat_df.slot("Theta.est"));
	estimator.setQuestionSet(questionSet);
}


double Cat::likelihood(double theta) {
	return estimator.likelihood(theta);
}

std::vector<double> Cat::probability(double theta, int question) {
	return estimator.probability(theta, question);
}


double Cat::estimateTheta() {
	return estimator.estimateTheta(prior);
}

double Cat::estimateSE() {
	return estimator.estimateSE(prior);
}

double Cat::expectedPV(int item) {
	double sum = 0.0;

	questionSet.applicable_rows.push_back(item); // add item to set of answered items
	if (questionSet.poly) {
		std::vector<double> variances;
		for (unsigned i = 0; i < questionSet.difficulty[item].size() + 1; ++i) {
			questionSet.answers[item] = i + 1;
			variances.push_back(estimator.estimateSE(prior));
			variances[i] *= variances[i];
		}
		questionSet.answers[item] = NA_INTEGER;
		questionSet.applicable_rows.pop_back();
		std::vector<double> question_cdf = probability(estimator.estimateTheta(prior), item);
		question_cdf.insert(question_cdf.begin(), 1.0);
		question_cdf.push_back(0.0);

		for (unsigned i = 0; i < question_cdf.size() - 1; ++i) {
			sum += variances[i] * (question_cdf[i] - question_cdf[i + 1]);
		}
		return sum;
	}

	questionSet.answers[item] = 0;
	double variance_zero = estimateSE();
	variance_zero *= variance_zero;

	questionSet.answers[item] = 1;
	double variance_one = estimateSE();
	variance_one *= variance_one;

	questionSet.applicable_rows.pop_back();
	questionSet.answers[item] = NA_INTEGER; // remove answer

	double prob_zero = probability(estimateTheta(), item)[0];
	double prob_one = 1.0 - prob_zero;

	return prob_zero * variance_zero + (prob_one * variance_one);

}

List Cat::nextItem() {
	// For every unanswered item, calculate the epv of that item
	std::vector<double> epvs;
	int min_item = -1;
	double min_epv = DBL_MAX;

	for (int row : questionSet.nonapplicable_rows) {
		double epv = expectedPV(row - 1);
		epvs.push_back(epv);

		if (epv < min_epv) {
			min_item = row;
			min_epv = epv;
		}
	}
	DataFrame all_estimates = Rcpp::DataFrame_Impl<Rcpp::PreserveStorage>::create(
			Named("questions") = questionSet.nonapplicable_rows, Named("EPV") = epvs);
	return Rcpp::List::create(Named("all.estimates") = all_estimates, Named("next.item") = wrap(min_item));
}

QuestionSet Cat::initialize_questionSet(S4 &cat_df) {
	QuestionSet questions;

	questions.answers = Rcpp::as<std::vector<int> >(cat_df.slot("answers"));
	questions.guessing = Rcpp::as<std::vector<double> >(cat_df.slot("guessing"));
	questions.discrimination = Rcpp::as<std::vector<double> >(cat_df.slot("discrimination"));


	for (size_t i = 0; i < questions.answers.size(); i++) {
		if (questions.answers[i] == NA_INTEGER) {
			questions.nonapplicable_rows.push_back(i); // + 1
		} else {
			questions.applicable_rows.push_back(i);
		}
	}

	for (auto item : (List) cat_df.slot("difficulty")) {
		questions.difficulty.push_back(Rcpp::as<std::vector<double> >(item));
	}
	questions.poly = cat_df.slot("poly");
	return questions;


}

