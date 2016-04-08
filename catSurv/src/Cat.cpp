#include "Rcpp.h"
#include "Cat.h"

using namespace Rcpp;


Cat::Cat(S4 cat_df) : prior(cat_df) {
	questionSet = (QuestionSet) {.answers = Rcpp::as<std::vector<int> >(cat_df.slot("answers")),
			.guessing =  Rcpp::as<std::vector<double> >(cat_df.slot("guessing")),
			.discrimination = Rcpp::as<std::vector<double> >(cat_df.slot("discrimination"))
	};


	for (int i = 0; i < questionSet.answers.size(); i++) {
		if (questionSet.answers[i] == NA_INTEGER) {
			questionSet.nonapplicable_rows.push_back(i + 1);
		} else {
			questionSet.applicable_rows.push_back(i);
		}
	}

	for (auto item : (List) cat_df.slot("difficulty")) {
		questionSet.difficulty.push_back(Rcpp::as<std::vector<double> >(item));
	}


	this->D = Rcpp::as<std::vector<double> >(cat_df.slot("D"))[0];
	this->theta_est = Rcpp::as<std::vector<double> >(cat_df.slot("Theta.est"));
}

double Cat::likelihood(double theta, std::vector<int> items) {
	double L = 1.0;
	if (questionSet.poly) {
		for (auto question : items) {
			auto question_cdf = probability(theta, question);
			question_cdf.insert(question_cdf.begin(), 1.0);
			question_cdf.push_back(0.0);
			int index = questionSet.answers[question] - 1;
			L *= question_cdf[index - 1] - question_cdf[index];
		}
		return L;
	}

	for (auto question : items) {
		double prob = probability(theta, question)[0];
		int this_answer = questionSet.answers[question];
		L *= pow(prob, this_answer) * pow(1 - prob, 1 - this_answer);
	}
	return L;
}

std::vector<double> Cat::probability(double theta, int question) {
	double guessing = questionSet.guessing[question - 1];

	auto calculate = [&](int difficulty) {
		double exp_prob = exp(D * questionSet.discrimination[question - 1] * (theta - difficulty));
		double base_probability = (exp_prob) / (1 + exp_prob);
		return questionSet.poly ? base_probability : guessing + (1 - guessing) * base_probability;
	};

	std::vector<double> probabilities;
	for (auto term : questionSet.difficulty[question - 1]) {
		probabilities.push_back(calculate(term));
	}
	return probabilities;
}

Cat::Cat(QuestionSet &questionSet, Prior &prior)
		: questionSet(questionSet), prior(prior) { }

double Cat::estimateTheta() {
	return estimator.estimateTheta(questionSet, prior);
}

double Cat::estimateSE() {
	return estimator.estimateSE(questionSet, prior);
}

double Cat::expectedPV(int item) {
	double sum = 0.0;

	questionSet.applicable_rows.push_back(item); // add item to set of answered items
	if (questionSet.poly) {
		std::vector<double> variances;
		for (unsigned i = 0; i < questionSet.difficulty[item].size() + 1; ++i) {
			questionSet.answers[item] = i + 1;
			variances.push_back(estimator.estimateSE(questionSet, prior));
			variances[i] *= variances[i];
		}
		questionSet.answers[item] = NA_INTEGER;
		questionSet.applicable_rows.pop_back();
		std::vector<double> question_cdf = probability(estimator.estimateTheta(questionSet, prior), item);
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