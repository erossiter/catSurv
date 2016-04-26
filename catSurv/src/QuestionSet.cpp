#include "QuestionSet.h"

QuestionSet::QuestionSet(Rcpp::S4 &cat_df) {
	answers = Rcpp::as<std::vector<int> >(cat_df.slot("answers"));
	guessing = Rcpp::as<std::vector<double> >(cat_df.slot("guessing"));
	discrimination = Rcpp::as<std::vector<double> >(cat_df.slot("discrimination"));


	for (size_t i = 0; i < answers.size(); i++) {
		if (answers[i] == NA_INTEGER) {
			nonapplicable_rows.push_back((int) i); // + 1
		} else {
			applicable_rows.push_back((int) i);
		}
	}

	for (auto item : (Rcpp::List) cat_df.slot("difficulty")) {
		difficulty.push_back(Rcpp::as<std::vector<double> >(item));
	}
	poly = cat_df.slot("poly");
}
