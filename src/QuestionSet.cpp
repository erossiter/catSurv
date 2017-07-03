#include "QuestionSet.h"

QuestionSet::QuestionSet(Rcpp::S4 &cat_df) {
	answers = Rcpp::as<std::vector<int> >(cat_df.slot("answers"));
	guessing = Rcpp::as<std::vector<double> >(cat_df.slot("guessing"));
	discrimination = Rcpp::as<std::vector<double> >(cat_df.slot("discrimination"));
	z = Rcpp::as<std::vector<double> >(cat_df.slot("z"));
	
	lowerBound = Rcpp::as<double >(cat_df.slot("lowerBound"));
	upperBound = Rcpp::as<double >(cat_df.slot("upperBound"));
	
	Rcpp::NumericVector discrim_names = cat_df.slot("discrimination");
  	Rcpp::CharacterVector names = discrim_names.names();
  	question_names = Rcpp::as<std::vector<std::string> >(names);

  	model = Rcpp::as<std::string >(cat_df.slot("model"));


	for (auto item : (Rcpp::List) cat_df.slot("difficulty")) {
		difficulty.push_back(Rcpp::as<std::vector<double> >(item));
	}

	
	reset_applicables();
	reset_all_extreme();
}

void QuestionSet::reset_answers(Rcpp::DataFrame& responses, size_t row)
{
	for(size_t i = 0; i < answers.size(); ++i)
	{
		Rcpp::IntegerVector col = responses[i]; 
		answers[i] = col[row];
	}
	
	reset_applicables();
	reset_all_extreme();
}

void QuestionSet::reset_answers(std::vector<int> const& source)
{
	std::copy(source.begin(), source.end(), answers.begin());
	reset_applicables();
	reset_all_extreme();
}

void QuestionSet::reset_answer(size_t question, int answer)
{
	if(answer == answers[question])
	{
		return; // nothing to be done
	}

	int old_answer = answers[question];
	answers[question] = answer;

	
	if(old_answer == NA_INTEGER)
	{
		if(answer != NA_INTEGER)
		{
			// remove question from nonapplicable_rows
			auto itr = std::lower_bound(nonapplicable_rows.begin(), nonapplicable_rows.end(), question);
			nonapplicable_rows.erase(itr);

			if(answer == -1)
			{
				// insert in skipped
				auto itr = std::lower_bound(skipped.begin(), skipped.end(), question);
				skipped.insert(itr, question);
			}
			else
			{
				// insert in applicable_rows
				auto itr = std::lower_bound(applicable_rows.begin(), applicable_rows.end(), question);
				applicable_rows.insert(itr, question);
			}
		}
	}
	else if(old_answer == -1)
	{
		// remove question from skipped
		auto itr = std::lower_bound(skipped.begin(), skipped.end(), question);
		skipped.erase(itr);

		if(answer == NA_INTEGER)
		{
			// insert in nonapplicable_rows
			auto itr = std::lower_bound(nonapplicable_rows.begin(), nonapplicable_rows.end(), question);
			nonapplicable_rows.insert(itr, question);
		}
		else
		{
			// insert in applicable_rows
			auto itr = std::lower_bound(applicable_rows.begin(), applicable_rows.end(), question);
			applicable_rows.insert(itr, question);
		}
	}
	else
	{
		if(answer == -1)
		{
			// remove question from applicable_rows
			auto itr = std::lower_bound(applicable_rows.begin(), applicable_rows.end(), question);
			applicable_rows.erase(itr);

			// insert in skipped
			itr = std::lower_bound(skipped.begin(), skipped.end(), question);
			skipped.insert(itr, question);
		}
		else if (answer == NA_INTEGER)
		{
			// remove question from applicable_rows
			auto itr = std::lower_bound(applicable_rows.begin(), applicable_rows.end(), question);
			applicable_rows.erase(itr);

			// insert in nonapplicable_rows
			itr = std::lower_bound(nonapplicable_rows.begin(), nonapplicable_rows.end(), question);
			nonapplicable_rows.insert(itr, question);
		}
	}

	reset_all_extreme();
}

void QuestionSet::reset_applicables()
{
	nonapplicable_rows.clear();
	nonapplicable_rows.reserve(answers.size()); // max capacity, no problem unless questions size goes beyond thousands

	applicable_rows.clear();
	applicable_rows.reserve(answers.size());

	skipped.clear();
	skipped.reserve(answers.size());

	for (size_t i = 0; i < answers.size(); i++) {
		if (answers.at(i) == NA_INTEGER)
		{
			nonapplicable_rows.push_back(i);
		}
		else if (answers.at(i) != -1)
		{
			applicable_rows.push_back(i);
		}
		else
		{
		  skipped.push_back(i);
		} 
	}
}

void QuestionSet::reset_all_extreme()
{
	// Added all this is to check if its appropriate to use MLE
	bool minAnswer_posDiscrim = false;
	bool maxAnswer_posDiscrim = false;
	bool minAnswer_negDiscrim = false;
	bool maxAnswer_negDiscrim = false;
	bool ans_not_extreme = false;
	
	int max_response = ((model == "ltm") | (model == "tpm")) ? 1.0 : difficulty[1].size() + 1.0;
	int min_response = ((model == "ltm") | (model == "tpm")) ? 0.0 : 1.0;

	for (auto i : applicable_rows) {
	  	if (discrimination.at(i) < 0.0 and answers.at(i) == min_response) minAnswer_negDiscrim = true;
	  	else if (discrimination.at(i) < 0.0 and answers.at(i) == max_response) maxAnswer_negDiscrim = true;
	  	else if (discrimination.at(i) > 0.0 and answers.at(i) == min_response) minAnswer_posDiscrim = true;
	  	else if (discrimination.at(i) > 0.0 and answers.at(i) == max_response) maxAnswer_posDiscrim = true;
	  	else
	  	{
	  		ans_not_extreme = true;
	  		break;
	  	}
	}

	all_extreme = false;

	if(!ans_not_extreme)
	{
		if (minAnswer_posDiscrim and maxAnswer_negDiscrim and (!minAnswer_negDiscrim)  and (!maxAnswer_posDiscrim))
		{
	  		all_extreme = true;
		} else if ((!minAnswer_posDiscrim)  and (!maxAnswer_negDiscrim) and minAnswer_negDiscrim and maxAnswer_posDiscrim)
		{
	  		all_extreme = true;
		}
	}
}
