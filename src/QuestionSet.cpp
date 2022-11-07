#include "QuestionSet.h"

QuestionSet::QuestionSet(Rcpp::S4 &cat_df) {
	answers = Rcpp::as<std::vector<int> >(cat_df.slot("answers"));
	guessing = Rcpp::as<std::vector<double> >(cat_df.slot("guessing"));
	discrimination = Rcpp::as<std::vector<double> >(cat_df.slot("discrimination"));
	
	z = Rcpp::as<std::vector<double> >(cat_df.slot("z"));
	z[0] = R::qnorm(z.at(0), 0.0, 1.0, 1, 0);
	
	
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
		Rcpp::IntegerVector col = responses.at(i); 
		answers.at(i) = col.at(row);
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
	if(answer == answers.at(question))
	{
		return; // nothing to be done
	}

	int old_answer = answers.at(question);
	answers.at(question) = answer;

	
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
    
    std::vector<bool> minAnswer_posDiscrim;
    std::vector<bool> maxAnswer_posDiscrim;
    std::vector<bool> minAnswer_negDiscrim;
    std::vector<bool> maxAnswer_negDiscrim;
    
    int min_response = ((model == "ltm") or (model == "tpm")) ? 0.0 : 1.0;
    
    
    for (auto i : applicable_rows) {
        int max_response = ((model == "ltm") or (model == "tpm")) ? 1.0 : difficulty.at(i).size() + 1.0;
        
        if (discrimination.at(i) < 0.0 and answers.at(i) == min_response){
            minAnswer_negDiscrim.push_back(true); 
        } else {
            minAnswer_negDiscrim.push_back(false); 
        }
        
        if (discrimination.at(i) < 0.0 and answers.at(i) == max_response) {
            maxAnswer_negDiscrim.push_back(true);
        } else {
            maxAnswer_negDiscrim.push_back(false);
        }
        
        
        if (discrimination.at(i) > 0.0 and answers.at(i) == min_response) {
            minAnswer_posDiscrim.push_back(true);
        } else {
            minAnswer_posDiscrim.push_back(false);
        }
        
        
        if (discrimination.at(i) > 0.0 and answers.at(i) == max_response) {
            maxAnswer_posDiscrim.push_back(true);
        } else {
            maxAnswer_posDiscrim.push_back(false);
        }
    }
    
    size_t sum_1 = 0;
    for (auto i : minAnswer_negDiscrim) sum_1 += i;
    for (auto i : maxAnswer_posDiscrim) sum_1 += i;
    
    size_t sum_2 = 0;
    for (auto i : minAnswer_posDiscrim) sum_2 += i;
    for (auto i : maxAnswer_negDiscrim) sum_2 += i;
    
    if ((sum_1 == applicable_rows.size()) or (sum_2 == applicable_rows.size())){
        all_extreme = true;
    } else {
        all_extreme = false;
    }
    
}
