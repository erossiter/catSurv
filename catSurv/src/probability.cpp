#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List probability(S4 cat_df, NumericVector t, IntegerVector q){
  // convert R inputs
  Cat cat = constructCppCat(cat_df);
  double theta = as<std::vector<double> >(t)[0];
  int question = as<std::vector<int> >(q)[0];
  
  std::vector<double> probs;
  if(cat.poly){
    probability(cat, theta, question, probs);
  }
  else{
    probs.push_back(probability(cat, theta, question));
  }
  DataFrame question_probs = DataFrame::create(Named("probabilities")=probs);
  return List::create(Named("all.probabilities")=question_probs);
}


void probability(Cat& cat, double theta, int question, std::vector<double>& ret_prob) {
  unsigned int diff_size = cat.poly_difficulty[question].size();
  double D = cat.D;
  double discrimination = cat.discrimination[question];
  double guessing = cat.guessing[question];
  for (unsigned int i = 0; i < diff_size; ++i) {
    double exp_prob = exp(D * discrimination * (theta - cat.poly_difficulty[question][i]));
    ret_prob.push_back(guessing + (1 - guessing) * (exp_prob) / (1 + exp_prob));
  }
}


/* Overloaded since non-poly case needs to just return one double value, 
 * rather than a vector of doubles.
 */
double probability(Cat & cat, double theta, int question){
  double D = cat.D;
  double discrimination = cat.discrimination[question];
  double difficulty = cat.nonpoly_difficulty[question];
  double guessing = cat.guessing[question];
  double exp_prob = exp(D*discrimination * (theta - difficulty));
  return guessing + (1 - guessing) * (exp_prob / (1 + exp_prob));
}