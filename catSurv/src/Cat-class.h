#pragma once
#include <Rcpp.h>
using namespace Rcpp;

struct Cat {
  std::vector<double> guessing;
  std::vector<double> discrimination;
  std::vector<double> prior_values;
  std::vector<double> prior_params;
  std::vector<int> answers;
  double D;
  // X is the abcissa values for whatever mode of integation
  // in trapezoidal, this doesn't matter that much
  // in hermite-gauss, it should be the roots of the hermite polynomial
  std::vector<double> X;
  std::vector<double> theta_est;
  std::vector<std::vector<double> > poly_difficulty;
  std::vector<double> nonpoly_difficulty;
  std::vector<int> applicable_rows;
  std::vector<int> nonapplicable_rows;
  bool poly;
  enum IntegrationType {
    TRAPEZOID, HERMITE, QAG
  };
  enum EstimationType { 
    EAP, MAP 
  };
  enum SelectionType{
    EPV, MFI, LWI, PWI, MEI, invalid
  };
  enum priorName {
    NORMAL, STUDENT_T
  };
  IntegrationType integration_method;
  EstimationType estimation_method;
  SelectionType selection_method;
  priorName prior_name;
  double coverage;
  int points;
  
  Cat(std::vector<double> guess, std::vector<double> disc, std::vector<double> pri_v, std::string pri_n,
      std::vector<double> pri_p, std::vector<int> ans, double d, std::vector<double> x, 
      std::vector<double> t_est, std::vector<std::vector<double> > poly_diff, std::vector<double> nonpoly_diff,
      std::vector<int> app_rows, std::vector<int> nonapp_rows, bool p, std::string im, std::string em, 
      std::string sm, double cov, int pts) :
    guessing(guess), discrimination(disc), prior_values(pri_v), prior_params(pri_p),
    answers(ans), D(d), X(x), theta_est(t_est), poly_difficulty(poly_diff), nonpoly_difficulty(nonpoly_diff),
    applicable_rows(app_rows), nonapplicable_rows(nonapp_rows), poly(p), coverage(cov), points(pts)
  {
    if(im == "trapezoid"){
      integration_method = TRAPEZOID;
    }
    
    if(em == "EAP"){
      estimation_method = EAP;
    }
    else if(em == "MAP"){
      estimation_method = MAP;
    }
    
    if(sm == "EPV"){
      selection_method = EPV;
    }
    else if(sm == "MFI"){
      selection_method = MFI;
    }
    else if(sm == "LWI"){
      selection_method = LWI;
    }
    else if(sm == "PWI"){
      selection_method = PWI;
    }
    else if(sm == "MEI"){
      selection_method = MEI;
    }
    else{
      selection_method = invalid;
    }
    
    if(pri_n == "normal"){
      prior_name = NORMAL;
    }
  }
  
  std::string priorEnumToString(){
    return (prior_name == NORMAL) ? "normal" : "student_t";
  }
};



//// CONSTRUCTING A CPP CAT OBJECT, FROM AN S4 CAT OBJECT //////

Cat constructCppCat(S4 cat_df){
  std::vector<double> X = as<std::vector<double> >(cat_df.slot("X"));
  std::string priorName = as<std::string>(cat_df.slot("priorName"));
  std::vector<double> priorParams = as<std::vector<double> >(cat_df.slot("priorParams"));
  std::vector<double> prior_values;
  
  //for(unsigned int i = 0; i < X.size(); ++i){
    //prior_values.push_back(prior(X[i], priorName, priorParams));
  //}
  
  // Precalculate the rows that have been answered.
  std::vector<int> applicable_rows;
  std::vector<int> nonapplicable_rows;
  std::vector<int> answers = as<std::vector<int> >(cat_df.slot("answers"));
  for (unsigned int i = 0; i < answers.size(); i++) {
    if (answers[i] != NA_INTEGER) {
      applicable_rows.push_back(i);
    } else {
      nonapplicable_rows.push_back(i + 1);
    }
  }
  
  bool poly = as<std::vector<bool> >(cat_df.slot("poly"))[0];
  std::vector<std::vector<double> > poly_difficulty; // if poly, construct obj with vector<vector<double>> for difficulty
  std::vector<double> nonpoly_difficulty;
  if(poly){
    // Unpack the difficulty list
    List cat_difficulty = cat_df.slot("difficulty");
    for (List::iterator itr = cat_difficulty.begin(); itr != cat_difficulty.end(); ++itr) {
      poly_difficulty.push_back(as<std::vector<double> >(*itr)); // if poly, set poly_difficulty to vector<vector<double>
    }
  }
  else{
    // if non-poly, set non_poly difficulty to vector<double>
    nonpoly_difficulty = as<std::vector<double> >(cat_df.slot("difficulty"));
  }
  
  
  //Construct C++ Cat object
  Cat cat(as<std::vector<double> >(cat_df.slot("guessing")), as<std::vector<double> >(cat_df.slot("discrimination")),
          prior_values, priorName, priorParams, as<std::vector<int> >(cat_df.slot("answers")), 
          as<std::vector<double> >(cat_df.slot("D"))[0], as<std::vector<double> >(cat_df.slot("X")),
          as<std::vector<double> >(cat_df.slot("Theta.est")), poly_difficulty, nonpoly_difficulty, applicable_rows, 
          nonapplicable_rows, poly, as<std::string>(cat_df.slot("integration")), as<std::string>(cat_df.slot("estimation")),
          as<std::string>(cat_df.slot("selection")), as<std::vector<double> >(cat_df.slot("coverage"))[0], 
                                                                                                      as<std::vector<int> >(cat_df.slot("points"))[0] );
  
  return cat;
}

