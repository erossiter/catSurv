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
        std::string sm, double cov, int pts);
  
  std::string priorEnumToString(){
    return (prior_name == NORMAL) ? "normal" : "student_t";
  }
};

Cat constructCppCat(S4 cat_df);
