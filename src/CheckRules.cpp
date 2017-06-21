#include "CheckRules.h"
#include <math.h>


CheckRules::CheckRules(Rcpp::S4 &cat_df) {
  lengthThreshold = Rcpp::as<double> (cat_df.slot("lengthThreshold"));
  seThreshold = Rcpp::as<double> (cat_df.slot("seThreshold"));
  infoThreshold = Rcpp::as<double> (cat_df.slot("infoThreshold"));
  gainThreshold = Rcpp::as<double> (cat_df.slot("gainThreshold"));
  lengthOverride = Rcpp::as<double> (cat_df.slot("lengthOverride"));
  gainOverride = Rcpp::as<double> (cat_df.slot("gainOverride"));
}

  





