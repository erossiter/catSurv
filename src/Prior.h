#pragma once
#include <string>
#include <vector>
#include <Rcpp.h>

/**
  * Represents the prior model used in other calculations.
  */
class Prior {
private:
  double dt(double x, int df, double mu);
  double uniform(double x, double min, double max);
  double normal(double x, double mean, double sd);
  
public:
  const std::string name;
  const std::vector<double> parameters;
  
  double prior(double x);
  
  Prior(Rcpp::S4 cat_df);
  
  Prior(const std::string &prior_name, const std::vector<double> &distribution_parameters);
};

