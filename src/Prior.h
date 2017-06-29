#pragma once
#include <string>
#include <vector>
#include <Rcpp.h>
#include <array>

/**
  * Represents the prior model used in other calculations.
  */
class Prior {
private:
  double dt(double x, double mu, double df) const;
  double uniform(double x, double min, double max) const;
  double normal(double x, double mean, double sd) const;

  void set_pdf_function(const std::string& name);

  typedef double (Prior::*pdf_ptr_t)(double,double,double) const;
  pdf_ptr_t pdf_ptr;

  //std::string name; //If at all needed, use enum/int representing the name
  std::array<double, 2> parameters;

public:
  double param0() const
  {
    return parameters[0];
  }
  double param1() const
  {
    return parameters[1];
  }
  double prior(double x) const;
  
  Prior(Rcpp::S4 cat_df);
  
  Prior(const std::string &prior_name, const std::vector<double> &distribution_parameters);
};

