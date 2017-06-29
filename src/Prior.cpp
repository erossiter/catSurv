#include <gsl/gsl_randist.h>
#include <cmath>
#include "Prior.h"

using namespace std;

double Prior::dt(double x, double mu, double df) const{
  // https://www.gnu.org/software/gsl/manual/html_node/The-t_002ddistribution.html
  return gsl_ran_tdist_pdf(x-mu,df);
}

double Prior::uniform(double x, double min, double max) const{
  return gsl_ran_flat_pdf(x,min,max);
}

double Prior::normal(double x, double mean, double sd) const{
  return gsl_ran_gaussian_pdf(x-mean, sd);
}

double Prior::prior(double x) const{
  return (this->*pdf_ptr)(x, parameters.at(0), parameters.at(1));
}

void Prior::set_pdf_function(const std::string& name)
{
  if(name == "STUDENT_T")
  {
    pdf_ptr = &Prior::dt;
  }
  else if (name == "UNIFORM")
  {
    pdf_ptr = &Prior::uniform;
  }
  else if (name == "NORMAL")
  {
    pdf_ptr = &Prior::normal;
  }
  else
  {  
    Rcpp::stop("%s is not a valid prior name.", name);
    throw std::invalid_argument("Invalid prior name");
  }
}

Prior::Prior(Rcpp::S4 cat_df)
{
  set_pdf_function(Rcpp::as<std::string>(cat_df.slot("priorName")));

  auto distribution_parameters = Rcpp::as<std::vector<double> >(cat_df.slot("priorParams"));
  parameters[0] = distribution_parameters[0];
  parameters[1] = distribution_parameters[1];
  
}

Prior::Prior(const std::string &prior_name, const std::vector<double> &distribution_parameters)
{
  set_pdf_function(prior_name);

  parameters[0] = distribution_parameters[0];
  parameters[1] = distribution_parameters[1];
}