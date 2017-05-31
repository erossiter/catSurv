#include <Rcpp.h>
#include <gsl/gsl_math.h>
using namespace Rcpp;

int main () {
  
  std::cout << "hello world" << std::endl;
  std::cout << M_PI << std::endl;
  
  return 0;
}

// [[Rcpp::export]]
void rcpp_hello_world() {

    main();

}

