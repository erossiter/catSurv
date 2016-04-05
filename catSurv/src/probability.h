#pragma once

#include <vector>
#include "Cat.h"

void probability(Cat &cat, double theta, int question, std::vector<double> &ret_prob);

double probability(Cat &cat, double theta, int question);

List probability(S4 cat_df, NumericVector t, IntegerVector q);