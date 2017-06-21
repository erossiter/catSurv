#pragma once

#include <vector>
#include <string>

struct Selection {
	std::vector<int> questions;
	std::vector<double> values;
	std::string name;
	int item;
	std::vector<std::string> question_names;
};