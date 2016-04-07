#pragma once
#include <string>
#include <vector>

class Difficulty {
private:
	const std::vector<std::vector<double>> parameters;
	const bool polytomous;
public:
	const bool is_polytomous() const;

	const std::vector<double> &get_difficulty(size_t index) const;
};