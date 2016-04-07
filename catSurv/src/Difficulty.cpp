#include "Difficulty.h"

const bool Difficulty::is_polytomous() const {
	return polytomous;
}

const std::vector<double> &Difficulty::get_difficulty(size_t index) const {
	return parameters[index];
}