#include "NotImplementedException.h"

NotImplementedException::NotImplementedException(const std::string &error) : errorMessage(error) { };

const char *NotImplementedException::what() const _NOEXCEPT {
	return errorMessage.c_str();
}