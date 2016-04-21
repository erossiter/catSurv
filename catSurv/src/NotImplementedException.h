#include <string>
#include <exception>

#pragma once


class NotImplementedException : public std::exception {

public:

	NotImplementedException(const std::string &error = "Functionality not yet implemented!");

	const char * what() const _NOEXCEPT;

private:
	std::string errorMessage;
};