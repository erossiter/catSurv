#pragma once
#include "Integrator.h"

class TrapezoidIntegrator : public Integrator {

public:

	virtual const double integrate(std::vector<double> &x, std::vector<double> &fx) const;
};