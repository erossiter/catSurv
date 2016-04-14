#pragma once
#include "Integrator.h"

class TrapezoidIntegrator : public Integrator {

public:

	virtual const double integrate(const std::vector<double> &x, const std::vector<double> &fx) const override;
};