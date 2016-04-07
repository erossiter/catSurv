#pragma once
#include "Integrator.h"

class TrapezoidIntegrator : public Integrator {

public:
	virtual const IntegrationType get_integration_type() const;

	virtual const double integrate(std::vector<double> &x, std::vector<double> &fx) const;
};