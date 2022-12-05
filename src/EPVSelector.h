#pragma once
#include "Selector.h"
#include "Selection.h"

class EPVSelector : public Selector {
public:
	virtual SelectionType getSelectionType();

	virtual Selection selectItem();

	EPVSelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);
	
	~EPVSelector() {};
	
private:
	std::string getSelectionName();
};

