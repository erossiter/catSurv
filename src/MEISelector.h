#pragma once


#include "Selector.h"

class MEISelector : public Selector {

public:
	MEISelector(QuestionSet &questions, Estimator &estimation, Prior &priorModel);
  
  ~MEISelector() {};

	virtual SelectionType getSelectionType();

	virtual Selection selectItem();
};

