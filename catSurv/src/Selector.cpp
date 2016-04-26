#include "Selector.h"

Selector::Selector(QuestionSet &questions, Estimator &estimation, Prior &priorModel)
		: questionSet(questions), estimator(estimation), prior(priorModel) {}