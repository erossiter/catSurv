#include "Selector.h"

/**
 * An abstract class that represents the various ways of selecting the next question.
 */
Selector::Selector(QuestionSet &questions, Estimator &estimation, Prior &priorModel)
		: questionSet(questions), estimator(estimation), prior(priorModel) {}
