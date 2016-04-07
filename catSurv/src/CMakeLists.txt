cmake_minimum_required(VERSION 3.5)
project(CAT_Survey)

set(RCPPGSL_PATH "/usr/local/lib/R/3.2/site-library/RcppGSL/include")
set(RCPP_PATH "/usr/local/lib/R/3.2/site-library/Rcpp/include")
set(RINSIDE_PATH "/usr/local/lib/R/3.2/site-library/RInside/include")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -I${RCPP_PATH} -I${RCPPGSL_PATH} -I${RINSIDE_PATH}")

set(SOURCE_FILES
		Cat.h
		Cat.cpp
		main.cpp Prior.h Prior.cpp
		Integrator.h TrapezoidIntegrator.cpp TrapezoidIntegrator.h
		Estimator.h EAPEstimator.cpp EAPEstimator.h
		QuestionSet.h )

add_library(catSurv STATIC ${SOURCE_FILES})