cmake_minimum_required(VERSION 3.5)
project(CAT_Survey)

set(RCPPGSL_PATH "/usr/local/lib/R/3.2/site-library/RcppGSL/include")
set(RCPP_PATH "/usr/local/lib/R/3.2/site-library/Rcpp/include")
set(RINSIDE_PATH "/usr/local/lib/R/3.2/site-library/RInside/include")

set(WARNINGS "-Werror -Wall -Wextra -pedantic -Wcast-align -Wcast-qual -Wctor-dtor-privacy -Wdisabled-optimization -Wformat=2 -Winit-self -Wmissing-include-dirs -Woverloaded-virtual -Wredundant-decls -Wshadow -Wsign-promo -Wstrict-overflow=5 -Wundef -Wno-unused -Wno-variadic-macros -Wno-parentheses -fdiagnostics-show-option")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 ${WARNINGS} -isystem ${RCPP_PATH} -isystem ${RCPPGSL_PATH} -isystem${RINSIDE_PATH}")

set(SOURCE_FILES
		Cat.h
		Cat.cpp
		main.cpp Prior.h Prior.cpp
		Integrator.h Estimator.h Estimator.cpp
		EAPEstimator.cpp EAPEstimator.h
		QuestionSet.h Integrator.cpp MAPEstimator.cpp MAPEstimator.h NotImplementedException.cpp NotImplementedException.h)

add_library(catSurv STATIC ${SOURCE_FILES})
add_executable(run main.cpp)