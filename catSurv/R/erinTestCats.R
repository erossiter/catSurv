testCats <- c(catBiCreator(6), catPolyCreator(6))

for(i in 7:12){
  answer_length <- length(testCats[[i]]@answers)
  categories <- max(testCats[[9]]@answers, na.rm = TRUE)
  testCats[[i]]@answers[1:answer_length] <- sample(1:categories, answer_length, replace = T)
}
  

testCats[[1]]@priorName <- "STUDENT_T"
testCats[[2]]@priorName <- "STUDENT_T"
testCats[[3]]@priorName <- "STUDENT_T"
testCats[[7]]@priorName <- "STUDENT_T"
testCats[[8]]@priorName <- "STUDENT_T"
testCats[[9]]@priorName <- "STUDENT_T"

for(i in 1:12){
  testCats[[i]]@priorParams[1] <- sample(1:10, 1)
  testCats[[i]]@priorParams[2] <- sample(1:10, 1)
}

testCats[[1]]@estimation <- "MAP"
testCats[[3]]@estimation <- "MAP"
testCats[[5]]@estimation <- "MAP"
testCats[[7]]@estimation <- "MAP"
testCats[[9]]@estimation <- "MAP"
testCats[[11]]@estimation <- "MAP"

for(i in 1:12){
  answer_length <- length(testCats[[i]]@answers)
  start_NA <- sample(4:answer_length-1, 1)
  testCats[[i]]@answers[start_NA:answer_length] <- NA 
}
