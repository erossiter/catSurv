testCats <- c(catBiCreator(6), catPolyCreator(6))
  

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


testCats[[1]]@estimation <- "MAP"
testCats[[3]]@estimation <- "MAP"
testCats[[5]]@estimation <- "MAP"
testCats[[7]]@estimation <- "MAP"
testCats[[9]]@estimation <- "MAP"
testCats[[11]]@estimation <- "MAP"


