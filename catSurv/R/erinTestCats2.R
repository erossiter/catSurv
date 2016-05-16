testCats <- vector("list", 8)
testCats[[1]] <- new("Cat", poly = F, priorName = "NORMAL", estimation = "MAP",
                     guessing = abs(rnorm(10)),
                     discrimination = abs(rnorm(10)),
                     difficulty = abs(rnorm(10)),
                     answers = c(0,0,0,0,0,1,1,NA, NA, NA))

testCats[[2]] <- new("Cat", poly = F, priorName = "NORMAL", estimation = "EAP",
                     guessing = abs(rnorm(10)),
                     discrimination = abs(rnorm(10)),
                     difficulty = abs(rnorm(10)),
                     answers = c(1,1,1,1,1,1,1,1,NA,NA))

testCats[[3]] <- new("Cat", poly = F, priorName = "STUDENT_T", estimation = "MAP",
                     guessing = abs(rnorm(10)),
                     discrimination = abs(rnorm(10)),
                     difficulty = abs(rnorm(10)),
                     answers = c(0,1,1,1,0,NA,NA,NA,NA,NA))

testCats[[4]] <- new("Cat", poly = F, priorName = "STUDENT_T", estimation = "EAP",
                     guessing = abs(rnorm(10)),
                     discrimination = abs(rnorm(10)),
                     difficulty = abs(rnorm(10)),
                     answers = c(0,1,1,1,0,NA,NA,NA,NA,NA))


testCats[[5]] <- new("Cat", poly = T, priorName = "NORMAL", estimation = "MAP",
                     ## 4 options for each question
                     guessing = runif(10),
                     discrimination = rnorm(10),
                     difficulty = lapply(1:10, function(x) sort(rnorm(4))),
                     answers = c(1,2,3,4,4,4,3, NA, NA, NA))

testCats[[6]] <- new("Cat", poly = T, priorName = "NORMAL", estimation = "EAP",
                     ## 5 options for each question
                     guessing = runif(10),
                     discrimination = rnorm(10),
                     difficulty = lapply(1:10, function(x) sort(rnorm(5))),
                     answers = c(1,5,3,4,4,4,3,5, NA, NA))


testCats[[7]] <- new("Cat", poly = T, priorName = "STUDENT_T", estimation = "MAP",
                     ## 5 options for each question
                     guessing = runif(10),
                     discrimination = rnorm(10),
                     difficulty = lapply(1:10, function(x) sort(rnorm(5))),
                     answers = c(1,5,3,4,4,4,3,5, NA, NA))

testCats[[8]] <- new("Cat", poly = T, priorName = "STUDENT_T", estimation = "EAP",
                     ## 5 options for each question
                     guessing = runif(10),
                     discrimination = rnorm(10),
                     difficulty = lapply(1:10, function(x) sort(rnorm(5))),
                     answers = c(1,5,3,4,4,4,3,5, NA, NA))

