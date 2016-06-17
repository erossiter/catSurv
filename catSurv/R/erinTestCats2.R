testCats <- vector("list", 8)
testCats[[1]] <- new("Cat", poly = F, priorName = "NORMAL", estimation = "MAP",
                     guessing = c(.1,.2,.1,0,0,0,.5,0,0,0),
                     discrimination = c(1, -1, 1, 1, -1, 1, 1, 1, -1, 1),
                     difficulty = c(1, -1, 1, 1, -1, 1, 1, 1, -1, 1),
                     answers = c(1,1,0,0,0,1,1,NA, NA, NA))

testCats[[2]] <- new("Cat", poly = F, priorName = "NORMAL", estimation = "EAP",
                     priorParams = c(0,10000000),
                     guessing = rep(.1, 10),
                     discrimination = rnorm(10),
                     difficulty = rnorm(10),
                     answers = c(1,0,1,1,0,0,NA,NA,NA,NA))

testCats[[3]] <- new("Cat", poly = F, priorName = "STUDENT_T", estimation = "MAP",
                     guessing = rep(0, 10),
                     discrimination = abs(rnorm(10)),
                     difficulty = abs(rnorm(10)),
                     answers = c(0,1,1,1,0,NA,NA,NA,NA,NA))

testCats[[4]] <- new("Cat", poly = F, priorName = "STUDENT_T", estimation = "EAP",
                     guessing = rep(0, 10),
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
                     guessing = c(0,1,-1,0,0,1,1,-1,0,1),
                     discrimination = rep(c(-1,1), 5),
                     difficulty = list(c(-2, -1, 1, 2.5),c(-3, 1, 1.5, 2),c(-2, -1, 1, 2.5),c(-2, -1, 1, 2.5),c(-2, -1, 1, 2.5),c(-2, -1, 1, 2.5),c(-2, -1, 1, 2.5),c(-2, -1, 1, 2.5),c(-2, -1, 1, 2.5),c(-2, -1, 1, 2.5)),
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

