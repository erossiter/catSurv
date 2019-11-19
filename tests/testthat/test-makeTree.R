context("makeTree")
load("cat_objects.Rdata")

makeTree_test <- function(cat){
  set.seed(239487)
  if((cat@model == "ltm") | (cat@model == "tpm")){
    resp <- matrix(NA, nrow = cat@lengthThreshold, ncol = 2)
    for(i in 1:cat@lengthThreshold){
      resp[i,1] <- paste0("Q", selectItem(ltm_cat)$next_item)
      resp[i,2] <- sample(c(0,1), 1)
      ltm_cat@answers[selectItem(ltm_cat)$next_item] <- as.numeric(resp[i,2])
    }
  }
  if(cat@model != "ltm"){
    resp <- matrix(NA, ncol = 2, nrow = cat@lengthThreshold)
    for(i in 1:cat@lengthThreshold){
      resp[i,1] <- names(cat@discrimination)[selectItem(cat)$next_item]
      resp[i,2] <- sample(c(-1, 1:5), 1)
      cat@answers[selectItem(cat)$next_item] <- as.numeric(resp[i,2])
    }
  }
  return(resp)
}

test_that("makeTree function (flat = FALSE) for ltm cat works", {
  ltm_cat@lengthThreshold <- 3
  ltm_list <- makeTree(ltm_cat)
  test_mat <- makeTree_test(ltm_cat)
  package_ans <- ltm_list[[test_mat[1,2]]][[test_mat[2,2]]][["Next"]]

  expect_equal(package_ans, test_mat[3,1])
})

test_that("makeTree function (flat = TRUE) for ltm cat works", {
  ltm_cat@lengthThreshold <- 3
  ltm_flat <- makeTree(ltm_cat, flat = TRUE)
  test_mat <- makeTree_test(ltm_cat)
  package_ans <- ltm_flat[which(ltm_flat[,test_mat[1,1]] == as.numeric(test_mat[1,2]) &
                   ltm_flat[,test_mat[2,1]] == as.numeric(test_mat[2,2])), "NextItem"]

  expect_equal(package_ans, test_mat[3,1])
})

test_that("makeTree function (flat = FALSE) for grm cat works", {
  grm_cat@lengthThreshold <- 3
  grm_list <- makeTree(grm_cat)
  test_mat <- makeTree_test(grm_cat)
  package_ans <- grm_list[[test_mat[1,2]]][[test_mat[2,2]]][["Next"]]

  expect_equal(package_ans, test_mat[3,1])
})

test_that("makeTree function (flat = TRUE) for grm cat works", {
  grm_cat@lengthThreshold <- 3
  grm_flat <- makeTree(grm_cat, flat = TRUE)
  test_mat <- makeTree_test(grm_cat)
  package_ans <- grm_flat[which(grm_flat[,test_mat[1,1]] == as.numeric(test_mat[1,2]) &
                   grm_flat[,test_mat[2,1]] == as.numeric(test_mat[2,2])), "NextItem"]

  expect_equal(package_ans, test_mat[3,1])
})