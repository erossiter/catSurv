# context("makeTree")
# load("cat_objects.Rdata")
# 
# makeTree_test <- function(cat){
#   if(cat@model == "ltm"){
#     resp <- matrix(NA, nrow = 6, ncol = 2)
#     for(i in 1:4){
#       resp[i,1] <- paste0("Q", selectItem(ltm_cat)$next_item)
#       resp[i,2] <- sample(c(0,1), 1, T)
#       ltm_cat@answers[selectItem(ltm_cat)$next_item] <- as.numeric(resp[i,2])
#     }
#   }
#   if(cat@model != "ltm"){
#     resp <- matrix(NA, ncol = 2, nrow = 4)
#     for(i in 1:4){
#       resp[i,1] <- names(cat@discrimination)[selectItem(cat)$next_item]
#       resp[i,2] <- sample(c(-1, 1:5), 1, T)
#       cat@answers[selectItem(cat)$next_item] <- as.numeric(resp[i,2])
#     }
#   }
#   return(resp)
# }
# 
# test_that("makeTree function (flat = FALSE) for ltm cat works", {
#   ltm_cat@lengthThreshold <- 3
#   ltm_list <- makeTree(ltm_cat)
#   test_mat <- makeTree_test(ltm_cat)
#   package_ans <- ltm_list[[test_mat[1,2]]][[test_mat[2,2]]][[test_mat[3,2]]]$Next
#   
#   expect_equal(package_ans, test_mat[4,1])
# })
# 
# test_that("makeTree function (flat = TRUE) for ltm cat works", {
#   ltm_cat@lengthThreshold <- 3
#   ltm_flat <- makeTree(ltm_cat, flat = TRUE)
#   test_mat <- makeTree_test(ltm_cat)
#   package_ans <- ltm_flat[which(ltm_flat[,test_mat[1,1]] == as.numeric(test_mat[1,2]) &
#                    ltm_flat[,test_mat[2,1]] == as.numeric(test_mat[2,2]) &
#                    ltm_flat[,test_mat[3,1]] == as.numeric(test_mat[3,2])), "NextItem"]
#   
#   expect_equal(package_ans, test_mat[4,1])
# })
# 
# test_that("makeTree function (flat = FALSE) for grm cat works", {
#   grm_cat@lengthThreshold <- 3
#   grm_list <- makeTree(grm_cat)
#   test_mat <- makeTree_test(grm_cat)
#   package_ans <- grm_list[[test_mat[1,2]]][[test_mat[2,2]]][[test_mat[3,2]]]$Next
#   
#   expect_equal(package_ans, test_mat[4,1])
# })
# 
# test_that("makeTree function (flat = TRUE) for grm cat works", {
#   grm_cat@lengthThreshold <- 3
#   grm_flat <- makeTree(grm_cat, flat = TRUE)
#   test_mat <- makeTree_test(grm_cat)
#   package_ans <- grm_flat[which(grm_flat[,test_mat[1,1]] == as.numeric(test_mat[1,2]) &
#                    grm_flat[,test_mat[2,1]] == as.numeric(test_mat[2,2]) &
#                    grm_flat[,test_mat[3,1]] == as.numeric(test_mat[3,2])), "NextItem"]
#   
#   expect_equal(package_ans, test_mat[4,1])
# })
# 
# test_that("makeTree function (flat = FALSE) for gpcm cat works", {
#   gpcm_cat@lengthThreshold <- 3
#   gpcm_list <- makeTree(gpcm_cat)
#   test_mat <- makeTree_test(gpcm_cat)
#   package_ans <- gpcm_list[[test_mat[1,2]]][[test_mat[2,2]]][[test_mat[3,2]]]$Next
#   
#   expect_equal(package_ans, test_mat[4,1])
# })
# 
# test_that("makeTree function (flat = TRUE) for gpcm cat works", {
#   gpcm_cat@lengthThreshold <- 3
#   gpcm_flat <- makeTree(gpcm_cat, flat = TRUE)
#   test_mat <- makeTree_test(gpcm_cat)
#   package_ans <- gpcm_flat[which(gpcm_flat[,test_mat[1,1]] == as.numeric(test_mat[1,2]) &
#                    gpcm_flat[,test_mat[2,1]] == as.numeric(test_mat[2,2]) &
#                    gpcm_flat[,test_mat[3,1]] == as.numeric(test_mat[3,2])), "NextItem"]
#   
#   expect_equal(package_ans, test_mat[4,1])
# })
