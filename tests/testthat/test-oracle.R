# context("estimateThetas")
# load("cat_objects.Rdata")
# 
# 
# oracle_test <- function(catObj, theta, ans_profile, n){
#   ## add more checks here, like correct type of answers?
#   if(length(x) != length(catObj@answers)){
#     stop("Response profile is not compatible with Cat object.")
#   }
#   
#   ## 10 million for now... not sure what a good cut off is
#   ncombos <- choose(length(x), n)
#   if(ncombos > 10000000){
#     stop("Too many combinations result from choose(length(x), n).")
#   }
#   
#   ## matrix of all length n combinations of indexes
#   ## where each column is a possible combo
#   combo_mat <- combn(1:length(x), n)
#   
#   ans_profiles <- matrix(nrow = ncol(combo_mat),
#                          ncol = (length(catObj@answers)+1),
#                          dimnames = list(1:ncol(combo_mat),
#                                          c(paste0("q", 1:length(catObj@answers)), "theta_est")))
#   for(i in 1:ncol(combo_mat)){
#     ## now use indexes to get actual answer profile
#     indexes <- combo_mat[ ,i]
#     ans_profiles[i, indexes] <- as.numeric(x[indexes])
#     
#     catObj@answers <- ans_profiles[i, 1:(ncol(ans_profiles)-1)]
#     ans_profiles[i, "theta_est"] <- estimateTheta(catObj)
#   }
#   
#   return_row <- which.min(abs(ans_profiles[,"theta_est"] - theta))
#   
#   return(list("true_theta" = theta,
#               "theta_est" = ans_profiles[return_row, "theta_est"],
#               "ans_profile" = ans_profiles[return_row, -ncol(ans_profiles)]))
# }
# 
# 
# ## GRM: A check with first respondent's answer profile
# grm_cat@answers <- as.numeric(nfc[1,])
# true_theta <- estimateTheta(grm_cat)
# grm_cat@answers <- rep(NA, length(grm_cat@answers))
# 
# ## oracle depends on response profiles
# grm_example <- oracle_test(grm_cat, theta = true_theta, x = nfc[1,], n = 15)
# grm_example$true_theta
# grm_example$theta_est
# grm_example$ans_profile
# 
# 
# 
# test_that("GRM returns correct output", {
# })
# 
# test_that("...", {
# })
# 
# test_that("Errors are thrown due to bad input", {
# })
