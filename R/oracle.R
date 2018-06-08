#' Find Answer Profile that Minimizes Bias
#'
#' Generating all possible combinations of length n from a response profile to determine
#' the possible response profile best able to estimate the true value of theta.
#'
#' @param catObj An object of class \code{Cat}
#' @param theta A numeric representing the true position on the latent trait.
#' @param ans_profiles A vector representing the respondent's full answer profile.
#' @param n A numeric indicating the length the combinations of all subsetted answer profile should be.
#'
#' @details ....
#'
#' @return A data.frame where the first column is the user-supplied true value of theta, the second column is the
#' best possible theta estimate given n questions are asked, and the remaining columns are the answer profile leading
#' to the best possible theta estimation.
#'  
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' 
#'
#' @importFrom utils combn
#' @importFrom plyr adply
#' 
#' @name oracle
NULL

setGeneric("oracle", function(catObj, theta, ans_profiles, n) standardGeneric("oracle"))

#' @rdname oracle
#' @export
setMethod(f = "oracle", signature = "Cat", definition = function(catObj, theta, ans_profiles, n){
    if(length(theta) != nrow(ans_profiles)){
        stop("Need a corresponding theta value for each answer profile.")
    }
    
    if(ncol(ans_profiles) != length(catObj@answers)){
        stop("Response profiles are not compatible with Cat object.")
    }
    
    ncombos <- choose(length(ans_profiles), n)
    if(ncombos > 1000000){
        stop("Too many combinations result from choose(length(ans_profiles), n).")
    }
    
    # if(n > 5){
    #     warning("Asking n>5 questions will likely be arbitrarily close...")
    # }
  
    ## matrix of all length n combinations of indexes where each row is a possible combo
    combo_mat <- t(combn(1:ncol(ans_profiles), n))
    
    ## results for one profile with respective true theta
    find_truth <- function(ind_theta, ind_ans, combo_mat, cat){
        combo_profiles <- adply(.data = combo_mat,
                                .margins = 1,
                                .id = NULL,
                                .fun = function(indices, catObj = cat, ans = ind_ans){
                                    catObj@answers[indices] <- unlist(ans[indices])
                                    theta_est <- estimateTheta(catObj)
                                    return(data.frame(theta = ind_theta, theta_est = theta_est, ans[indices]))
                                })
        
        return_row <- which.min(abs(combo_profiles$theta_est - ind_theta))
        return(combo_profiles[return_row, ])
    }
    
    ## results for each profile
    return(adply(.data = 1:nrow(ans_profiles),
                 .margins = 1,
                 .fun = function(x) find_truth(ind_theta = theta[x], ind_ans = ans_profiles[x,], combo_mat = combo_mat, cat = catObj),
                 .id = NULL))
})


