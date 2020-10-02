#' Find Answer Profile that Minimizes Bias
#'
#' Generating all possible combinations of length n from a response profile to determine
#' the possible response profile best able to estimate the true value of theta.
#'
#' @param catObj An object of class \code{Cat}
#' @param theta A numeric representing the true position on the latent trait.
#' @param responses A vector representing the respondent's full answer profile.
#' @param approx If TRUE, uses a subset of 1000 potential response profiles
#' @param parallel If TRUE, computes in parallel
#'
#' @details lengthThreshold slot should specify how many questions to ask.
#' Note this function uses the estimateTheta method specified in the supplied cat object
#' 
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

setGeneric("oracle", function(catObj, theta, responses, approx = FALSE, parallel = FALSE) standardGeneric("oracle"))

#' @rdname oracle
#' @export
setMethod(f = "oracle", signature = "Cat", definition = function(catObj, theta, responses, approx = FALSE, parallel = FALSE){
    
    # TODO: generalize using checkStopRules
    n <- catObj@lengthThreshold
    
    if(length(theta) != nrow(responses)){
        stop("Need a corresponding theta value for each answer profile.")
    }
    
    if(ncol(responses) != length(catObj@answers)){
        stop("Response profiles are not compatible with Cat object.")
    }
    
    ncombos <- choose(ncol(responses), n)
    if(ncombos > 1000000){
        stop("Too many combinations result from choose(nrow(responses), n).")
    }
    
    if(n > 5){
        warning("Asking n>5 questions will provide estimate likely arbitrarily close to truth.")
    }
  
    ## matrix of all length n combinations of ~indexes~ where each row is a possible combo
    ## will be the same combo_mat applied to each specific answer profile
    combo_mat <- t(combn(1:ncol(responses), n))
    if(approx & nrow(combo_mat) > 1000){
        combo_mat <- combo_mat[sample(x = 1:nrow(combo_mat), size = 1000, replace = FALSE), ]
    }
    
    ## results for one profile with respective true theta
    find_truth <- function(ind_theta, ind_ans, combo_mat, cat){
        combo_profiles <- adply(.data = combo_mat,
                                .margins = 1,
                                .id = NULL,
                                .parallel = parallel,
                                .fun = function(indices, catObj, ans){
                                  catObj@answers[indices] <- unlist(ans[indices])
                                  theta_est <- estimateTheta(catObj)
                                  return(data.frame(theta = ind_theta, theta_est = theta_est, ans[indices]))
                                },
                                catObj = cat,
                                ans = ind_ans)
        
        return_row <- which.min(abs(combo_profiles$theta_est - ind_theta))
        return(combo_profiles[return_row, ])
    }
    
    ## results for each profile
    out <- adply(.data = 1:nrow(responses),
                 .margins = 1,
                 .id = NULL,
                 .parallel = parallel,
                 .fun = function(x) find_truth(ind_theta = theta[x], ind_ans = responses[x,], combo_mat = combo_mat, cat = catObj))
    return(out)
})


