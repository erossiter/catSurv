#' Find Answer Profile that Minimizes Bias
#'
#' Generating all possible combinations of length n from a response profile to determine
#' the possible response profile best able to estimate the true value of theta.
#'
#' @param catObj An object of class \code{Cat}
#' @param theta A numeric representing the true position on the latent trait.
#' @param ans_profile A vector representing the respondent's full answer profile.
#' @param n A numeric indicating the length the combinations of all subsetted answer profile should be.
#'
#' @details ....
#'
#' @return ...
#'  
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' 
#'
#' @importFrom utils combn
#'
#' 
#' @name oracle
NULL

setGeneric("oracle", function(catObj, theta, ans_profile, n) standardGeneric("oracle"))

#' @rdname oracle
#' @export
setMethod(f = "oracle", signature = "Cat", definition = function(catObj, theta, ans_profile, n){
    ## add more checks here, like correct type of answers?
    if(length(ans_profile) != length(catObj@answers)){
        stop("Response profile is not compatible with Cat object.")
    }
    
    ## 10 million for now... not sure what a good cut off is
    ncombos <- choose(length(ans_profile), n)
    if(ncombos > 10000000){
        stop("Too many combinations result from choose(length(ans_profile), n).")
    }
    
    ## matrix of all length n combinations of indexes
    ## where each column is a possible combo
    combo_mat <- combn(1:length(ans_profile), n)
    
    ans_profiles <- matrix(nrow = ncol(combo_mat),
                           ncol = (length(catObj@answers)+1),
                           dimnames = list(1:ncol(combo_mat),
                                           c(paste0("q", 1:length(catObj@answers)), "theta_est")))
    for(i in 1:ncol(combo_mat)){
        ## now use indexes to get actual answer profile
        indexes <- combo_mat[ ,i]
        ans_profiles[i, indexes] <- as.numeric(ans_profile[indexes])
        
        catObj@answers <- ans_profiles[i, 1:(ncol(ans_profiles)-1)]
        ans_profiles[i, "theta_est"] <- estimateTheta(catObj)
    }
    
    return_row <- which.min(abs(ans_profiles[,"theta_est"] - theta))
    
    return(list("true_theta" = theta,
                "theta_est" = ans_profiles[return_row, "theta_est"],
                "ans_profile" = ans_profiles[return_row, -ncol(ans_profiles)]))
})


