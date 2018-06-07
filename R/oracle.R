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

setGeneric("oracle", function(catObj, theta, ans_profiles, n) standardGeneric("oracle"))

#' @rdname oracle
#' @export
setMethod(f = "oracle", signature = "Cat", definition = function(catObj, theta, ans_profiles, n){
    ## matrix of answer profiles
    ## vector of thetas
    ## same 'n'
    ## warning if n>5 or something...
  
  if(length(theta) != nrow(ans_profiles)){
    stop("Need a corresponding theta value for each answe profile.")
  }

  if(length(ans_profiles) != length(catObj@answers)){
    stop("Response profile is not compatible with Cat object.")
  }
    
  ncombos <- choose(length(ans_profiles), n)
  if(ncombos > 1000000){
    stop("Too many combinations result from choose(length(ans_profiles), n).")
  }
  
  if(n > 6){
    warning("Will likely get to be arbitrarily close to the truth....")
  }
  
  ## matrix of all length n combinations of ~indexes~
  ## where each column is a possible combo
  combo_mat <- combn(1:ncol(ans_profiles), n)
  
  ## results for one profile with true theta
  find_truth <- function(t, ans, combo_mat = combo_mat){
    # combo_profiles <- matrix(nrow = ncol(combo_mat),
    #                          ncol = (length(catObj@answers)+1),
    #                          dimnames = list(1:ncol(combo_mat),
    #                                        c(paste0("q", 1:length(catObj@answers)), "theta_est")))
    # for(i in 1:ncol(combo_mat)){
    #   ## now use indexes to get actual answer profile
    #   indexes <- combo_mat[ ,i]
    #   combo_profiles[i, indexes] <- as.numeric(ans[indexes])
    #   
    #   catObj@answers <- combo_profiles[i, 1:(ncol(combo_profiles)-1)]
    #   combo_profiles[i, "theta_est"] <- estimateTheta(catObj)
    # }
    combo_profiles <- adply(.data = combo_mat, .margins = 2, .fun = function(indices, catObj = ltm_cat, ans = ans){
      catObj@answers[indexes] <- ans[indexes]
      theta_est <-  estimateTheta(catObj)
      return(c(as.numeric(ans[indexes]), theta_est))
    })
    
    return_row <- which.min(abs(combo_profiles[,"theta_est"] - t))
    
    return(c(t,
             combo_profiles[return_row, "theta_est"],
             combo_profiles[return_row, -ncol(combo_profiles)]))
  }
  return(adply(.data = 1:nrow(ans_profiles),
               .margins = 1,
               .fun = function(x) find_truth(t = theta[x], ans = ans_profiles[x,]),
               .id = NULL))
})


