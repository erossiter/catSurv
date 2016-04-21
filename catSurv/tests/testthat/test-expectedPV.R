expectedPV_test <- function(cat, item){
  
  if(cat@poly == FALSE){
    ## Probability they get it right
    ## and variance *if* they get it wrong
    pr_correct <- probability(cat, theta_hat, item)
    cat@answers[item] <- 1 
    theta_correct <- estimateTheta(cat)
    var_correct <- estimateSE(cat, theta_correct)^2
    
    ## Probability they get it wrong,
    ## and variance *if* they get it wrong
    pr_incorrect <- 1 - pr_correct
    cat@answers[item] <- 0 
    theta_incorrect <- estimateTheta(cat)
    var_incorrect <- estimateSE(cat, theta_incorrect)^2
    
    cat@answers[item] <- NA
    
    item_EPV <- (pr_correct * var_correct) + (pr_incorrect * var_incorrect)
  }
  
  
  if(cat@poly == TRUE){
    
    item_probabilities <- function(cat){
    ## temporarily changing cat to just have this question's info
      cat@difficulty <- cat@difficulty[[item]] #this will be a list for poly, right?
      cat@discrimination <- cat@discrimination[item]
      cat@guessing <- cat@guessing[item]
      result <- probability(cat, estimateTheta(cat))
      return(result)
    }
    
    item_thetas <- rep(NA, length(cat@difficulty[[item]]))
    item_vars <- rep(NA, length(cat@difficulty[[item]]))
    for(i in 1:length(item_thetas)){
      cat@answers <- i
      item_vars[i] <- estimateSE(cat)^2
    }
    cat@answers[item] <- NA
    
    itemEPV <- sum(item_probabilities * item_vars)
  }
  
  return(item_EPV)
}
