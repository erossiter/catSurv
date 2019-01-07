#' Estimates theta with multiple specifications
#'
#' Takes response profiles from multiple respondents and multiple Cat objects and returns a set of theta estimates
#'
#' @param respondents A matrix of response profiles
#' @param catObjs A list of \code{Cat} objects of the same class (i.e., LTM, TPM, GRM, GPCM) specifying different estimation routines
#'
#'
#' @details The function takes a \code{Cat} object and generates an estimation for \code{theta} using \code{MAP}, \code{EAP}, etc. 
#' The user must store all \code{Cat} objects in a single list.
#' 
#' @return The function \code{allEst} returns a vector.  Each \code{Cat} object returns a column and each respondent returns a row.
#' 
#' 
#' This function is to allow users to look at data computed before \code{Cat} technologies were availabe and and analyze multpile types of analyses on mulptile resondents to achieve a theta estimate.
#' 
#' @seealso \code{\link{Cat-class}}, \code{\link{apply}}, \code{\link{selectItem}}, \code{\link{store}}
#' 
#' @examples 
#' 
#'  ## Loading ltm Cat object
#' data(grm_cat)
#'    
#' ## load sample data
#'  
#' respondents<-apply(thetaValue[1:3], 2, simulateRespondents, cat=grm_cat, n=10)
#' 
#' #Choose your \code{cat} object and the types of estimation you would like to use
#'  grm_MAP<-grm_cat
#'  grm_MAP@estimation<-"MAP"
#'  grm_EAP<-grm_cat
#'  grm_EAP@estimation<-"EAP"
#' 
#' 
#' grmList<-list(grm_MAP, grm_EAP)
#' ## Create a threshold for the estimation
#' 
#' ## You must set some sort of CheckStopRule
#' grm_cat@lengthThreshold<-4
#' 
#' ## Run 
#' 
#' allEst(catObjs=grmList, resp=respondents)
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil, Jaerin Kim, Dominique Lockett 
#' 
#' @rdname allEst
#' 
#' @export
allEst <- function(catObjs, resp){
    UseMethod("allEst", catObj)
}

allEst<- function(catObjs=list(), resp){
    #the multiple cat objects must be of the same class and be in list type
    #first do for loop to go over our stopping rules having trouble creating more efficient code
    store<-NULL
    obj<-NULL
    for (i in 1:length(catObjs)){
        #create a dummy object so we can collect the model type of every catObj
        obj<-c(obj, catObjs[[i]]@model)
        
        if (length(levels(as.factor(obj)))!=1){
            #if there are more than one types of catObj (ltm, grm), stop
            
            stop("List of Cat objects must be of the same type*")
        }
        if (length(resp) != length(catObjs[[i]]@answers)){
            #make sure answer profile is appropriate for catObj grm@answers = 18 so make sure battery has 18 questions
            
            stop("Response profile is not compatible with Cat object.")
        }
        
        #there are 2 selection types which do not work if the first answer is not chosen
        #for these we will resort back to the default "EPV" and switch back after the first question is picked
        if(catObjs[[i]]@selection=="KL" | catObjs[[i]]@selection=="MFII"){
            #lots going on here; go over each respondent and return  each theta value
            #we do this adaptively by selecting the next question  and grabbing it from the answer profile
            
            store<-(rbind(store,apply(resp,1,  function(catObj, answers=c()){
                #throw a tryCatch in here because there are troubles with MAP
                
                theta_est <- tryCatch({   
                    
                    #Bc they cant generate 1st answer we resort to EPV 
                    orig<-catObjs[[i]]@selection
                    tempcatObj<-catObjs[[i]]
                    setSelection(tempcatObj) <-"EPV"
                    #here we pick the first item that "EPV" chooses
                    while ( answers[selectItem(tempcatObj)$next_item]== -1){
                        tempcatObj<-storeAnswer(catObj=tempcatObj,item=selectItem(tempcatObj)$next_item, answers[selectItem(tempcatObj)$next_item])
                    }
                    tempcatObj<-storeAnswer(catObj=tempcatObj,item=selectItem(tempcatObj)$next_item, answers[selectItem(tempcatObj)$next_item])
                    # after first question we flip back to KL and continue as usual
                    
                    setSelection(tempcatObj) <-orig
                    cat<-tempcatObj
                    # Look to checkStopRules and identify the question selection and grab the corresponding answer
                    while (checkStopRules(cat)==F) {
                        cat<-storeAnswer(cat, item=selectItem(cat)$next_item, answers[selectItem(cat)$next_item])}
                    
                    #used estimateTheta here probably could have went with estimateThetas, idk
                    estimateTheta(cat)
                }, 
                
                #back to tryCatch there are lots of potential errors so we just nip it in the bud and return an error message and NA's
                error = function(cond) {
                    message(cond)
                    return(NA)
                }
                )
                return(theta_est)
            }, catObj=catObjs[[i]])))
            
            
        } else
            #then we do it all again with the functions that can pick the first question
            
            store<-(rbind(store,apply(resp,1,  function(catObj, answers=c()){
                theta_est <- tryCatch(
                    {
                        cat<-storeAnswer(catObj,item=selectItem(catObj)$next_item, answers[selectItem(catObj)$next_item])
                        while (checkStopRules(cat)==F) {
                            cat<-storeAnswer(cat, item=selectItem(cat)$next_item, answers[selectItem(cat)$next_item])
                        }
                        estimateTheta(cat)
                    }, 
                    error = function(cond) {
                        message(cond)
                        return(NA)
                    })
                return(theta_est)
            }, catObj=catObjs[[i]])))
    }
    store<-t(store)
    colnames(store)<- paste("catObj", 1:ncol(store))
    return(store)
}