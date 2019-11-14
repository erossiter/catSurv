#' Qualtrics AJAX Handler
#'
#' Qualtrics AJAX Handler used to implement catSurv functionality in a Qualtrics survey
#'
#' @param catObj An object of class \code{Cat}.
#' @param item An integer indicating the index of the question item
#'
#' @details This function is not intended for researcher use, rather it is a public
#' facing function of the package because it is used by catSurv
#' to integrate computerized adaptive testing into a Qualtrics survey.
#'  
#' @author Joshua Landman 
#' 
#' @name processAJAX
NULL

setGeneric("processAJAX", function(catObj, item) standardGeneric("processAJAX"))


#' @rdname processAJAX
#' @export
setMethod(f = "processAJAX", signature = "character", definition = function(catObj, item){
    catObj <- fromJSONCat(catObj)
    firstThing <- F
    lastItem <- F
    
    if (item == -1) {
      item <- selectItem(catObj)$next_item
      firstThing <- T
    }

    if ((catObj@lengthThreshold - sum(as.numeric(!is.na(catObj@answers)))) == 1) {
      lastItem <- T
    }
    
    validObject(catObj)
    nexts <- NULL
    
    if (!checkStopRules(catObj)) {
        nexts <- as.list(lookAhead(catObj, item))
        nexts$newCat <- toJSONCat(catObj)

        if (firstThing) {
          nexts$firstThing <- item
        }
        if (lastItem) {
          nexts$lastItem <- 1
        }
    } else {
        nexts <- list(all = "NULL")
    }

    return(nexts)
})
