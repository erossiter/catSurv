' Qualtrics AJAX Handler
#'
#' Qualtrics AJAX Handler
#'
#' @param catObj An object of class \code{character}.
#' @param item An integer indicating the index of the question item
#' @param answer The answer to the \code{item} to be updated
#'
#' @details Blah
#' @return Blah
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
    lastThing <- F
    
    if (item == -1) {
      item <- selectItem(catObj)$next_item
      firstThing <- T
    }

    if (sum(as.numeric(is.na(catObj@answers))) == 1) {
      lastThing <- T
    }
    
    validObject(catObj)
    nexts <- NULL
    if (!checkStopRules(catObj)) {
        nexts <- as.list(lookAhead(catObj, item))
        
        # ### Begin handle the skipped item response -- delete when `lookAhead` is fixed
        # fakecat <- catObj
        # fakecat@answers[item] <- -1
        # nullNext <- selectItem(fakecat)$next_item
        # 
        # nexts$response_option <- c(-1, nexts$response_option)
        # nexts$next_item <- c(nullNext, nexts$next_item)
        # ### End handle the skipped item response
        
        nexts$newCat <- toJSONCat(catObj)
        if (firstThing) {
          nexts$firstThing <- item
        }
        if (lastThing) {
          nexts$lastThing <- 1
        }
    } else {
        nexts <- list(all = "NULL")
    }
    return(nexts)
})
