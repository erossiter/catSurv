#' Convert Cat object to JSON
#'
#' This function  object of class \code{Cat}.
#'
#' @param catObj A \code{Cat} object
#'
#'@return The function \code{toJSONCat} returns a JSON object with list elements corresponding to information stored in the \code{catObj}.
#' 
#' See \code{\link{Cat-class}} for required \code{Cat} object slots.
#' 
#' 
#' @seealso 
#' 
#' \code{\link{Cat-class}}
#' 
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil
#' 
#'
#' @rdname toJSONCat
#' 
#' @import jsonlite
#' @name toJSONCat
setOldClass("json")


setGeneric("toJSONCat", function(catObj){
  standardGeneric("toJSONCat")
})

#' @rdname toJSONCat
#' @export
setMethod("toJSONCat",
          signature(catObj = "Cat"),
          function(catObj){
            cat_list <- attributes(catObj)
            cat_list <- cat_list[1:(length(cat_list)-1)] #remove class attribute
            return(jsonlite::toJSON(cat_list))
})