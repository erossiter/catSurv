#' Convert JSON/Character object to Cat object and calls SelectItem
#'
#'
#' @param jsonObj A \code{json} or character object with elements matching Cat object slots
#'
#' @return Results from \code{selectItem}
#' 
#' @seealso 
#' 
#' \code{\link{Cat-class}}
#' 
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil
#' 
#'
#' @rdname selectItem_qualtrics
#' 
#' @name selectItem_qualtrics


setGeneric("selectItem_qualtrics", function(jsonObj){
  standardGeneric("selectItem_qualtrics")
})

#' @rdname fromJSONCat
#' @export
setMethod("selectItem_qualtrics",
          signature(jsonObj = "character"),
          function(jsonObj){
            cat <- fromJSONCat(jsonObj)
            return(selectItem(cat))
})

