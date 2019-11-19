#' Clean adaptive inventory responses from Qualtrics
#'
#' This function cleans the adaptive inventory responses stored as embedded data in Qualtrics
#' 
#' @param catObj Vector containing JSON character representations of the completed Cat objects from Qualtrics survey
#' @param responseID Vector containing unique character identifiers for the respondents in the Qualtrics survey
#' 
#' @details 
#' 
#' This function cleans the adaptive inventory responses contained in the Qualtrics survey
#' results.  Because different respondents recieve different adaptive inventories, 
#' their answers to the battery are not stored as usual as separate columns in the Data & Analysis tab 
#' in the Qualtrics toolbar.  Rather, the respondents' answers to the adaptive battery are saved in 
#' the catObj embedded data object.  To access the answers, click "Export & Import", and then "Export Data."
#' In the window that appears, we recommend downloading the data as a .csv file.
#' Then, feed this function the catObj column and the responseID column.
#' 
#' 
#' @return 
#' 
#' This function returns a data frame containing cleaned adaptive inventory responses.
#' 
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' raw_df <- read.csv("qualtrics_results.csv", stringsAsFactors = FALSE)
#' 
#' # removing first two header rows containing question text and import ID
#' clean_df <- readQualtrics(raw_df$catObj[-c(1,2)], raw_df$catObj[-c(1,2)])
#' }
#'
#' @seealso
#' 
#' \code{\link{Cat-class}}
#' 
#' @author Haley Acevedo, Ryden Butler, Josh W. Cutler, Matt Malis, Jacob M. Montgomery, Tom Wilkinson, Erin Rossiter, Min Hee Seo, Alex Weil 
#' 
#' @name readQualtrics
NULL

setGeneric("readQualtrics", function(catObj, responseID){
  standardGeneric("readQualtrics")
})


#' @rdname readQualtrics
#' @export
setMethod("readQualtrics",
          signature = "character",
          definition = function(catObj, responseID){
              
              
              # these columns might be there
              if(any(c("catObj", '{"ImportId":"catObj"}') %in% catObj)){
                  stop("Remove any header information.")
              }
              if(length(catObj) != length(responseID)){
                  stop("Need a unique ID for each catObj.")
              }
              
              # get answers from JSON strings
              responses <- data.frame(responseID = responseID,
                                      plyr::adply(.data = catObj,
                                       .margins = 1,
                                       .id = NULL,
                                       .fun = function(json_cat){
                                           cat <- fromJSONCat(json_cat)
                                           cat@answers
                                       })
              )
              
              # append to data
              colnames(responses) <- c("responseID", fromJSONCat(catObj[1])@ids)
              return(responses)
})


