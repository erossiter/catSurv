# data("npi")
# binary_data <- npi[1:100,]
# binary_cat <- ltmCat(binary_data)
# binary_cat@lengthThreshold <- 5
# flattenTree(binary_cat)
# 
# data("nfc")
# poly_data <- nfc[1:100,]
# poly_cat <- grmCat(poly_data)
# poly_cat@lengthThreshold <- 2
# flattenTree(poly_cat)
# 
# flattenTree <- function(cat) {
#   
#   tree <- makeTree(cat)
#   flatTree <- unlist(tree)
#   names(flatTree) <- sub("Next", "", names(flatTree))
#   flatTree <- flatTree[order(nchar(names(flatTree)))]
#   
#   if(cat@poly == FALSE){
#     for(i in 1:length(flatTree)){
#       if(substring(names(flatTree[i]), 1,1) == 1){
#         flatTree <- c(flatTree, flatTree[i])
#       } 
#     }
#     remove <- which(substring(names(flatTree), 1,1) == 1)
#     remove <- remove[1:(length(remove)/2)]
#     flatTree <- flatTree[-c(remove)]
#   } else {
#   answerChoices <- length(unique(substring(names(flatTree), 1,1))) - 1
#   orderedTree <- flatTree[1]
#   for(i in 1:answerChoices){
#     answers <- rep(NA, (length(orderedTree)-1)/answerChoices)
#     answers <- flatTree[substring(names(flatTree), 1,1) == i]
#     orderedTree <- c(orderedTree, answers)
#   }
#   flatTree <- orderedTree
#   }
#   output <- matrix(data = NA, nrow = length(flatTree), ncol = length(cat@answers) + 1)
#   colnames(output) <- c(names(cat@difficulty), "NextItem")
#   
#   for(i in 1:length(flatTree)){
#     output[i,ncol(output)] <- flatTree[i]
#     if(i > 1){
#       if(nchar(names(flatTree[i])) == 2){
#         output[i, output[1,ncol(output)]] <- substring(names(flatTree[i]), 
#                                                        nchar(names(flatTree[i]))-1, 
#                                                        nchar(names(flatTree[i]))-1)
#       } else {
#         for(j in 1:((nchar(names(flatTree[i]))/2)-1)){
#           output[i, flatTree[substring(names(flatTree[i]), 1, nchar(names(flatTree[i]))-2*j)]] <- substring(names(flatTree[i]), 
#                                                                                                             nchar(names(flatTree[i]))-((j*2) - 1), 
#                                                                                                             nchar(names(flatTree[i]))-((j*2) - 1))
#         }
#         output[i, output[1,ncol(output)]] <- substring(names(flatTree[i]), 1,1)
#       }
#     }
#   }
#   output <- as.table(as.matrix(output))
#   return(output)
# }