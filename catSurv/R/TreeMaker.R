# data("npi")
# binary_data <- npi[1:100,]
# binary_cat <- ltmCat(binary_data)
# binary_cat@lengthThreshold <- 3
# biTree <- makeTree(binary_cat)
# 
# data("nfc")
# poly_data <- nfc[1:100,]
# poly_cat <- grmCat(poly_data)
# poly_cat@lengthThreshold <- 2
# polyTree <- makeTree(poly_cat)
# 
# makeTree <- function(cat){
#   
#   varNames <- names(cat@discrimination)
#   numPossibleAnswers <- rep(NA, length(varNames))
#   for(i in 1:length(varNames)){
#     numPossibleAnswers[i] <-  length(cat@difficulty[[i]])+2
#   }
# 
# 
#   ## This should probably be passed in.
#   numPossibleAnswers <- c(numPossibleAnswers)
#   n <- length(varNames)
#   q <- selectItem(cat)$next.item
#   this <- list()
#   for (i in 1:(numPossibleAnswers[q]-1)){
#     this[[paste(i)]] <- NA
#   }
#   this[[i+1]] <- varNames[q]
#   if(cat@poly == FALSE){
#     names(this)   <- c(1:(numPossibleAnswers[q]-1)-1, "Next")
#   } else {
#     names(this)   <- c(1:(numPossibleAnswers[q]-1), "Next")
#   }
#   #this[["-1"]]<-NA
#   this
# 
# 
#   
#   my.fun <- function(x, cat, varNames, numPossibleAnswers){
#     for(i in 1:length(x)){
#       namVar <- names(x)
#       if(is.na(x[[i]])){
#         if(sum(is.na(cat@answers))==1){
#           q <- varNames[q]
#           x[[namVar[i]]] <-  list(Next=q)
#         }
#         if(sum(is.na(cat@answers))>1 & sum(!is.na(cat@answers))<(cat@lengthThreshold-1)){
#           as.integer(names(x)[i])
#           thisQ <- which(varNames==x[["Next"]])
#           catNew <- storeAnswer(cat, thisQ, as.integer(names(x)[i]))
#           q <- selectItem(catNew)$next.item
#           for (j in 1:(numPossibleAnswers[q])){
#             x[[namVar[i]]][[j]] <- NA
#           }
#           x[[namVar[i]]][[j]] <- varNames[q]
#           if(cat@poly == FALSE){
#           names(x[[namVar[i]]])   <- c(1:(numPossibleAnswers[q]-1)-1,
#                                        #-1,
#                                        "Next")
#           } else {
#             names(x[[namVar[i]]])   <- c(1:(numPossibleAnswers[q]-1),
#                                          #-1,
#                                          "Next")
#           }          
#           x[[namVar[i]]] <- as.list(x[[namVar[i]]])
# 
#           x[[namVar[i]]] <- my.fun(x=x[[namVar[i]]],cat=catNew,
#                                   varNames=varNames, numPossibleAnswers=numPossibleAnswers)
#         }
#         if(sum(!is.na(cat@answers))>=(cat@lengthThreshold-1)){
#           thisQ <- which(varNames==x[["Next"]])
#           thisQ
#           catNew <- storeAnswer(cat, thisQ, as.integer(names(x)[i]))
#           q <-  selectItem(catNew)$next.item
#           q <- varNames[q]
#           x[[namVar[i]]] <-  list(Next=q)
#         }
#       }
#     }
#     return(x)
#   }
#   out <- my.fun(this, cat, varNames=varNames, numPossibleAnswers=numPossibleAnswers)
#   return(out)
# }
#   
