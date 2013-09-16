#' The function that asks you before it tests every single function
#' 
#' This function will probably not be in the final version because it's quite time consuming, 
#' but can be useful for debugging. 
#' @param data the data (in a list) that you want to test
#' @param answer the answer you expect
#' @export

isitperm <- function(data, answer){
  require(gtools)
  fnList <- ls("package:base")
 # badfunctions <- dget("badfunctions.robj")
  listNoSideEffects <- fnList[fnList %in% badfunctions==FALSE]
  orderings <- permutations(length(data), length(data), 1:length(data))
  
  for (i in 1:dim(orderings)[1]){
    for (procName in listNoSideEffects){
      readline(prompt = paste0("hit Enter to try ", deparse(substitute(procName)), ":")) 
        print(data[orderings[i,]])
        trialAnswer <- tryCatch(
          do.call(procName, data[orderings[i,]])
          , error = function(e) as.character(e)
        )
      }
  }
  
}

