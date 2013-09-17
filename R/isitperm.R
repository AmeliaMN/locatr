#' The function that asks you before it tests every single function
#' 
#' This function will probably not be in the final version because it's quite time consuming, 
#' but can be useful for debugging. 
#' 
#' It runs through all the good functions, printing the names of the 
#' functions as it goes, so you can see which function is causing the hang-up. 
#' @param data the data (in a list) that you want to test
#' @param answer the answer you expect
#' @param ask a binary TRUE/FALSE of whether you want to be prompted at the command line every time 
#' \code{isitperm} tries another function, or if it should just print each function name as it goes. 
#' @export

isitperm <- function(data, answer, ask=TRUE){
  require(gtools)
  listNoSideEffects <- goodfunctions
  orderings <- permutations(length(data), length(data), 1:length(data))
  
  for (i in 1:dim(orderings)[1]){
    for (procName in listNoSideEffects){
      if(ask){
        readline(prompt = paste0("hit Enter to try ", deparse(substitute(procName)), ":")) 
      }
      else {
        print(procName)
      }
      timeOut <- function (expr, ...)  {
        on.exit(setTimeLimit())
        setTimeLimit(...)
        expr
      }
        trialAnswer <- tryCatch(
          timeOut(do.call(procName, data[orderings[i,]]), elapsed=1)
          , error = function(e) as.character(e)
        )
      }
  }
  
}

