

isitperm <- function(data, answer){
  require(gtools)
  fnList <- ls("package:base")
  badfunctions <- dget("badfunctions.robj")
  listNoSideEffects <- fnList[fnList %in% badfunctions==FALSE]
  listNoSideEffects <- listNoSideEffects[-c(2:3,8:9, 13:26, 28:32,34:36,38:42,45,47, 49, 51:52, 62:63, 73:74, 78:79, 81)]
  
  
  for (procName in listNoSideEffects){
    orderings <- permutations(length(data), length(data), 1:length(data))
    readline(prompt = paste0("hit Enter to try ", deparse(substitute(procName)), ":")) 
    for (i in 1:dim(orderings)[1]){
      trialAnswer <- tryCatch(
        do.call(procName, data[orderings[i,]])
        , error = function(e) as.character(e)
      )
    }
  }
  
}

