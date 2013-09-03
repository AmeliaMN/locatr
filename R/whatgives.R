#' Find the function that gives the result you want (for a specific ordering of data)
#'
#' This function takes a list of data and an expected answer and tests all known functions for the expected answer.
#' Data are tested exactly in the order they were entered. To test permutations of the data, use \code{\link{locatr}}.
#' 
#' @param data a list containing the data, formatted as you believe is necessary
#' @param answer the answer you expect (may someday need to be a list?)
#' @return A list of potential functions for the specified data
#' @export

whatgives <- function(data, answer, anstr=NULL){
  fnList <- ls("package:base")
 # badfunctions <- dget("badfunctions.robj")
  badfunctions <- c("cat", "kronecker", "l10n_info", "La.svd", "lockEnvironment", 
                    "lockBinding", "message", "packageStartupMessage", "print", "print.AsIs", 
                    "print.by", "print.condition", "print.connection", "print.data.frame", 
                    "print.Date", "print.default", "print.difftime", "print.DLLInfo", 
                    "print.DLLInfoList", "print.DLLRegisteredRoutines", "print.factor", 
                    "print.function", "print.hexmode", "print.libraryIQR", "print.listof", 
                    "print.NativeRoutineList", "print.noquote", "print.numeric_version", 
                    "print.octmode", "print.packageInfo", "print.POSIXct", "print.POSIXlt", 
                    "print.proc_time", "print.restart", "print.rle", "print.simple.list", 
                    "print.srcfile", "print.srcref", "print.summary.table", "print.summaryDefault", 
                    "print.table", "print.warnings", "quit", "R.home", "rawConnection", 
                    "readRenviron", "Recall", "repeat", "require", "restartDescription", 
                    "stop", "stopifnot", "substring<-", "summary.srcref", "traceback", 
                    "while", "within.data.frame", "asNamespace", "getNamespaceInfo", 
                    "importIntoEnv", "isBaseNamespace", "isNamespace", "namespaceExport", 
                    "namespaceImport", "namespaceImportFrom", "namespaceImportClasses", 
                    "namespaceImportMethods", "packageHasNamespace", "parseNamespaceFile", 
                    "registerS3method", "registerS3methods", "setNamespaceInfo", 
                    "loadingNamespaceInfo", "getNamespace", "file", "url", "gzfile", 
                    "bzfile", "xzfile", "unz", "pipe", "fifo", "socketConnection", 
                    "open", "close", "flush", "isOpen", "isIncomplete", "browser", 
                    "gc", "RNGkind", "trace", "tracemem", "tracingState", "warning", 
                    "lazyLoad", "library", "library.dynam", "library.dynam.unload", 
                    "license", "load", "attachNamespace", "loadNamespace", "requireNamespace", 
                    "loadedNamespaces", "unloadNamespace", "options", "remove", "rm", 
                    "save", "save.image", "setwd", "srcfile", "srcfilealias", "srcfilecopy", 
                    "srcref", "sys.call", "sys.calls", "Sys.chmod", "Sys.Date", "sys.frame", 
                    "sys.frames", "sys.function", "Sys.getenv", "Sys.getlocale", 
                    "Sys.getpid", "Sys.glob", "Sys.info", "sys.load.image", "Sys.localeconv", 
                    "sys.nframe", "sys.on.exit", "sys.parent", "sys.parents", "Sys.readlink", 
                    "sys.save.image", "Sys.setenv", "Sys.setFileTime", "Sys.setlocale", 
                    "Sys.sleep", "sys.source", "sys.status", "Sys.time", "Sys.timezone", 
                    "Sys.umask", "Sys.unsetenv", "Sys.which", "system", "system.file", 
                    "system.time", "system2", "tempdir", "tempfile", "socketSelect", 
                    "open.connection", "open.srcfile", "open.srcfilealias", "open.srcfilecopy", 
                    "gc.time", "gcinfo", "gctorture", "gctorture2", "readline", "file.choose", 
                    "find.package", "path.package", "testPlatformEquivalence", "findPackageEnv", 
                    "lazyLoadDBfetch", "file.access", "file.append", "file.copy", 
                    "file.create", "file.exists", "file.info", "file.link", "file.path", 
                    "file.remove", "file.rename", "file.show", "file.symlink", "mean.data.frame", 
                    "mean.Date", "mean.default", "mean.difftime", "mean.POSIXct", 
                    "mean.POSIXlt", "gzcon", "memCompress", "readLines", "writeLines", 
                    "sink", "scan", "read.dcf", "dput", "dump", "readBin", "readChar", 
                    "writeBin", "writeChar", "textConnection", "seek", "pushBack", 
                    "getConnection", "install.packages", "stdin", "stdout", "stderr", 
                    "isatty", "setTimeLimit", "setSessionTimeLimit", "autoload", 
                    "autoloader", "readRDS", "saveRDS", "environment", "environment<-", 
                    "emptyenv", "globalenv", "baseenv", "parent.env", "parent.env<-", 
                    "environmentName", "env.profile", "getwd", "list.files", "normalizePath", 
                    ".libPaths", ".Library", ".Library.site", "dir.create", "basename", 
                    "dirname", "browserText", "browserCondition", "browserSetDebug", 
                    "pos.to.env", "gettext", "ngettext", "bindtextdomain", "as.environment", 
                    "as.list.environment", "environmentIsLocked", "unlockBinding", 
                    "bindingIsLocked", "makeActiveBinding", "bindingIsActive", "dget", 
                    "list2env", "rawConnectionValue", "search", "searchpaths", "seek.connection", 
                    "standardGeneric", "taskCallbackManager", "attach", "detach", "serialize", "unserialize", 
                    "lazyLoadDBexec"    
  )
  listNoSideEffects <- fnList[fnList %in% badfunctions==FALSE]
  listNoSideEffects <- listNoSideEffects[-c(2:3,8:9, 13:26, 28:32,34:36,38:42,45,47, 49, 51:52, 62:63, 73:74, 78:79, 81)]
  binaryOps <- listNoSideEffects[1:38]
  output <- NULL
  i <- 1
  if(missing(anstr)){
    anstr <- substitute(data)[-1]
  }
  for (procName in listNoSideEffects){
    if(isit(procName, data, answer)){
      flagb <- procName %in% binaryOps
      output[i] <- prettyfunctionprint(procName, data, anstr, flagb)
      i <- i + 1
    }
  }
#   
#   condition_call <- substitute(answer)
#   env <- list2env(data, parent=parent.frame())
#   r <- eval(condition_call, env)
  
  if(length(output)==0){
    return("sorry, no functions found")
  }
  else{
    return(output)
  }
}





prettyfunctionprint <- function(procName, data, anstr=NULL, flagb){
if(flagb){
  output <- paste(data[1], procName, data[2])
}
# Otherwise, use the parentheses format
else{
  dataU <- paste0(anstr, collapse=", ")
  output <- paste0(procName, "(", dataU, ")")
}
return(output)
}