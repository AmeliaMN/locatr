#' Take out methods for generic functions
#' 
#' 
#' 
badfunctions <- c("cat", "kronecker", "l10n_info", "La.svd", "lockEnvironment", 
                  "lockBinding", "message", "packageStartupMessage", "print",
                  "quit", "R.home", "rawConnection", 
                  "readRenviron", "Recall", "repeat", "require", "restartDescription", 
                  "stop", "stopifnot", "substring<-", "traceback", 
                  "while", "asNamespace", "getNamespaceInfo", 
                  "importIntoEnv", "isBaseNamespace", "isNamespace", "namespaceExport", 
                  "namespaceImport", "namespaceImportFrom", "namespaceImportClasses", 
                  "namespaceImportMethods", "packageHasNamespace", "parseNamespaceFile", 
                  "registerS3method", "registerS3methods", "setNamespaceInfo", 
                  "loadingNamespaceInfo", "getNamespace", "file", "url", "gzfile", 
                  "bzfile", "xzfile", "unz", "pipe", "fifo", "socketConnection", 
                  "open", "close", "flush", "isOpen", "isIncomplete", "browser", 
                  "gc", "RNGkind", "trace", "tracemem", "tracingState", "warning", "last.warning",
                  "lazyLoad", "library", "library.dynam", "library.dynam.unload", 
                  "license", "load", "attachNamespace", "loadNamespace", "requireNamespace", 
                  "loadedNamespaces", "unloadNamespace", "options", "remove", "rm", 
                  "save", "save.image", "setwd", "srcfile", "srcfilealias", "srcfilecopy", 
                  "srcref",  "system", "system2", "tempdir", "tempfile", "socketSelect", 
                  "gc.time", "gcinfo", "gctorture", "gctorture2", "readline", 
                  "find.package", "path.package", "testPlatformEquivalence", "findPackageEnv", 
                  "lazyLoadDBfetch", "gzcon", "memCompress", "readLines", "writeLines", 
                  "sink", "scan", "read.dcf", "dput", "dump", "readBin", "readChar", 
                  "writeBin", "writeChar", "textConnection", "seek", "pushBack", 
                  "getConnection", "stdin", "stdout", "stderr", 
                  "isatty", "setTimeLimit", "setSessionTimeLimit", "autoload", 
                  "autoloader", "readRDS", "saveRDS", "environment", "environment<-", 
                  "emptyenv", "globalenv", "baseenv", "parent.env", "parent.env<-", 
                  "environmentName", "env.profile", "getwd", "list.files", "normalizePath", 
                   "dir.create", "basename", 
                  "dirname", "browserText", "browserCondition", "browserSetDebug", 
                  "pos.to.env", "gettext", "ngettext", "bindtextdomain", "as.environment", 
                  "environmentIsLocked", "unlockBinding", 
                  "bindingIsLocked", "makeActiveBinding", "bindingIsActive", "dget", 
                  "list2env", "rawConnectionValue", "search", "searchpaths", "seek.connection", 
                  "standardGeneric", "taskCallbackManager", "attach", "detach", "serialize", "unserialize", 
                  "lazyLoadDBexec", "prmatrix", "getCallingDLL","getCallingDLLe", "getDLLRegisteredRoutines"
)


df2 <- read.table("subsidiaryToIgnore")
df2$V1 <- as.character(df2$V1)
df2$V1 <- gsub(".", "\\.", df2$V1, fixed=TRUE)

takeout <- function(fnnames){
  whichones <- NULL
  fullL <- ls("package:base")
  for (i in 1:length(fnnames)){
    pstr <- paste0("^",fnnames[i])
    whichones <- c(whichones, grep(pstr, fullL))
  }
return(whichones)
}


fnList <- ls("package:base")
badfunctions <- c(badfunctions, fnList[takeout(df2$V1)])
dput(badfunctions, file="badfunctions.robj")









