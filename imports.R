rm(list=ls())
library(stringr)

# Getting imports
check <- readLines('/home/george/Documents/projects/twitterreport/twitterreport.Rcheck/00check.log')
check <- paste(check, collapse=' ')

check <- str_extract_all(check,'(?<=definition for)\\s+[[:graph:]]+',simplify = TRUE)
check <- gsub('‘|’|\\s','',check)
check <- unique(as.vector(check))
check <- paste0(check,collapse=' ')


# This function was suggested by Kurt Hornik
imports_for_undefined_globals <-
function(txt, lst, selective = TRUE)
{
  if(!missing(txt))
    lst <- scan(what = character(), text = txt, quiet = TRUE)
  nms <- lapply(lst, find)
  ind <- sapply(nms, length) > 0L
  imp <- split(lst[ind], substring(unlist(nms[ind]), 9L))
  if(selective) {
    # sprintf("importFrom(%s)",
    sprintf("#' @importFrom %s",
            vapply(Map(c, names(imp), imp),
                   function(e)
                     #paste0("\"", e, "\"", collapse = ", "),
                     paste0(e,collapse = " "),
                   ""))
  } else {
    sprintf("import(\"%s\")", names(imp))
  }
}

imports_for_undefined_globals(check)


################################################################################
# Check the dependencies of the packages
################################################################################

library(stringr)
library(twitterreport)
check_imports <- function(f, exclude_base=TRUE, DESCRIPTION=NULL) {
  fs <- f
  
  # Checking what type of object is
  if (dir.exists(f)) fs <- list.files(f,pattern = '\\.R$',ignore.case = TRUE,
                                      full.names=TRUE)
  else if (!file.exists(f)) stop('No such file or directory')
  
  funs <- lapply(fs, function(x) {
    x <- readLines(x)
    # x <- x[!grepl('^\\s*[#]',x)]
    str_extract_all(x,'[a-zA-Z_\\.]+(?=\\()',simplify = TRUE)
  })
  
  # Making unique
  funs <- unique(unlist(funs,recursive = TRUE))
  funs <- funs[which(funs!="")]
  
  # Looking for NAMESPACES
  pkgs <- lapply(funs,find)
  
  # Checking out packages to load
  pkgs <- lapply(pkgs, gsub, pattern="package[:]", replace="")
  names(pkgs) <- funs
  pkgs
}

x<-check_imports("R")
x
table(unlist(x))