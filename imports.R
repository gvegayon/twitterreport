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
