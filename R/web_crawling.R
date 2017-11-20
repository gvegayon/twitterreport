#' Retrieve twitter accounts from a website
#' 
#' @param uri Character scalar. Web address of the website from which to extract
#' twitter accounts.
#' @param redirect Whether to follow up the link or not (redirection)
#' @param normalize When `TRUE` returns the accounts as lower-case.
#' @param quiet When `FALSE` suppress the message
#' 
#' @return A character vector with twitter accounts' screen name found if any.
#' 
#' @examples
#' 
#' \dontrun{
#' # Twitter accounts from US gov officials (out dated)
#' tw_get_tw_account('http://twitter.pbworks.com/w/page/1779986/USGovernment')
#' 
#' # Caltech
#' tw_get_tw_account('http://caltech.edu')
#' }
#' 
#' @author George G. Vega Yon
#' @family text processors
#' @export
tw_get_tw_account <- function(uri, redirect=TRUE, normalize=TRUE, quiet=TRUE) {
  
  # Getting the format
  if (is.factor(uri)) uri <- as.character(uri)
  
  web <- getURL(uri,followlocation=TRUE)
  if (redirect) {
    if (str_detect(web,'meta\\s+http-equiv="refresh"\\s+content')) {
      uri <- str_replace(str_extract(web,'(?<=(url|URL)\\="?).*'),'".*',"")
      message('\tGoing deeper, visiting ',uri)
      return(tw_get_tw_account(uri))
    }
  }
  
  accounts <- str_extract_all(
    web,
    '"https?:\\/\\/(www\\.)?twitter\\.com\\/(#!\\/|intent\\/user\\?screen_name=)?[a-zA-Z0-9_]+(?=">?)',
    simplify = TRUE)
  # Normalizing the twitter accounts (note that screen names are not case
  # sensitive)
  accounts <- str_extract(accounts, "[a-zA-Z0-9_]+$")
  
  if (normalize)
    accounts <- unique(tolower(accounts))
  
  if (!quiet)
    message("URI ", substr(uri,1,50),ifelse(nchar(uri)>50,"...","")," done.")
  
  return(accounts)
}

