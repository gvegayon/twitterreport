# x <- tw_extract(tweets$text)
# conv <- tw_conversation(tweets$screen_name,lapply(x,"[[","mention"))
# mygraph <- write.gexf(conv2$nodes,conv2$edges[,-3],keepFactors = TRUE)

#' @description  Function to enter the website and get the twitter account
tw_get_tw_account <- function(uri, redirect=TRUE) {
  web <- getURL(uri,followlocation=TRUE)
  if (redirect) {
    if (str_detect(web,'meta\\s+http-equiv="refresh"\\s+content')) {
      uri <- str_replace(str_extract(web,'(?<=(url|URL)\\="?).*'),'".*',"")
      message('\tGoing deeper, visiting ',uri)
      return(tw_get_tw_account(uri))
    }
  }
  accounts <- str_extract_all(web,'https?://(www\\.)?twitter.com/(#!/)?[a-zA-Z0-9_]+(?=">?)')[[1]]
  
  # Normalizing the twitter accounts (note that screen names are not case
  # sensitive)
  accounts <- str_extract(accounts, "[a-zA-Z0-9_]+$")
  accounts <- unique(tolower(accounts))
  
  return(accounts)
}