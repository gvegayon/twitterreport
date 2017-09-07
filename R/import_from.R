###############################################################################
# Regular package imports
################################################################################

#' @importFrom stringr str_extract str_detect str_extract_all str_replace
#' @importFrom dplyr summarise_ left_join group_by_ bind_rows semi_join
#' @importFrom DT datatable
#' @importFrom dygraphs dygraph dyRangeSelector
#' @importFrom httr GET config status_code write_stream content oauth1.0_token oauth_endpoints oauth_app
#' @importFrom networkD3 forceNetwork
#' @importFrom wordcloud wordcloud
#' @importFrom tm stopwords
#' @importFrom RColorBrewer brewer.pal
#' @importFrom RCurl getURL
#' @importFrom stats hclust cutree
#' @importFrom leaflet leaflet addTiles addCircles
#' @importFrom stats dist reshape setNames terms
#' @importFrom utils URLdecode URLencode head read.csv read.table
NULL

################################################################################
# knitr!
################################################################################
#' @import knitr
NULL

################################################################################
# For the Rcpp part
################################################################################
#' @useDynLib twitterreport, .registration = TRUE
NULL

#' @importFrom Rcpp sourceCpp
NULL

#' @import methods
#' @importClassesFrom Matrix dgCMatrix
#' @importFrom Matrix Matrix diag
NULL