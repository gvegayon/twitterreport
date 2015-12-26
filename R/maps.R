# rm(list=ls())
# library(dplyr)
# library(twitterreport)
# library(leaflet)
# 
# data("senate_tweets")

#' Creates a leaflet map
#' 
#' This function works as a wrapper of the \code{leaflet} function, creating a
#' map with circles which sizes are set by the number of observations in those
#' coordinates.
#'
#' @param dat A twitter dataset (with coordinates)
#' @param coordinates Name of the \code{coordinates} variable (see details)
#' @param popup Name of the grouping variable (for example, \code{screen_name})
#' @param lat Name of the latitude variable
#' @param lng Name of the longitude variable
#' @param radii Formula to compute the radious of the circles
#' @param weight Thickness of the circles' borders
#' @param cluster.method Clustering method (see \code{\link{hclust}})
#' @param nclusters Max number of clusters to include
#' @param ... Further arguments to be passed to \code{\link{addCircles}}
#' @details 
#' The \code{coordinates} must be in the format of longitude:latitude
#' (as the twitter API returns). 
#' 
#' In order to improve visualization, the function performs Hierarchical
#' Clustering via \code{\link{hclust}} (from the stats package), grouping
#' observations by geo coordinates. For each cluster, the final lat/lng coords
#' are defined as the mean within the cluster.
#' 
#' In the case of \code{radii}, it is measured in meters (in the map). The
#' \code{n} is the number of observations in that cluster.
#' 
#' For performance considerations, it is recommended not to use more than
#' 1,000 observations (Try using a random sample from your data!) as 
#' \code{hclust} requires computing square matrices.
#' @author George G. Vega Yon
#' @return A map object (see the \code{\link{leaflet}} package)
#' @export
#' @examples
#' \dontrun{
#' # Getting the data
#' data(senate_tweets)
#' 
#' # using formulas to pass the variables names
#' tw_leaflet(senate_tweets,~coordinates,~screen_name)
#' tw_leaflet(senate_tweets,~coordinates)
#' 
#' # Using characters to pass the variables names
#' tw_leaflet(senate_tweets,'coordinates','screen_name')
#' tw_leaflet(senate_tweets,'coordinates')
#' 
#' # Aggregating until get only 3 big groups
#' tw_leaflet(senate_tweets,'coordinates', nclusters=3)
#' }
tw_leaflet <- function(dat,coordinates=NULL,popup=NULL ,lat=NULL,lng=NULL,
                       radii=~sqrt(n)*1000, weight=1, cluster.method='centroid',
                       nclusters=min(c(50,nrow(dat) %/% 3)) ,...) {
  # Checking latitude and longitude
  if (!length(coordinates) && !length(lat) && !length(lng))
    stop('At least -coordinates- should be provided')
  
  # If coordinates was provided as a formula
  if (inherits(coordinates,'formula')) 
    coordinates <- attr(terms(coordinates),'term.labels')
  if (length(coordinates))
    colnames(dat)[which(colnames(dat)==coordinates)] <- 'coordinates'
  
  # If lat/lng was provided as a formula
  if (inherits(lat,'formula')) lat <- attr(terms(lat),'term.labels')
  if (length(lat)) colnames(dat)[which(colnames(dat)==lat)] <- 'lat'
  
  if (inherits(lng,'formula')) lng <- attr(terms(lng),'term.labels')
  if (length(lng)) colnames(dat)[which(colnames(dat)==lng)] <- 'lng'
  
  # If no lat/lng was provided, we need to create these for the cluster analysis
  # Here we get the distance between all the points
  if (!length(lat)) dat[,c('lng','lat')] <- 
      do.call(rbind,strsplit(dat$coordinates,":", fixed = TRUE))
  
  dat$lat <-as.numeric(dat$lat)
  dat$lng <-as.numeric(dat$lng)
  
  # Subset of the data that has a coordinate or lat/lng
  dat <- subset(dat,!is.na(lat))
  
  d <- dist(dat[,c('lng','lat')])

  # Computing clusters
  clusters <- stats::hclust(d,cluster.method)
  dat$clusters <- stats::cutree(clusters, min(c(nclusters,nrow(dat))))
  
  # Checking popup
  if (inherits(popup, 'formula'))
    popup <- attr(terms(popup),'term.labels')
  if (length(popup)) colnames(dat)[which(colnames(dat)==popup)] <- 'popup'
  
  # Computing the aggregation, for this we use cluster analysis
  
  if (length(popup)) geo <- group_by_(dat, "clusters","popup")
  else geo <- group_by_(dat, "clusters")
  
  # Tabulating the data
  dots <- list(~n(),~mean(lat),~mean(lng))
  geo <-  summarise_(geo, .dots=setNames(dots,c('n','mean_lat','mean_lng'))) # "n=n()", "mean_lat=mean(lat)", "mean_lng=mean(lng)")

  if (!length(popup)) geo$popup <- prettyNum(geo$n, ',')
  
  geo <- leaflet(geo)
  geo <- addTiles(geo)
  
  addCircles(geo,radius = radii, lng = ~mean_lng, lat = ~mean_lat,
             weight = weight, popup =~popup,...)
}

# tw_leaflet(senate_tweets,~coordinates,~screen_name)
# tw_leaflet(senate_tweets,~coordinates)
# 
# tw_leaflet(senate_tweets,'coordinates','screen_name')
# tw_leaflet(senate_tweets,'coordinates')
