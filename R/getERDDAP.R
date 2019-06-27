#' GetSST
#'
#' \code{GetSST} Retrieves 2x2 degree aggregated SST data from the ERDDAP SERVER https://github.com/ropensci/rerddap
#' @param df a dataframe containing latitudes ("LAT") and longitude ("LONG") values, either aggregated by site or individual records
#' @param lat the column name with decimal degrees latitude
#' @param lon the column name with decimal degrees longitude
#' @param hitrans logical. If T, negative coordinates will be recentered around Hawaii so the Pacific ocean is whole. Note that this is required for use with many errdap datasets.
#' @param meanloc logical. If T, function will determine mean lat and long for each site (if many records per site).
#' @param grouploc If meanloc is T, provide grouping column (e.g. "site") for mean lat and long.
#' @param info Optionally select another griddap dataset; default is erdBAsstamday
#' @param dates Two dates in YYYY-MM-DD format, between which all SST values will be averaged.
#' @param buffer Number in decimal degrees that will be added to each location when searching for data
#' @seealso \code{\link[rerddap]{ed_search(query = "OCEANWATCH SST GOES-POES")}}
#' @export
getSST <-
  function(df,
           lat = "LAT",
           lon = "LON",
           hitrans = T,
           meanloc = F,
           grouploc = "Site",
           info = "erdBAsstamday",
           dates = c('2012-07-16', '2013-09-16'),
           buffer = 2) {
    if (hitrans) {
      df %>%  mutate(lon = round(ifelse(lon < 0, 360 + lon, lon)))
    }
    if (meanloc) {
      dat <- df %>% group_by(grouploc) %>%
        summarise(meanLat = mean(LAT), meanLon = mean(lon))
    }


  }
