#' compareNA
#' @param v1 first thing, can be NA
#' @param v2 second thing, can be NA
#' @export
compareNA <- function(v1,v2){
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}
