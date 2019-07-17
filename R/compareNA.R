#' compareNA
#' \code{compareNA} taken from R cookbook -- enables comparison of two things (strings/numbers/whatever) that may or may not contain NA. Will return TRUE if both  are equal, including if both are NA.
#' @param v1 first thing, can be NA
#' @param v2 second thing, can be NA
#' @seealso \link[ http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/]
#' @export
compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}
