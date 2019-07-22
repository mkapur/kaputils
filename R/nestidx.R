#' Nested loop index
#'
#' \code{nestidx} returns the index value for a matrix populated by nested loops.
#' @param vals a list of up to length three with the current indices and length of each index vector
#' @examples
#' a.vector = seq(1:4)
#' b.vector = seq(7:100)
#' c.vector = seq(12:14)
#' #' mk <- matrix(NA)
#' for(a in 1:length(a.vector)){
#' for(b in 1:length(b.vector)){
#'  for(c in 1:length(c.vector)){
#'     idx <- nestidx(vals = list(a,b,b.vector,c,c.vector))
#'       cat(idx,"\n")
#'       mk[idx] <- a*b
#'    }
#'  }
#' }
#' @export

nestidx <-
  function(vals = list(a, b, b.vector, c = 0, c.vector = 0)) {
    return((vals[[1]] - 1) * length(vals[[3]]) + (vals[[2]] - 1) * length(vals[[5]]) + vals[[4]])
  }


