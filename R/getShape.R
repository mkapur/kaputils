#' getShape
#'generate a beta random variable given a mean and sd. If the mean is 1 it will throw an error.
#' @param phis list of length two containing mean, cv
#' @param rng logical. if T, this will randomly generate a number under your params. if falso it will return alpha and beta (shape and rate).
#' @param ngen numeric. number of random number(s) to generate.

getShape <- function(phis, rng = F, ngen = NA){
  mu <- as.numeric(phis[1])
  cv <- as.numeric(phis[2])
  variance <- (mu * cv)^2
  alpha  <-   (((1/mu - 1)*mu^3)/variance) - mu
  beta  <-  alpha*(1/mu - 1)
  if (mu >= 1){stop('mu >= 1')}
  if(rng == F)
    return(c(alpha,beta)) ## provides shape parameters
  else if(rng == T)
    return(rbeta(ngen, shape1 = alpha, shape2 = beta))
  ## provides vector of length ngen given those shape parameters
}

## not run:
## generate some values and plot them
# testshape = cbind(seq(0.01,0.90,0.01),seq(0.01,1.5,1.5/90)) %>%
#   apply(.,1, getShape, rng = F) %>%
#   t() %>%
#   as.data.frame() %>%
#   na.omit(.)
# plot(
#   seq(0.01, 1, 0.01),
#   dbeta(seq(0.01, 1, 0.01), shape1 = testshape[33, "V1"], shape2 = testshape[33, "V2"]),
#   xlab = '',
#   ylab = 'Density',
#   ylim = c(0, 2.5),
#   type = 'l',
#   lwd = 2,
#   col = 'seagreen4',
#   main = "example beta distributions using getShape() function",
#   cex.main = 1.1,
#   bty = 'n'
# )
# lines(seq(0.01, 1, 0.01),
#       dbeta(seq(0.01, 1, 0.01), shape1 = testshape[31, "V1"], shape2 = testshape[31, "V2"]), lwd = 2, col = "seagreen3")
# lines(seq(0.01, 1, 0.01),
#       dbeta(seq(0.01, 1, 0.01), shape1 = testshape[35, "V1"], shape2 = testshape[35, "V2"]), lwd = 2, col = "dodgerblue3")
# legend("topright", cex = 0.85, bty = 'n', lwd = 1,
#        legend = c(expression(paste(alpha," = 2.34 ",beta," = 5.21")),
#                   expression(paste(alpha," = 1.93 ",beta," = 3.93")),
#                   expression(paste(alpha," = 1.60 ",beta," = 2.98"))),
#        col = c('seagreen3','seagreen4','dodgerblue3'))
