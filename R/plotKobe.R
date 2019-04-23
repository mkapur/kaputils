#' plotKobe
#' terminal kobe plot for a singly model. check out FLR.
#' @param rootdir home directory
#' @param terminal.only  Do you only want to show the final year or the whole timeseries?
#' @param axes.limits x and y maximums for kobe plot, default to 2
#' @param kobe.type if 'ISC', will have pale orange coloring in place of orange/yellow
#' @param b.name the character column name of the biomass ref point; can rename to change y lab
#' @param f.name the character column name of the fishing ref point
#' @param pattern 1st order directory character
#' @param subpattern 2nd order directory character
#' @param saveplot logical, do you want to save the plot
#' @param plotloc where to save plot
#' @param doLegend logical. do you want to write a legend (not advised for huge rep sets)



plotKobe <- function(summaryoutput,
                    terminal.only = c(T,F)[1],
                    axes.limits = c(2,2),
                    kobe.type = c(NA,'ISC')[1],
                    b.name = NA,
                    f.name = NA,
                    saveplot = T,
                    plotloc = NA,
                    doLegend = T) {

  ## plotting defaults
  graphics.off()

  if (is.na(plotloc)) {
    if (!exists(paste0(getwd(), "/plots/"))) {
      dir.create(paste0(getwd(), "/plots/"))
    }
    plotloc <- paste0(getwd(), "/plots/")
  } ## end plotploc

  if (saveplot){
    jpeg(
      paste0(plotloc, "kobe.jpg"),
      width = 8,
      height = 6,
      units = 'in',
      res = 420
    )} ## end saveplot

  par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

  # Kobe plot layout setting
  x_max = axes.limits[1]
  x_min = 0
  y_max = axes.limits[2]
  y_min = 0


  plot(
    c(x_min, x_max),
    c(y_min, y_max),
    type = "n",
    ylab = "",
    xlab = "",
    xaxs = "i",
    yaxs = "i",
    bty = "n"
  )
  mtext(
    side = 1,
    expression(SSB / SSB[MSY]),
    line = 2.5,
    cex = 1
  )
  mtext(side = 2,
        expression(F / F[MSY]),
        line = 2.5,
        cex = 1)

  if(is.na(kobe.type)){
    polygon(c(x_min, 1, 1, x_min),
            c(1, 1, x_min, x_min),
            col = "gold",
            border = NA)
    polygon(c(1, x_max, x_max, 1),
            c(1, 1, 0, 0),
            col = "forestgreen",
            border = NA)
    polygon(c(0, 1, 1, 0),
            c(1, 1, y_max, y_max),
            col = "red",
            border = NA)
    polygon(c(1, x_max, x_max, 1),
            c(1, 1, y_max, y_max),
            col = "goldenrod",
            border = NA)
  }else if(kobe.type == 'ISC'){
    polygon(c(x_min,1,1,x_min), c(1,1,y_min,y_min),col="khaki1")
    polygon(c(1,x_max,x_max,1), c(1,1,y_min,y_min),col="palegreen")
    polygon(c(x_min,1,1,x_min), c(1,1,y_max,y_max),col="salmon")
    polygon(c(1,x_max,x_max,1), c(1,1,y_max,y_max),col="khaki1")
  }

  if(terminal.only){
    with(mtemp$Kobe[nrow(mtemp$Kobe), ],
         points(
           B.Bmsy,
           F.Fmsy,
           pch = 25,
           bg = "grey88",
           col = 'blue'
         ))
  } else if(terminal.only == FALSE){
    # if(is.na( df[i,b.name])  | is.na( df[i,f.name])) stop("Your biomass or F column in management_quantities is NA. \n Did your model(s) estimate F/FMSY or B/BMSY for the final year? Check SPRSeries.csv")

    B_BMSY = summaryoutput$Kobe$B.Bmsy
    F_FMSY = summaryoutput$Kobe$F.Fmsy
    lines(
      B_BMSY,
      F_FMSY, col = 'grey22', lty = 'dashed'
    )

    points(
      B_BMSY,
      F_FMSY,
      pch = 21,
      col = c(rep('grey44',(length(B_BMSY)-2)),'blue'),
      bg =  c(rep('white',(length(B_BMSY)-2)),'black'),
      cex = 1.1
    )
    text(summaryoutput$Kobe$B.Bmsy[summaryoutput$Kobe$Yr == summaryoutput$endyr],
         summaryoutput$Kobe$F.Fmsy[summaryoutput$Kobe$Yr == summaryoutput$endyr]*0.9,
         paste(summaryoutput$endyr))

    text(summaryoutput$Kobe$B.Bmsy[summaryoutput$Kobe$Yr == summaryoutput$startyr + which.max(F_FMSY)],
         summaryoutput$Kobe$F.Fmsy[summaryoutput$Kobe$Yr == summaryoutput$startyr + which.max(F_FMSY)],
         paste(summaryoutput$startyr + which.max(F_FMSY)))

    text(summaryoutput$Kobe$B.Bmsy[summaryoutput$Kobe$Yr == summaryoutput$startyr],
         summaryoutput$Kobe$F.Fmsy[summaryoutput$Kobe$Yr == summaryoutput$startyr]*0.9,
         paste(summaryoutput$startyr))
  } ## end not terminal only
  graphics.off()

} ## end function

## not run:

# plotKobe(summaryoutput,
#                      terminal.only = c(T,F)[2],
#                      axes.limits = c(3,3),
#                      kobe.type = c(NA,'ISC')[2],
#                      b.name = 'B/BMSY',
#                      f.name = 'F/FMSY',
#                      saveplot = T,
#                      plotloc = "C:/Users/Maia Kapur/Dropbox/UW/coursework/FISH-555/stm_mods/Model_716/")