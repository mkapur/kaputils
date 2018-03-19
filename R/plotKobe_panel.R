#' plotKobe_panel
#'
#' \code{plotKobe_panel} custom wrapper to loop through nested operating/estimation models and plot using r4ss
#' @param rootdir master location of all models and (optional) replicates; will search for Report.sso files therein
#' @param pattern either a pattern to match many models or the full directory name for a single model.
#' @param subpattern can be NA; a secondary string by which models will be grouped, e.g. "Replicate"
#' @param llabels logical. If T, will write a CSV with results.
#' @seealso \code{\link[r4ss]}
#'
plotKobe_panel <- function(rootdir,
                       pattern = NA,
                       plotloc = NA,
                       plotrows = 1,
                       plotcols = 2,
                       saveplot = T) {
  ## plotting defaults
  graphics.off()

  if (is.na(plotloc)) {
    if (!exists(paste0(getwd(), "/plots/"))) {
      dir.create(paste0(getwd(), "/plots/"))
    }
    plotloc <- paste0(getwd(), "/plots/")
  }

  if(saveplot) jpeg(paste0(plotloc, "kobe_panel.jpg"), width = 8, height = 6, units = 'in', res = 1020)

  par(mfrow = c(plotrows, plotcols), mar = c(4, 4, 2, 1))
  # par(mfrow = c(1,1))
  # Kobe plot layout setting
  x_max = 2
  x_min = 0
  y_max = 5
  y_min = 0

  col.choices = c('deepskyblue',
                  "mediumspringgreen",
                  "aquamarine",
                  "bisque2",
                  "brown1",
                  "deeppink")

if(!is.na(pattern)){
  mods <- list.dirs(rootdir, recursive = F) %>%
    .[grepl(pattern, .)]
} else if (is.na(pattern)){
    mods <- rootdir}

  for (m in 1:length(mods)) {

    ## use SS_output function to extract quantities


    mtemp <- mods[m] %>%
      SS_output(.,
                covar = F,
                ncols = 313)
    plot(
      c(x_min, x_max),
      c(y_min, y_max),
      type = "n",
      ylab = "",
      xlab = "",
      xaxs = "i",
      yaxs = "i",
      bty = "n",
      main = sub('.*\\/', '', mods)[m]
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

    with(mtemp$Kobe,
         points(
           B.Bmsy,
           F.Fmsy,
           pch = 21,
           bg = "black",
           col = 'black'
         ))
    with(mtemp$Kobe,
         lines(
           B.Bmsy,
           F.Fmsy,
           pch = 21,
           bg = "black",
           col = 'black'
         ))
    with(mtemp$Kobe[1,],
         points(
           B.Bmsy,
           F.Fmsy,
           pch = 17,
           bg = "grey88",
           col = 'purple'
         ))
    with(mtemp$Kobe[nrow(mtemp$Kobe),],
         points(
           B.Bmsy,
           F.Fmsy,
           pch = 25,
           bg = "grey88",
           col = 'grey'
         ))
    legend('topright',
           pch = c(17,25),
           col = c('purple','grey'),
           legend = c(mtemp$Kobe[1,'Year'], mtemp$Kobe[nrow(mtemp$Kobe),'Year']))
    text(labels = mtemp$Kobe[1,'Year'],
         x = mtemp$Kobe[1,'B.Bmsy']*0.95,
         y = mtemp$Kobe[1,'F.Fmsy']*0.95,
         cex = 1)
    text(labels = mtemp$Kobe[nrow(mtemp$Kobe),'Year'],
         x = mtemp$Kobe[nrow(mtemp$Kobe),'B.Bmsy']*0.95,
         y = mtemp$Kobe[nrow(mtemp$Kobe),'F.Fmsy']*0.95,
         cex = 1)


  }
  cat('saved plot with model(s)',pattern," to ", plotloc,"\n")
  cat("set plotloc or change working dir if needed","\n")
  graphics.off()

}

# ## not run:
# ## test many
# plotKobe_panel(
#   rootdir = "G:\\MAKO\\mako_sim\\FCruns",
#   pattern = "7_S",
#   plotloc = "G:\\MAKO\\mako_sim\\FCruns\\plots\\",
#   plotrows = 3,
#   plotcols = 2,
#   saveplot = T)
#
# ## test many
# plotKobe_panel(
#   rootdir = "G:\\MAKO\\mako_sim\\presentation",
#   pattern = "13",
#   plotloc = "G:\\MAKO\\mako_sim\\presentation\\plots\\",
#   plotrows = 1,
#   plotcols = 1,
#   saveplot = T)
#
#
# ## test one
# plotKobe_panel(
#   rootdir = "G:\\MAKO\\mako_sim\\FCruns",
#   pattern = "47_S8",
#   plotloc = "G:\\MAKO\\mako_sim\\FCruns\\plots\\",
#   plotrows = 1,
#   plotcols = 2,
#   saveplot = T)
