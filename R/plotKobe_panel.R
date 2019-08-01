#' plotKobe_panel
#' \code{plotKobe_panel} custom wrapper to loop through nested operating/estimation models and plot using r4ss
#' @param rootdir master location of all models and (optional) replicates; will search for Report.sso files therein
#' @param pattern either a pattern to match many models or the full directory name for a single model.
#' @param plotrows number of rows per page of PDF
#' @param plotcols number of cols per page of PDF
#' @param saveplot logical. should plots be saved to model directory
#' @param plotloc folder to save plots; if NA, will save in location of cpue_results
#' @export
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

  ## iterate avail. runs and assign colors
  if(!is.na(pattern)){
    mods <- list.dirs(rootdir, recursive = F) %>%
      .[grepl(pattern, .)]
  } else if (is.na(pattern)){ mods <- rootdir}
  ## use SS_output function to extract quantities
  par(mfrow = c(plotrows, plotcols), mar = c(4, 4, 2, 1))

  for (m in 1:length(mods)) {
    # Kobe plot layout setting
    x_max = 2
    x_min = 0
    y_max = 5
    y_min = 0
    plot(
      c(x_min, x_max),
      c(y_min, y_max),
      type = "n",
      ylab = "",
      xlab = "",
      xaxs = "i",
      yaxs = "i",
      bty = "n",
      main = basename(mods[m])
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

    mtemp <- mods[m] %>%
      SS_output(.,
                covar = F,
                forecast = F,
                ncols = 313)

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

    # text(labels = mtemp$Kobe[1,'Year'],
    #      x = mtemp$Kobe[1,'B.Bmsy']*0.95,
    #      y = mtemp$Kobe[1,'F.Fmsy']*0.95,
    #      cex = 1)
    # text(labels = mtemp$Kobe[nrow(mtemp$Kobe),'Year'],
    #      x = mtemp$Kobe[nrow(mtemp$Kobe),'B.Bmsy']*0.95,
    #      y = mtemp$Kobe[nrow(mtemp$Kobe),'F.Fmsy']*0.95,
    #      cex = 1)

  } ## end mods loop
  cat('saved plot with model(s)',pattern," to ", plotloc,"\n")
  cat("set plotloc or change working dir if needed","\n")
  graphics.off()

} ## end function



# ## not run:
# ## test many
# kaputils::plotKobe_panel(
#   rootdir = "G:\\MAKO\\mako_sim",
#   pattern = "0.7-4-1",
#   plotloc = "G:\\MAKO\\mako_sim\\plots\\",
#   plotrows = 5,
#   plotcols = 4,
#   saveplot = T)
# #
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
