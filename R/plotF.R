#' plotF
#'
#' \code{plotF} plots vector of F_std from SS3 report file, with optional overlay of multiple models
#' @param SPRseries csv
#' @param saveplot logical. should plots be saved
#' @param plotloc folder to save plots; if NA, will save in location of cpue_results
#' @seealso \code{\link[r4ss]}

plotF <- function(SPRseries,
                  saveplot = T,
                  mods = NA,
                  plotloc = NA,
                  pdfrows = 4,
                  pdfcols = 2) {

 # if(saveplot)jpeg(  paste0(plotloc, "F_Trajectory.jpg"), width = 8, height = 5, unit = 'in', res = 1020)

  if (is.na(plotloc)) {
    if (!exists(paste0(getwd(), "/plots/"))) {
      dir.create(paste0(getwd(), "/plots/"))
    }
    plotloc <- paste0(getwd(), "/plots/")
  }


  if (sum(is.na(mods)) == 0) {
    SPRseries0 <-
      SPRseries[SPRseries$MOD %in% mods, ]
  } else{
    SPRseries0 <- SPRseries
  }
  if(length(unique(SPRseries0$MOD)) <= 5){

    SPRseries0 %>% ggplot(., aes(x = Year, y = F_std, col = MOD)) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = rel(1)),
      axis.title = element_text(size = rel(1)),
      legend.position = c(0.9, 0.80),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1)),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = rep(brewer.pal(11, name = 'Spectral'), length(unique(SPRseries0$MOD)))) +
    ylab("F_Std") +
    xlab("Year") +
    geom_line(lwd = 1.1)
  } else if(length(unique(SPRseries0$MOD))> 5){

      SPRseries0 %>% ggplot(., aes(x = Year, y = F_std, col = MOD)) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        legend.position = 'none'
      ) +
      scale_color_manual(values = rep(brewer.pal(11, name = 'Spectral'), length(unique(SPRseries0$MOD)))) +
      ylab("F_Std") +
      xlab("Year") +
      geom_line(lwd = 1.1)
  }

if (saveplot == T) {
  ggplot2::ggsave(
    paste0(plotloc, "/F_Trajectory.jpeg"),
    plot = last_plot(),
    path = NULL,
    scale = 1,
    width = 7,
    height = 5,
    units = "in",
    dpi = 1020,
    limitsize = TRUE
  )
  ggplot2::ggsave(
    paste0(plotloc, "/F_Trajectory.pdf"),
    plot = last_plot(),
    path = NULL,
    scale = 1,
    width = 7,
    height = 5,
    units = "in",
    dpi = 1020,
    limitsize = TRUE
  )
}
    cat('saved plot with model(s)',
        mods,
        " to ",
        paste0(plotloc, "F_Trajectory.jpg"),
        "\n")
    cat("set plotloc or change working dir if needed", "\n")

  }

# RootFile = "G:/MAKO/mako_sim/"
# SPRseries <-read.csv(paste0(RootFile,"/results/SPRseries.csv")) %>%
#   plyr::rename(c('Yr' = 'Year', 'F.Z.M' = 'F_std'))
# plotF(
#   SPRseries,
#   saveplot = T,
#   mods = NA,
#   plotloc = paste0(RootFile,"/plots/"),
#   pdfrows = 1,
#   pdfcols = 1
# )

## not run
# SPRseries <- read.csv("G:\\MAKO\\mako_sim\\presentation\\results\\SPRseries.csv")
# plotF(
#   SPRseries,
#   saveplot = T,
#   mods = NA,
#   plotloc = NA,
#   pdfrows = 4,
#   pdfcols = 2
# )
