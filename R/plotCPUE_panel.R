#' plotCPUE_panel
#'
#' \code{plotCPUE_panel} panel plot illustrating fitted CPUE from model(s) and observed/CI
#' @param cpue_results a data frame generated using extract_results()
#' @param saveplot logical. should plots be saved to model directory
#' @param mods vector of model names as they appear in the cpue_results CSV to use; NA will use all
#' @param plotloc folder to save plots; if NA, will save in location of cpue_results
#' @seealso \code{\link[r4ss]}
#'
plotCPUE_panel <-
  function(cpue_results,
           saveplot = T,
           mods = NA,
           plotloc = NA,
           pdfrows = 4,
           pdfcols = 2) {

    if (is.na(plotloc)) {
      if (!exists(paste0(getwd(), "/plots/"))) {
        dir.create(paste0(getwd(), "/plots/"))
      }
      plotloc <- paste0(getwd(), "/plots/")
    }

    cpuep <- list()
    yrvec <- with(cpue_results, c(min(Yr), max(Yr)))
    cpuevec <- with(cpue_results, c(0, max(Exp, Obs)))

    for (i in unique(cpue_results$FleetName)) {
      if (sum(is.na(mods)) == 0) {
        cpue_results0 <-
          cpue_results[cpue_results$MOD %in% mods &
                         cpue_results$FleetName == i, ]
      } else{
        cpue_results0 <-
          with(cpue_results, cpue_results[grepl(i, FleetName), ])
      }
      cpuep[[i]] <-
        ggplot(cpue_results0, aes(x = Yr, col = MOD)) +
        theme_bw() +
        theme(
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(size = rel(0.75)),
          axis.title = element_text(size = rel(0.75)),
          legend.position = c(0.15, 0.80),
          legend.background = element_rect(fill = NA, colour = NA),
          legend.key = element_blank(),
          legend.text = element_text(size = rel(0.75)),
          legend.title = element_blank()
        ) +
        scale_color_manual(values = rep(brewer.pal(11, name = 'Spectral'), nrow(cpue_results))) +
        scale_y_continuous(limits = cpuevec, expand = c(0, 0)) +
        scale_x_continuous(limits = yrvec) +
        ylab(paste0(i, " Expected CPUE")) +
        xlab("Year") +
        geom_line(lwd = 1.1, aes(y = Exp)) +
        geom_point(aes(y = Obs), col = 'black') +
        geom_errorbar(aes(
          x = Yr,
          ymin = Obs - SE,
          ymax = Obs + SE
        ),
        col = 'black',
        width = 0)
    }
    if (saveplot == T) {
      # # save individual JPEGs
      # for (s in 1:length(cpuep)) {
      #   ggplot2::ggsave(
      #     paste0(plotloc, "\\CPUE_",  names(cpuep)[s], ".jpg"),
      #     plot = cpuep[[s]],
      #     width = 7,
      #     height = 5,
      #     units = 'in',
      #     dpi = 1020
      #   )
      # }
      ## save one-page PDF
      ml <-
        gridExtra::marrangeGrob(cpuep, nrow = pdfrows, ncol = pdfcols)
      ## non-interactive use, multipage pdf
      ggplot2::ggsave(
        paste0(plotloc, "\\cpue_panel.pdf"),
        ml,
        width = 8.5,
        height = 11,
        units = 'in'
      )
      cat("saved plot with model(s) to ", plotloc,"\n")
      cat("set plotloc or change working dir if needed","\n")
      graphics.off()
    } else
      (print(cpuep))

  }

## not run:
## testing with subset of mods
# cpue_results <- read.csv("G:\\MAKO\\mako_sim\\FCruns\\results\\cpue.csv")
#
# plotCPUE_panel(cpue_results,
#            saveplot = T,
#            mods = c("47_S1-NewCTL","47_S2-NewCTL"),
#            plotloc = "G:\\MAKO\\mako_sim\\FCruns\\plots",
#            pdfrows = 2,
#            pdfcols = 2)
#
# ## test with all mods
# plotCPUE_panel(cpue_results,
#                saveplot = T,
#                mods = NA,
#                plotloc = "G:\\MAKO\\mako_sim\\FCruns\\plots",
#                pdfrows = 3,
#                pdfcols = 2)
#
# ## test with one mod
# plotCPUE_panel(cpue_results,
#                saveplot = T,
#                mods = "47_S1-NewCTL",
#                plotloc = "G:\\MAKO\\mako_sim\\FCruns\\plots",
#                pdfrows = 3,
#                pdfcols = 2)

