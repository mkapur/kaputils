#' plotSRR_panel
#' reboot of stock-recruit relationship plots from r4ss; plots expected curve and model-fitted estimates from Stock Synthesis Model
#' @param rootdir root filepath where all subdirectories containing Report.sso are stored
#' @param pattern a string that specifically matches all directories with report files of interest.
#' @param saveplot logical. should plots be saved to model directory
#' @param plotloc folder to save plots; if NA, will save in rootdir/plots
#' @param pdfrows number of rows per page of PDF
#' @param pdfcols number of cols per page of PDF
plotSRR_panel <- function(rootdir,
                          pattern = NA,
                          saveplot = T,
                          plotloc = NA,
                          pdfrows = 4,
                          pdfcols = 2) {
  if (is.na(plotloc)) {
    if (!exists(paste0(rootdir, "/plots/"))) {
      dir.create(paste0(rootdir, "/plots/"))
    }
    plotloc <- paste0(rootdir, "/plots/")
  }

  if (sum(is.na(pattern)) == 0) {
    mods <- list.dirs(rootdir) %>%
      .[grepl(pattern, .)]
  } else{
    mods <-
      list.files(
        rootdir,
        pattern = "EM|OM",
        recursive = F,
        full.names = T
      )
  }
  srrp <- list()

  for (m in 1:length(mods)) {
    ## loop into master file

    summaryoutput0 <- mods[m] %>%
      SSgetoutput(dirvec = .,
                  getcovar = F,
                  forecast = F,
                  ncols = 1000)

    summaryoutput <- summaryoutput0$replist1[["recruit"]]


    ssbvec <- with(summaryoutput, c(0, max(SpawnBio)))
    recvec <- with(summaryoutput, c(0, max(exp_recr, pred_recr)))

    srrp[[m]] <- summaryoutput %>%
      ggplot(., aes(x = SpawnBio)) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        legend.position = c(0.15, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1)),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      scale_color_manual(
        values = c('firebrick2', 'grey22'),
        labels = c("Estimated Recruits", "Expected Recruits"),
        guide = guide_legend(override.aes = list(
          linetype = c('blank', 'solid'),
          shape = c(19, NA)
        ))
      ) +
      xlab("Spawning Biomass (mt)") +
      ylab("Recruitment (1,000s)") +
      xlim(ssbvec) +
      ylim(recvec) +
      ggtitle(basename(mods)[m]) +

      geom_hline(yintercept = 0, col = 'grey50') +
      geom_vline(xintercept = 0, col = 'grey50') +

      geom_line(aes(y = exp_recr, col = "Expected Recruits"), lwd = 1.1) +
      geom_point(aes(y = pred_recr, col = "Estimated Recruits"),
                 size = 2,
                 pch = 19)
  } ## end mods
  if (saveplot == T) {
    ## save one-page PDF
    ml <-
      gridExtra::marrangeGrob(srrp, nrow = pdfrows, ncol = pdfcols)
    ## non-interactive use, multipage pdf
    ggplot2::ggsave(
      paste0(plotloc, "/srr_panel.pdf"),
      ml,
      width = 8.5,
      height = 11,
      units = 'in'
    )
    ggplot2::ggsave(
      paste0(plotloc, "/srr_panel.jpg"),
      ml,
      width = 8.5,
      height = 11,
      units = 'in'
    )
    cat("saved plot with model(s) to", plotloc,
        "\n")
    cat("set plotloc or change working dir if needed", "\n")
    graphics.off()
  }

}
