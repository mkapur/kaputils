#' plotInputSel
#'
#' \code{plotInputSel} parsing of input selectivity plots to show by-fleet, by-sex curves as proposed in SS3 Control File
#' @param moddir filepath where Report.sso is stored
#' @param write logical. should plots be saved to model directory
#' @param type type of file to be saved
#' @seealso \code{\link[r4ss]}



plotInputSel <- function(rootdir,
                         pattern = NA,
                         year = 2016,
                         lmin = 35,
                         lmax = 350,
                         linc = 5,
                         pdfrows = 3,
                         pdfcols = 2) {


  if (is.na(plotloc)) {
    if (!exists(paste0(getwd(), "/plots/"))) {
      dir.create(paste0(getwd(), "/plots/"))
    }
    plotloc <- paste0(getwd(), "/plots/")
  } ## end plotploc

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

  for (m in 1:length(mods)) {
  summaryoutput0 <- mods[m] %>%
    SSgetoutput(dirvec = .,
                getcovar = F,
                forecast = F,
                ncols = 1000)

  summaryoutput <- summaryoutput0$replist1

  fleetKey <-
    data.frame(Fleet = summaryoutput$fleet_ID,
               fleetName = summaryoutput$FleetNames)


  seldf <-
    summaryoutput$sizeselex %>%
    dplyr::filter(Factor == "Lsel" & year == year) %>%
    dplyr::select(-year,-Factor,-label) %>%
    reshape2::melt(id = c('Fleet', 'gender')) %>%
    dplyr::mutate(variable = as.numeric(as.character(variable)))

  seldf[, 'fleetName'] <-
    fleetKey$fleetName[match(seldf$Fleet, fleetKey$Fleet)]


  selp <- list()

  for (i in unique(seldf$Fleet)) {
    temp <- subset(seldf, Fleet == i)
    selp[[i]] <- ggplot2::ggplot(temp, aes(
      x = variable,
      y = value,
      col = factor(gender)
    )) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        title = element_text(size = rel(0.6)),
        axis.text = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        legend.position = c(0.9, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.6)),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      scale_x_continuous(limits = c(lmin, lmax),
                         breaks = seq(lmin, lmax, 50)) +
      ylim(0, 1) +
      scale_color_manual(
        values = c('dodgerblue3', 'goldenrod'),
        labels = c("Male", "Fem")
      ) +
      labs(
        title = paste0('L-based Selex for ', temp[1, 'fleetName'], ', ', year),
        y = 'Selectivity',
        x = 'Length (cm)'
      ) +
      geom_line(lwd = 1.2)
  } ## end selp

  ## save individual JPEGs
  for (s in 1:length(selp)) {
    ggplot2::ggsave(
      paste0(mods[m], "plots/Selex_",  seldf[seldf$Fleet == s, 'fleetName'][1], ".jpg"),
      plot = selp[[s]],
      width = 7,
      height = 5,
      units = 'in',
      dpi = 1020
    )
  }


  ## save one-page PDF
  ml <- gridExtra::marrangeGrob(selp, nrow=pdfrows, ncol=pdfcols)
  ## non-interactive use, multipage pdf
  ggplot2::ggsave(
    paste0(mods[m], "/plots/Selex_all.pdf"),
    ml,
    width = 8.5,
    height = 11,
    units = 'in'
  )
  graphics.off()
  cat("saved plot to ", paste0(rootdir, "/plots/Selex_all.pdf"), "\n")
  cat("set plotloc or change working dir if needed", "\n")


} ## end loop mods
} ## end function


## not run:
# plotInputSel(
#   rootdir = "G:/MAKO/mako_sim",
#   pattern = NA,
#   year = 2016,
#   lmin = 35,
#   lmax = 350,
#   linc = 5,
# pdfrows = 3,
# pdfcols = 2
# )
