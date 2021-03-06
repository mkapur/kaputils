#' plotInputSel
#' \code{plotInputSel} parsing of input selectivity plots to show by-fleet, by-Sex curves as proposed in SS3 Control File
#' @param moddir filepath where Report.sso is stored
#' @param write logical. should plots be saved to model directory
#' @param type type of file to be saved
#' @export
plotInputSel <- function(rootdir,
                         pattern = NA,
                         year = 2016,
                         seltype = c('age','length')[1],
                         plotloc = NA,
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
  } ## end plot loc

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

  ## skip if it's just directory with folders inside
  moddrop <- NA
  for(m in 1:length(mods)){
    if(length(list.dirs(mods[m], recursive = F)) > 0)  moddrop[m] <- m
  }
  moddrop <- moddrop[!is.na(moddrop)]
  if(length(moddrop >0)) mods <- mods[-moddrop]

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

  if(seltype == 'length') {
    seldf0 <-  summaryoutput$sizeselex
    # if('Seas' %in% names(seldf0))   dropList

    seldf <- seldf0 %>%
      plyr::rename(c('Factor'='fctr')) %>% ## breaks if factor
      dplyr::filter(fctr == "Lsel" & Yr == year) %>%
      dplyr::select(-fctr,  -Label ) %>%
      reshape2::melt(id = c('Fleet', 'Sex')) %>%
      dplyr::mutate(variable = as.numeric(as.character(variable))) %>%
      filter(complete.cases(.))

  }

  if(seltype == 'age'){
    seldf0 <- summaryoutput$ageselex

    seldf <- seldf0  %>%
      plyr::rename(c('Factor'='fctr')) %>% ## breaks if factor
      dplyr::filter(fctr == "Asel" & Yr == max(.$Yr)) %>%
      dplyr::select(-Yr,-fctr,-Label,-Seas,-Morph) %>%
      reshape2::melt(id = c('Fleet', 'Sex')) %>%
      dplyr::mutate(variable = as.numeric(as.character(variable)))
  }



  seldf[, 'fleetName'] <- fleetKey$fleetName[match(seldf$Fleet, fleetKey$Fleet)]


  selp <- list()

  for (i in unique(seldf$Fleet)) {
    temp <- subset(seldf, Fleet == i)
    lpos <-
    selp[[i]] <- ggplot2::ggplot(temp, aes(
      x = variable,
      y = value,
      col = factor(Sex)
    )) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        title = element_text(size = rel(0.6)),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.position = ifelse(length(unique(temp$Sex)) > 1, c(0.9, 0.9), 'none'),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.6)),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      # scale_x_continuous(limits = c(lmin, lmax),
      #                    breaks = seq(lmin, lmax, linc)) +
      ylim(0, 1.1) +
      scale_color_manual(
        values = c('dodgerblue3', 'goldenrod'),
        labels = c("Male", "Fem")
      ) +
      labs(
        title = ifelse(seltype == 2,
                       paste0('L-based Selex for ', temp[1, 'fleetName'], ', ', year),
                       paste0('A-based Selex for ', temp[1, 'fleetName'], ', ', year)),
        y = 'Selectivity',
        x = ifelse(seltype == 2,'Length (cm)','Age (yrs)')
      ) +
      geom_line(lwd = 1.2)
  } ## end selp

  ## save individual JPEGs
  for (s in 1:length(selp)) {
    ggplot2::ggsave(
      paste0(plotloc,"Selex_",  seldf[seldf$Fleet == s, 'fleetName'][1], ".jpg"),
      plot = selp[[s]],
      width = 7,
      height = 5,
      units = 'in',
      dpi = 720
    )
  }


  ## save one-page PDF
  ml <- gridExtra::marrangeGrob(selp, nrow=pdfrows, ncol=pdfcols)
  ## non-interactive use, multipage pdf
  ggplot2::ggsave(
    paste0( mods[m], "/Selex_all.pdf"),
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

# plotInputSel(rootdir = "C:/users/mkapur/dropbox/HAKEBOOT/",
#              pattern = "HAKE_",
#              plotloc = "C:/users/mkapur/dropbox/HAKEBOOT/plots/",
#              year = 2016,
#              lmin = 0,
#              lmax = 50,
#              linc = 5,
#              seltype = c('age','length')[2],
#              pdfrows = 2,
#              pdfcols = 2)
