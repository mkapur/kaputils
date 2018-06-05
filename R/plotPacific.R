#' plotPacific
#' A ggplot basemap that is Pacific-centered. Defaults to full North Pacific
#' @param ew.breaks in 180-degree centered coordinates, provide your E/W limits and breaks
#' @param ns.breaks in 0-degree centered coordinates, provide your E/W limits and breaks; 0 is minimum
#' @param fillcol color to fill countries
#' @param bordercol color to outline countries
#' @param alpha optional transparency, defaults to 1


plotPacific <- function(ew.breaks = c(seq(-120,-170,-10),180,seq(170,80,-10)),
                        ns.breaks = seq(0,60,10),
                        fillcol = "grey88",
                        bordercol = "skyblue3",
                        alpha = 1) {
  ## load shapefiles
  WorldData <-
    map_data('world',
             wrap = c(-25, 335),
             orientation = c(20, 225)) %>% filter(region != 'Antarctica')
  WorldData <- fortify(WorldData)

  ## customize the x,y labels to have the degree symbol
  ewlbls <-
    unlist(lapply(ew.breaks, function(x)
      ifelse(
        x < 0, paste(-x, "°E"), ifelse(x > 0, paste(x, "°W"), x)
      )))
  ew.lims <- c(abs(ew.breaks)[1], last(ew.breaks)+200)

  ns.lims <- c(ns.breaks[1],last(ns.breaks))
  nslbls <- unlist(lapply(ns.breaks, function(x)
    paste(x, "°N")))

p <-  ggplot() +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = 'bottom'
    ) +
    ## add map
    geom_map(
      data = WorldData,
      map = WorldData,
      aes(
        x = long,
        y = lat,
        group = group,
        map_id = region
      ),
      fill = fillcol,
      colour = bordercol,
      size = 0.5
    ) +
    scale_x_continuous(
      limits = ew.lims,
      breaks = seq(ew.lims[1],ew.lims[2],10),
      labels = ewlbls,
      position = 'top'
    ) +
    scale_y_continuous(
      limits = ns.lims,
      breaks = ns.breaks,
      labels = nslbls,
      position = 'top'
    )
return(p)
}
