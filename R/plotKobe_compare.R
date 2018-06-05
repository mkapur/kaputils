#' plotKobe_compare
#' terminal kobe plot. check out FLR.
#' @param rootdir home directory
#' @param mq_csv  optional; if you already generated a MQ csv using extractResults it will read from that, otherwise need pattern/subpattern
#' @param b.name the character column name of the biomass ref point; can rename to change y lab
#' @param f.name the character column name of the fishing ref point
#' @param pattern 1st order directory character
#' @param subpattern 2nd order directory character
#' @param saveplot logical, do you want to save the plot
#' @param plotloc where to save plot
#' @param doLegend logical. do you want to write a legend (not advised for huge rep sets)



plotKobe_compare <- function(rootdir,
                             mq_csv = NA,
                             b.name = NA,
                             f.name = NA,
                             pattern = NA,
                             subpattern = NA,
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

  col.choices <- RColorBrewer::brewer.pal(10,'Spectral')

  ## iterate avail. runs and assign colors
  if(!is.na(pattern)){
    mods <- list.dirs(rootdir, recursive = F) %>%
      .[grepl(pattern, .)]
    cols <- c(rep(col.choices, (length(mods)/10),'black'))
  } else if (is.na(pattern)){ mods <- rootdir; cols <- 'black'}

  if (saveplot){
    jpeg(
      paste0(plotloc, "kobe_compare.jpg"),
      width = 8,
      height = 6,
      units = 'in',
      res = 1020
    )} ## end saveplot

  par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

  # Kobe plot layout setting
  x_max = 2
  x_min = 0
  y_max = 5
  y_min = 0

  modnames <- NULL ## empty vector for model names
  ## ensure OM is always black

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

  for (m in 1:length(mods)) {
    if(is.na(mq_csv)){

      ## loop into master file
      modnames[m] <- basename(mods[m])
      ## use SS_output function to extract quantities
      if (!is.na(subpattern)) {
        ## if subpattern provided loop once more
        subdirs <- mods[m] %>%
          list.dirs(., recursive = T) %>%
          .[grepl(subpattern, .)]
        for (s in 1:length(subdirs)) {
          mtemp <- subdirs[s] %>%
            SS_output(.,
                      covar = F,
                      forecast = F,
                      ncols = 1000)
          with(mtemp$Kobe[nrow(mtemp$Kobe), ],
               points(
                 B.Bmsy,
                 F.Fmsy,
                 pch = 25,
                 bg = "grey88",
                 col = cols[i]
               ))
        } ## end subdirs of mod
      } ## end !is na subpattern
    if (doLegend == T & !is.na(subpattern)) {
      legend(
        "topright",
        legend = modnames,
        pch = 19,
        col = rep(cols,ceiling(length(modnames)/length(cols)))
      )
    } ## end legend
    dev.off()
    cat('saved plot with model(s)',pattern," to ", plotloc,"\n")
    cat("set plotloc or change working dir if needed","\n")
    graphics.off()
  } ## end NA mq_csv
  else if(!is.na(mq_csv)){
    df <- read.csv(mq_csv)
    for(i in 1:nrow(df)){
      with(df,
           points(
             df[i,b.name],
             df[i,f.name],
             pch = 19,
             col = cols[i]
           ))
    } ## end looped points
    if (doLegend == T) {
      legend(
        "topright",
        legend = df$MOD,
        pch = 19,
        col = rep(cols,ceiling(length(df$MOD)/length(cols)))
      )
    } ## end legend
    graphics.off()
  } ## end !is.na(mq)
  } ## end mods loop
  } ## end function

## not run
# plotKobe_compare(
#   rootdir = "G:\\MAKO\\mako_sim",
#   mq_csv = "G:\\MAKO\\mako_sim\\results\\management_quantities.csv",
#   b.name = 'SPB_SSBMSY',
#   f.name = 'F_FMSY',
#   pattern = "MAK_",
#   subpattern = NA,
#   plotloc = "G:\\MAKO\\mako_sim\\plots\\",
#   saveplot = T,
#   doLegend = F)
