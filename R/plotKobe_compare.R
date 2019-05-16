#' plotKobe_compare
#' terminal kobe plot. check out FLR.
#' @param rootdir home directory
#' @param mq_csv  optional; if you already generated a MQ csv using extractResults it will read from that, otherwise need pattern/subpattern
#' @param axes.limits x and y maximums for kobe plot, default to 2
#' @param terminal_year  Either a value or vector corresponding to the terminal year in each model.
#' @param kobe.type if 'ISC', will have pale orange coloring in place of orange/yellow
#' @param b.name the character column name of the biomass ref point; can rename to change y lab
#' @param f.name the character column name of the fishing ref point
#' @param pattern 1st order directory character
#' @param subpattern 2nd order directory character
#' @param saveplot logical, do you want to save the plot
#' @param plotloc where to save plot
#' @param doDistrib logical. Do you want to have FLR style distributions on outer margins? Only works if models run with hessian.
#' @param doLegend logical. do you want to write a legend (not advised for huge rep sets)



plotKobe_compare <- function(rootdir,
                             axes.limits = c(2,2),
                             terminal_year  = c(2017,2017,2016,2017),
                             kobe.type = c(NA,'ISC')[1],
                             mq_csv = NA,
                             b.name = NA,
                             f.name = NA,
                             pattern = NA,
                             subpattern = NA,
                             saveplot = T,
                             plotloc = NA,
                             doDistrib = FALSE,
                             doLegend = T) {

  ## plotting defaults
  graphics.off()

  if (is.na(plotloc)){
    if (!exists(paste0(rootdir, "/plots/"))){
      dir.create(paste0(rootdir, "/plots/")) ## make it
      plotloc <- paste0(rootdir, "/plots/") ## assign it
    }
    if (!exists(plotloc) {
      dir.create(plotloc) ## make it
    } ## end plotploc

  col.choices <- RColorBrewer::brewer.pal(8,'Dark2')

  ## iterate avail. runs and assign colors
  if(!is.na(pattern)){
    mods <- list.dirs(rootdir, recursive = F) %>%
      .[grepl(pattern, .)]
    cols <- c(rep(col.choices, max(c(10,length(mods)/10)),'black'))
  } else if (is.na(pattern)  | length(list.dirs(rootdir, recursive = F) %>%  .[grepl(pattern, .)])){ mods <- rootdir; cols <- 'black'}

  modnames <- NULL ## empty vector for model names

  if(doDistrib == TRUE){
    for (m in 1:length(mods)) {
      ## loop into master file
      modnames[m] <- basename(mods[m])
      ## use SS_output function to extract quantities
      if (!is.na(subpattern)) {
        ## if subpattern provided loop once more
        subdirs <- mods[m] %>%
          list.dirs(., recursive = T) %>%
          .[grepl(subpattern, .)] %>%
          .[!grepl("plots", .)]

        for (m in 1:length(subdirs)) {
          mtemp0 <- subdirs[m] %>%
            SS_output(.,
                      forecast = F)
          # with(mtemp$Kobe[nrow(mtemp$Kobe), ],
          #      points(
          #        B.Bmsy,
          #        F.Fmsy,
          #        pch = 25,
          #        bg = "grey88",
          #        col = cols[i]
          #      ))
        } ## end subdirs of mod
      } else if (is.na(subpattern)){
        mtemp <- mods[m] %>% SS_output(.,  forecast = F)
      } ## end is na subpattern

      ## write this unique model
      kb_hes <-
        getMVNhes(
          data = mtemp,
          pars = c('Bratio', 'F'),
          yr = ifelse(length(terminal_year) > 1, terminal_year[m], terminal_year),
          yr.cov = ifelse(length(terminal_year) > 1, terminal_year[m], terminal_year),
          mc = 10000
        )
      kbs <- mles <- NULL
      kb <- kb_hes$kb
      kb$run <-  modnames[m]
      mle <- kb_hes$mle #  Bratio, F/Fmsy
      mle <- data.frame(t(mle))
      mle$run <-  unique(kb$run)
      colnames(mle) <- c("stock","harvest","run")
      kbs <- rbind(kbs,kb)
      mles <- rbind(mles,mle)

      # Get Kobes
      pkb  <- kbs[kbs$run == modnames[m],1:2]
      pmle <- (mles[mles$run==modnames[m],1:2])
      trj <- get_trj(mtemp)
      png(file = paste0(plotloc,"KobeFLR_",modnames[m],".png"), width = 6.5, height = 5.5,
          res = 420, units = "in")
      kobe:::kobePhaseMar2(transform(kbs,run=paste(run))[,c("stock","harvest","run")],col =cols
                           ,xlab = expression(B/B[MSY]),ylab = expression(F/F[MSY]),ylim =quantile(kb[,2],0.999),xlim=c(2.5))
      dev.off()

    } ## end mods loop

  } else if(doDistrib == FALSE) {

    if (saveplot){
      jpeg(
        paste0(plotloc, "kobe_compare.jpg"),
        width = 8,
        height = 6,
        units = 'in',
        res = 420
      )} ## end saveplot
    par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), xpd = TRUE)

    # Kobe plot layout setting
    x_max = axes.limits[1]
    x_min = 0
    y_max = axes.limits[2]
    y_min = 0

    ## ensure OM is always black
    par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), xpd = TRUE)

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

    if(is.na(mq_csv)  | doDistrib == FALSE){
      for (m in 1:length(mods)) {
        ## loop into master file
        modnames[m] <- basename(mods[m])
        ## use SS_output function to extract quantities
        if (!is.na(subpattern)) {
          ## if subpattern provided loop once more
          subdirs <- mods[m] %>%
            list.dirs(., recursive = T) %>%
            .[grepl(subpattern, .)] %>%
            .[!grepl("plots", .)]

          for (m in 1:length(subdirs)) {
            mtemp0 <- subdirs[m] %>%
              SS_output(.,
                        forecast = F)
            with(mtemp$Kobe[nrow(mtemp$Kobe), ],
                 points(
                   B.Bmsy,
                   F.Fmsy,
                   pch = 25,
                   bg = "grey88",
                   col = cols[i]
                 ))
          } ## end subdirs of mod
        } else if (is.na(subpattern)){
          mtemp <- mods[m] %>% SS_output(.,  forecast = F)
        } ## end is na subpattern


      } ## end mods loop

      if (doLegend == T & !is.na(subpattern)) {
        legend(
          "topright",
          legend = modnames,
          pch = 19,
          col = rep(cols,ceiling(length(modnames)/length(cols)))
        )
      } ## end legend
      # dev.off()

      # graphics.off()
    } else if(!is.na(mq_csv)){
      df <- read.csv(mq_csv)
      for(i in 1:nrow(df)){
        if(is.na( df[i,b.name])  | is.na( df[i,f.name])) stop("Your biomass or F column in management_quantities is NA. \n Did your model(s) estimate F/FMSY or B/BMSY for the final year? Check SPRSeries.csv")

        with(df,
             points(
               df[i,b.name],
               df[i,f.name],
               pch = rep(21:25, length(as.numeric(df$MOD)))[i],
               col = factor(df$MOD)[i],
               bg = 'white',
               cex = 1.1
             ))
      } ## end looped points
      if (doLegend == T) {
        legend(
          "right",
          inset=c(-0,-0.9),
          legend = df$MOD,
          pch = rep(21:25, length(as.numeric(df$MOD))),
          col = factor(df$MOD),
          bg = 'white',
          cex = 1.1
        )
      } ## end legend

    } ## end !is.na(mq)
    graphics.off()
    cat('saved plot with model(s)',pattern," to ", plotloc,"\n")
    cat("set plotloc or change working dir if needed","\n")
  } ## end doDistrib == False
} ## end function

## not run
rootdir <- "C:/Users/Maia Kapur/Dropbox/UW/coursework/FISH-555/SA_Meeting_Final Runs-20190513T235227Z-001/SA_Meeting_Final Runs/Base_case"
plotKobe_compare(rootdir,
                 mq_csv = paste0(rootdir,"/results/management_quantities.csv"),
                 axes.limits = c(4,4),
                 kobe.type = 'ISC',
                 b.name = "SPB_SSBMSY",
                 f.name = 'F_FMSY',
                 pattern = NA,
                 subpattern = NA,
                 saveplot = T,
                 plotloc = paste0(rootdir,"/plots/"),
                 doDistrib = T,
                 doLegend = T)
