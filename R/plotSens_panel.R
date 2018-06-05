#' plotSens_panel
#' a four-panel transparency shade plot for sensitivities
#' @param mq_csv optional; if you already generated a MQ csv using extractResults it will read from that, otherwise need pattern/subpattern
#' @param b.name the character column name of the biomass ref point; can rename to change y lab
#' @param f.name the character column name of the fishing ref point
#' @param pattern 1st order directory character
#' @param subpattern 2nd order directory character
#' @param plotloc where to save plot
#' @param doLegend logical. do you want to write a legend (not advised for huge rep sets)


plotSens_panel <- function(rootdir,
                           spr_csv = NA,
                           b.name = NA,
                           f.name = NA,
                           pattern = NA,
                           subpattern = NA,
                           plotloc = NA,
                           doLegend = T){

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

  ## check for cor file before continuing
  if(length(list.files(rootdir, recursive = T) %>%
    .[grepl('cor', .)]) == 0) stop(".cor file not found, can't do shaded sens plots \n run ss3 with hess = T")


  df <- data.frame()
  for (m in 1:length(mods)) {
    if(is.na(spr_csv)){

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

          df0 <-
            bind_cols(
              mtemp$derived_quants[grep("SSB_",mtemp$derived_quants$Label ),] %>%
                mutate('YEAR' = substr(Label,5,8),
                       SPB = Value/6,
                       SPB_SD = StdDev/6,
                       SPB_LCI = ifelse(SPB - SPB_SD*1.96 < 0 ,0, SPB - SPB_SD*1.96 ),
                       SPB_UCI =  SPB + SPB_SD*1.96 ) %>%
                select(YEAR, SPB, SPB_LCI, SPB_UCI) %>%
                filter(YEAR %in% 1975:2016) ,


              mtemp$derived_quants[grep("SPRratio_",mtemp$derived_quants$Label ),] %>%
                mutate('YEAR' = substr(Label,10,15),
                       ONE_SPR = Value,
                       ONE_SPRCI = 1.96*StdDev,
                       SPR_LCI = ifelse(ONE_SPR - ONE_SPRCI < 0 ,0, ONE_SPR - ONE_SPRCI ),
                       SPR_UCI =  ONE_SPR + ONE_SPRCI) %>%
                filter(YEAR %in% 1975:2016) %>%
                select( ONE_SPR, SPR_LCI, SPR_UCI) )   %>%
            mutate('MOD' = basename(mods[m]))
          df <- rbind(df,df0) ## bind model-specific to master
        } ## end subdirs of mod
      } ## end !is na subpattern
      else if(is.na(subpattern)){
        mtemp <- mods[m] %>%
          SS_output(.,
                    covar = F,
                    forecast = F,
                    ncols = 1000)

        df0 <-
          bind_cols(
            mtemp$derived_quants[grep("SSB_",mtemp$derived_quants$Label ),] %>%
              mutate('YEAR' = substr(Label,5,8),
                     SPB = Value/6,
                     SPB_SD = StdDev/6,
                     SPB_LCI = ifelse(SPB - SPB_SD*1.96 < 0 ,0, SPB - SPB_SD*1.96 ),
                     SPB_UCI =  SPB + SPB_SD*1.96 ) %>%
              select(YEAR, SPB, SPB_LCI, SPB_UCI) %>%
              filter(YEAR %in% 1975:2016) ,


            mtemp$derived_quants[grep("SPRratio_",mtemp$derived_quants$Label ),] %>%
              mutate('YEAR' = substr(Label,10,15),
                     ONE_SPR = Value,
                     ONE_SPRCI = 1.96*StdDev,
                     SPR_LCI = ifelse(ONE_SPR - ONE_SPRCI < 0 ,0, ONE_SPR - ONE_SPRCI ),
                     SPR_UCI =  ONE_SPR + ONE_SPRCI) %>%
              filter(YEAR %in% 1975:2016) %>%
              select( ONE_SPR, SPR_LCI, SPR_UCI) )   %>%
          mutate('MOD' = basename(mods[m]))
        df <- rbind(df,df0) ## bind model-specific to master

      }
    } ## end NA spr_csv
    else if(!is.na(spr_csv)){
      df <- read.csv(spr_csv)
    } ## end !is.na(spr_csv)

  } ## end mods loop
    ## plotting
    ## make master list -- needed for density plots
    modlist <- SSgetoutput(dirvec = mods, forecast = F, ncols = 1000) %>% SSsummarize(.)

    ## time series of SPB with shaded CIs
    sa <- ggplot(df, aes(x = as.numeric(YEAR), y = SPB,  group = MOD, fill = MOD)) +
      theme_minimal() +
      theme(panel.grid = element_blank(), legend.position = 'none',
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      labs(fill = "", col = "", y = "Stock Abundance", x = 'YEAR') +
      scale_color_manual(values = c(rainbow(n = length(unique(df$MOD))-1),'black')) +
      scale_fill_manual(values = c(rainbow(n = length(unique(df$MOD))-1),'black')) +
      scale_x_continuous(limits = c(1975,2016), breaks = c(seq(1980,2011,10),2016)) +
      geom_ribbon(aes(ymin = SPB_LCI, ymax = SPB_UCI, fill = MOD), alpha = 0.2) +
      geom_line(aes(col = MOD), lwd = 0.9)


    ## time series of 1-spr with shaded CIs
    spr <- ggplot(df, aes(x = as.numeric(YEAR), y = ONE_SPR,  group = MOD, fill = MOD)) +
      theme_minimal() +
      theme(panel.grid = element_blank(), legend.position = 'none', panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      labs(fill = "", col = "", y =  f.name, x = 'YEAR') +
      scale_color_manual(values = c(rainbow(n = length(unique(df$MOD))-1),'black')) +
      scale_fill_manual(values = c(rainbow(n = length(unique(df$MOD))-1),'black')) +
      # ylim(0,5) +
      scale_x_continuous(limits = c(1975,2016), breaks = c(seq(1980,2011,10),2016)) +
      geom_ribbon(aes(ymin = SPR_LCI, ymax =SPR_UCI), alpha = 0.2) +
      geom_line(aes(col = MOD), lwd = 0.9)

    tiff(file = paste0(plotloc,"sensPanel.tiff"),
         height = 8,
         width = 10,
         units = 'in',
         res = 800)

    par(mfrow = c(2,2))
    SSplotComparisons(modlist, subplot = c(14), legend = doLegend,legendloc = 'top',
                      plot = T, new = F, pch = NA,
                      densitynames = c("SR_LN(R0)"),
                      col =  c(rainbow(n = length(mods)-1),'black'),
                      shadealpha=0.2, legendlabels = c(basename(mods)) )

    SSplotComparisons(modlist, plot = T, new = F, subplot = 14, legend = F,
                      densitynames = c("SSB_Virgin"),
                      col =  c(rainbow(n = length(mods)-1),'black'),
                      shadealpha=0.2,
                      pch = NA,
                      densitytails = F,
                      legendlabels = c(basename(mods)) )


    ## custom plotting stuff to combine base & ggplots
    ## https://stackoverflow.com/questions/14124373/combine-base-and-ggplot-graphics-in-r-figure-window?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

    vp1.BottomLeft <-
      grid::viewport(
        height = unit(1, "npc"),
        width = unit(1, "npc"),
        just = c("right", "top"),
        y = 1,
        x = 1
      )
    vp1.BottomRight <-
      grid::viewport(
        height = unit(1, "npc"),
        width = unit(1, "npc"),
        just = c("left", "top"),
        y = 1,
        x = 1
      )

    plot.new()
    vps <- gridBase::baseViewports()
    grid::pushViewport(vps$figure)

    print(sa,vp = vp1.BottomLeft)
    print(spr,vp = vp1.BottomRight)
    graphics.off()


} ## end function
