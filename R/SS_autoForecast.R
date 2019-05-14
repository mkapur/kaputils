#' SS_autoForecast
#' beta version of tool to automate catch-only update iterations and development of decision tables
#' hopefully port to r4ss when ready
#' @param rootdir  somewhere you'd like several models in folders created
#' @param basedir directory with executed base-case model
#' @param catch_proportions  a single or vector of values denoting the allocation proportiosn for each fleet in order matching .dat file; assuming F_relative opt 2
#' @param forecast_start the first year to forecast; assume inputs before this
#' @param forecast_end last year to forecast
#' @param fixed_catches a matrix of input fixed catches from the end of the original model to forecast_start
#' @param Flimitfraction a value or fector of same length as forecast period with P* values corresponding to a given year

SS_autoForecast <- function(rootdir,
                            basedir,
                            catch_proportions = c(0.5,0.08426184,0.4157382),
                            forecast_start = 2021,
                            forecast_end = 2031,
                            fixed_catches = catch_projections[1:4,5:7],
                            Flimitfraction = catch_projections$PSTAR_0.45[catch_projections$YEAR >2020]){
  devtools::source_url("https://raw.githubusercontent.com/r4ss/r4ss/development/R/SS_ForeCatch.R") ## use dev version
  devtools::source_url("https://raw.githubusercontent.com/mkapur/kaputils/master/R/SS_writeforecastMK.R") ## use dev version


  df <- data.frame()
  foreyrs <- forecast_end-forecast_start
  if(!exists(paste0(rootdir,"/forecasts"))) dir.create(paste0(rootdir,"/forecasts"))
  replist0 <- SS_output(paste0(rootdir,"/",basedir))

  ## error trapping
  if(length(catch_proportions) != replist0$nfleets) stop('catch_proportions should have a value for each fleet')
  # if(nrow(fixed_catches) != (forecast_start-1-inityr)) stop('fixed_catches should have a value for years before forecast_start')
  if(ncol(fixed_catches) != replist0$nfleets) stop('fixed_catches should have a value for each fleet')

  for(t in 1:foreyrs){
    base_temp <- paste0("forecasts/forecast", (t-1)+forecast_start)
    setwd(rootdir); if(exists(base_temp)) unlink(base_temp, force = TRUE)
    dir.create(base_temp)
    ## copy original files into new forecast folder
    file.copy(list.files(
      paste0(rootdir,"/",basedir),
      full.names = TRUE,
      recursive = TRUE), base_temp)

    setwd(base_temp)

    ## change init_src to 1 (read from .par)
    strt <- SS_readstarter(file = "starter.ss")
    strt$init_values_src <- 1
    strt$last_estimation_phase <- 10 ## could go as high as 20
    SS_writestarter(strt, file = "starter.ss", overwrite = TRUE)

    ## add zeroes to end of par file
    mpar <- readLines("ss3.par")
    LOI <- grep("Fcast",mpar)+1 ## get line(s) containing data after fcast
    NewLine <- strsplit(mpar[LOI],"0 ") ## split elements
    length(NewLine[[1]]);length(NewLine[[2]])

    for(a in 1:length(NewLine)){
      ltemp <- length(NewLine[[a]])
      NewLine[[a]][1:ltemp] <- " "
      NewLine[[a]][1:(ltemp+(forecast_start-2-replist0$endyr))] <- 0.000000000000 ## ! need to custom this 5
      mpar[LOI][a] = paste0(NewLine[[a]], collapse = " ")
    }
    NewLine <- strsplit(mpar[LOI],"0 ") ## split elements
    length(NewLine[[1]]);length(NewLine[[2]])
    writeLines(text=mpar, con="ss3.par") ## save it

    ## Step 4a. Add catch/projections through 2020.
    fore <- SS_readforecast(file = './forecast.ss',
                            Nareas = replist0$nareas,
                            Nfleets = replist0$nfleets,
                            version = paste(replist0$SS_versionNumeric),
                            readAll = TRUE)
    fore$Nforecastyrs <- forecast_end-replist0$endyr
    fore$FirstYear_for_caps_and_allocations <- forecast_start+(t-1)
    fore$Ncatch <- replist0$nfleets*(t+forecast_start-replist0$endyr-2)
    fore$InputBasis <- 2 ## discards

    ## Now Add Catch data/projections thru the year before forecast_start.
    ## This acts similarly to SS_ForeCatch except it reads directly from your inputs.
    inityr <- max(fore$ForeCatch$Year)
    for(k in 1:(forecast_start-1-inityr)){
      term <- nrow(fore$ForeCatch) ## intital final row
      for(i in 1:replist0$nfleets){
        fore$ForeCatch[term+i,'Year'] <- inityr+k
        fore$ForeCatch[term+i,'Seas'] <- 1
        fore$ForeCatch[term+i,'Fleet'] <- i
        fore$ForeCatch[term+i,'Catch_or_F'] <- fixed_catches[k,i]
      } ## end nfleets
    } ## end yrs to 2020



    ## Fix forecast file to end year selectivity
    fore$Bmark_years[1:6] <- 0
    fore$Fcast_years[1:4] <- 0
    ## Fix trawl relative F to reflect proportional catch amounts by fleet in forecast.
    fore$fleet_relative_F <- 2 ## will cause original r4ss write_forecast to fail
    fore$vals_fleet_relative_f <- paste(paste0(catch_proportions, collapse = " "))
    fore$basis_for_fcast_catch_tuning <- 2 ## dead biomass

    ##  Input correct buffer fraction for this year
    fore$Flimitfraction <- Flimitfraction[t]

    # Step 5b. Iterate the forecast file -- only if not first iter
    ## Find the total forecasted catch and OFL for specific years inthe "mod1" object generated by SS_output below.
    ## Allocate this catch among the fleets according to the given proportions
    ## add this to forecast file in increments
    if(t > 1){ ## add a single year of catch
      tempForeCatch <- SS_ForeCatch(mod1,
                                    yrs = 2021:(2021+(t-2)),
                                    average = FALSE,
                                    total = df$PredOFL[df$Year %in% (forecast_start+(t-2))]) ## total are the total catches for each year, given by OFLcatch

      fore$ForeCatch[(nrow(fore$ForeCatch)+1):(nrow(fore$ForeCatch)+nrow(tempForeCatch)),] <- tempForeCatch[,1:4]

    } ## end forecast if t > 1

    cat(paste0('Added forecast catch thru year ',forecast_start+(t-1),"\n"))

    ## save file
    SS_writeforecastMK(fore, file = './forecast.ss', overwrite = TRUE)
    ## execute model
    system('ss3 -nohess') ## works

    if(t == 1){
      mod1 <- SS_output(paste0(rootdir,"/forecasts/forecast2021"), covar = FALSE)
      YOI <- (replist0$endyr+1):(forecast_end-1); lYOI <- length(YOI)
      ## this will read the output of the first model and save the OFLs
      ## which will get used to comptue subsequent mods
      ## https://github.com/melmonk/StockAssessment_template/blob/master/8a_Tables.Rmd
      df[1:lYOI,"Year"] <- YOI
      df[1:lYOI,"PredOFL"] <- mod1$derived_quants[grep(paste0("ForeCatch_",YOI,collapse = "|"), mod1$derived_quants$Label),"Value"]
      df[1:lYOI,"ABCCatch"] <- NA ## placeholder
      ForecastC = mod1$timeseries[, grepl('Yr|retain[(]B[)]', names(mod1$timeseries))]
      ForecastC$total = rowSums(ForecastC[, -1])
      df[1:lYOI,"Landings"] <- subset(ForecastC, Yr %in% YOI)$total
      df[1:lYOI,"Age10+Biomass"] <- subset(mod1$timeseries[, c('Yr', 'Bio_smry')], Yr %in% YOI)$Bio_smry
      df[1:lYOI,"SpawnBio"] <-mod1$derived_quants[grep(paste0("SSB_",YOI,collapse = "|"), mod1$derived_quants$Label),"Value"]
      df[1:lYOI,"Depletion"] <- paste0(round(mod1$derived_quants[grep(paste0("Bratio_",YOI,collapse = "|"), mod1$derived_quants$Label),"Value"],3)*100,"%")
      for(i in YOI){ ## grab ovserved and/or forecast catch for all fleets f
        df$FiveYrAvgCatch[df$Year == i] <- mean(c(mod1$catch$Obs[mod1$catch$Yr %in% c(i:(i-5))],
                                                  mod1$derived_quants[grep(paste0("ForeCatch_",i:(i-5),collapse = "|"), mod1$derived_quants$Label),"Value"]))
      } ## end 5yr avg

    } ## end make df if t == 1
    if(t == 10){ ## when done, round and save
      mod10 <- SS_output(paste0(rootdir,"/forecasts/forecast2030"), covar = FALSE)

      df[1:lYOI,"ABCCatch"] <- mod10$sprseries$Retain_Catch_B[mod10$sprseries$Yr %in% YOI]

      df[,2:4] <- round(df[,2:4],2)
      write.csv(df,file =paste0(rootdir,"/forecasts/decision_table_base.csv"),row.names = FALSE)
    }
    cat(paste0('Executed model with forecast thru year ',forecast_start+(t-1),"\n"))

    # Step 5c. Iterate through 2030 -- the loop will continue making a new folder each time

  } ## end t loop
} ## end function

## not run:
# catch_projections <- read.csv("C:/Users/Maia Kapur/Dropbox/UW/chinarock_update/china_north_cproj.csv")
# SS_autoForecast(rootdir = "C:/Users/Maia Kapur/Dropbox/UW/chinarock_update",
#                 basedir = "base2015",
#                 catch_proportions = c(0.5,0.08426184,0.4157382),
#                 forecast_start = 2021,
#                 forecast_end = 2031,
#                 fixed_catches = catch_projections[1:4,5:7],
#                 Flimitfraction = catch_projections$PSTAR_0.45[catch_projections$YEAR >2020])
