#' SS_autoForecast
#' beta version of tool to automate catch-only update iterations and development of decision tables
#' hopefully port to r4ss when ready
#' @param rootdir  somewhere you'd like several models in folders created
#' @param basedir directory with executed base-case model
#' @param state one of low/med/high
#' @param catch_proportions  a single or vector of values denoting the allocation proportiosn for each fleet in order matching .dat file; assuming F_relative opt 2
#' @param forecast_start the first year to forecast; assume inputs before this
#' @param forecast_end last year to forecast
#' @param fixed_catches a matrix of input fixed catches from the end of the original model to forecast_start
#' @param Flimitfraction a value or fector of same length as forecast period with P* values corresponding to a given year
# devtools::install_github("r4ss/r4ss@2663227")
SS_autoForecast <- function(rootdir,
                            basedir,
                            state = 'base',
                            catch_proportions = c(0.5,0.08426184,0.4157382),
                            forecast_start = 2021,
                            forecast_end = 2031,
                            fixed_catches = catch_projections[1:4,5:7],
                            Flimitfraction = catch_projections$PSTAR_0.45[catch_projections$YEAR >2020]){
  devtools::source_url("https://raw.githubusercontent.com/r4ss/r4ss/development/R/SS_ForeCatch.R") ## use dev version
  devtools::source_url("https://raw.githubusercontent.com/mkapur/kaputils/master/R/SS_writeforecastMK.R") ## use dev version


  # source("C:/Users/mkapur/Dropbox/kaputils/R/SS_writeforecastMK.R")

  df <- data.frame()
  foreyrs <- forecast_end-forecast_start
  if(!exists(paste0(rootdir,"/forecasts"))) dir.create(paste0(rootdir,"/forecasts"))
  replist0 <- SS_output(paste0(rootdir,"/",basedir))

  ## error trapping
  if(length(catch_proportions) != replist0$nfishfleets) stop('catch_proportions should have a value for each fleet')
  # if(nrow(fixed_catches) != (forecast_start-1-inityr)) stop('fixed_catches should have a value for years before forecast_start')
  if(ncol(fixed_catches) != replist0$nfishfleets) stop('fixed_catches should have a value for each fleet')

  for(t in 1:foreyrs){

    base_temp <- paste0(rootdir,"/forecasts/forecast", (t-1)+forecast_start)
    setwd(rootdir); if(exists(base_temp)) unlink(  paste0(rootdir,"/",base_temp), force = TRUE)
    dir.create(base_temp)

    setwd(base_temp)

    ## copy original files into new forecast folder
    if(t == 1){
      file.copy(from = list.files(
        paste0(rootdir,"/",basedir),
        full.names = TRUE,
        recursive = TRUE),
        to = base_temp, overwrite = TRUE)
      ## change init_src to 1 (read from .par)
      strt <- SS_readstarter(file = "starter.ss")
      strt$init_values_src <- ifelse(state == 'base',1,0)
      strt$last_estimation_phase <- 10 ## could go as high as 20
      SS_writestarter(strt, file = "starter.ss", overwrite = TRUE)


      ## update CTL file with state of nature low/base/high (all fixed, reading from par)
      if(state != 'base'){
        mctl <- readLines(list.files(base_temp)[grep('_control', list.files(base_temp))])
        LOI <- grep("NatM_p_1_Fem_GP_1",mctl)[1] ## get line(s) containing data after natm, ignoring comment
        NewLine <- strsplit(mctl[LOI],"   ") ## split elements

        NewLine[[1]][3] <- ifelse(state == 'low', 0.05, ifelse(state == 'high', 0.08, 0.07))
        mctl[LOI][1] = paste0(NewLine[[1]], collapse = " ")

        writeLines(text=mctl, con= paste(list.files(base_temp)[grep('_control', list.files(base_temp))])) ## save it
      }
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
    }
    # list.files(
      #   paste0(rootdir,"/",base_temp),
      #   full.names = TRUE,
      #   recursive = TRUE), overwrite = TRUE)
## copy from previous year so as to retain proper catches
    if(t>1){
      # file.copy(list.files(
      #   paste0(rootdir,"/",paste0("forecasts/forecast2021")),
      #   full.names = TRUE,
      #   recursive = TRUE), to = base_temp, overwrite = TRUE)

      ## now get previous forecast only
      file.copy(list.files(
        paste0(rootdir,"/",paste0("forecasts/forecast",(forecast_start+t-2))),
        full.names = TRUE,
        recursive = TRUE)[grep('forecast.ss',list.files(
          paste0(rootdir,"/",paste0("forecasts/forecast",(forecast_start+t-2))),
          full.names = TRUE,
          recursive = TRUE))], to = base_temp, overwrite = TRUE)
    }

    ## Step 4a. Add catch/projections through given year. -- this will likely need to revert to MK version to 'build on' prev
    fore <- SS_readforecastMK(file = './forecast.ss',
                              Nareas = replist0$nareas,
                              Nfleets = replist0$nfishfleets,
                              nseas = 1,
                              version = paste(replist0$SS_versionNumeric),
                              readAll = TRUE)

    # fore <-  SS_readforecast(file = './forecast.ss',
    #                          Nareas = replist0$nareas,
    #                          Nfleets = replist0$nfishfleets,
    #                          nseas = replist0$nseasons,
    #                          version = paste(replist0$SS_versionNumeric),
    #                          readAll = TRUE)
    fore$Nforecastyrs <- forecast_end-replist0$endyr
    fore$FirstYear_for_caps_and_allocations <- forecast_start+(t-1)
    fore$Ncatch <- replist0$nfishfleets*(t+forecast_start-replist0$endyr-2)
    fore$InputBasis <- 2 ## discards
    fore$ControlRuleMethod <- 3 ## 3: ramp does catch=f(SSB), buffer on catch

    ## Now Add Catch data/projections thru the year before forecast_start.
    ## This acts similarly to SS_ForeCatch except it reads directly from your inputs.
    if(t == 1){
      inityr <- max(fore$ForeCatch$Year)
      for(k in 1:(forecast_start-1-inityr)){
        term <- nrow(fore$ForeCatch) ## intital final row
        for(i in 1:replist0$nfishfleets){
          fore$ForeCatch[term+i,'Year'] <- inityr+k
          fore$ForeCatch[term+i,'Seas'] <- 1
          fore$ForeCatch[term+i,'Fleet'] <- i
          fore$ForeCatch[term+i,'Catch_or_F'] <- fixed_catches[k,i]
        } ## end nfleets
      } ## end yrs to 2020
    }

    ## Fix forecast file to end year selectivity
    fore$Bmark_years[1:6] <- 0
    fore$Fcast_years[1:4] <- 0
    ## Fix trawl relative F to reflect proportional catch amounts by fleet in forecast.
    fore$fleet_relative_F <- 2 ## will cause original r4ss write_forecast to fail
    fore$vals_fleet_relative_f <- paste(paste0(catch_proportions, collapse = " "))
    fore$basis_for_fcast_catch_tuning <- 2 ## dead biomass

    ##  Input correct buffer fraction for this year -- won't matter if CTL rule method == 3
    fore$Flimitfraction <- Flimitfraction[t]

    # Step 5b. Iterate the forecast file -- only if not first iter
    ## Find the total forecasted catch for specific years in the "mod1" object generated by SS_output below.
    ## Allocate this catch among the fleets according to the given proportions
    ## add this to forecast file in increments
    if(t > 1){ ## add a single year of catch
      # if(t == 2)
      ## get previous model
      mod_prev <- SS_output(paste0(rootdir,"/forecasts/forecast",(forecast_start+(t-2))), covar = FALSE) ## just load once
      # foreCatch_thisyear <-  mod_prev$derived_quants[grep(paste0("ForeCatch_",(forecast_start+(t-2)),collapse = "|"), mod_prev$derived_quants$Label),"Value"]
      OFLCatch_thisyear <-  mod_prev$derived_quants[grep(paste0("OFLCatch_",(forecast_start+(t-2)),collapse = "|"), mod_prev$derived_quants$Label),"Value"]
      ## manually multiply OFL for this year by the buffer
      input_forecatch <- OFLCatch_thisyear*Flimitfraction[t]
      # modX <- SS_output(paste0(rootdir,"/forecasts/forecast",forecast_start+(t-1)), covar = FALSE) ## just load once
      # predOFLs_startForecast <-  mod_prev$derived_quants[grep(paste0("OFLCatch_",(forecast_start+(t-2)),collapse = "|"), mod_prev$derived_quants$Label),"Value"]

      # tempForeCatch <- SS_ForeCatch(mod1,
      #                               yrs = 2021:(2021+(t-2)), ## just do THIS year
      #                               average = FALSE,
      #                               total = predOFLs_startForecast)
      tempForeCatch <- SS_ForeCatch(mod_prev,
                                    yrs = forecast_start+(t-2), ## just do THIS year
                                    average = FALSE,
                                    total = input_forecatch)

                                    # total = df$PredOFL[df$Year %in% (forecast_start+(t-2))]) ## total are the total catches for each year, given by OFLcatch

      fore$ForeCatch[(nrow(fore$ForeCatch)+1):(nrow(fore$ForeCatch)+nrow(tempForeCatch)),] <- tempForeCatch[,1:4]
      if(t == 10){
        write.csv(fore$ForeCatch, file = "./tempForeCatch.csv",row.names = FALSE) ## save final year ABC catch
      }
    } ## end forecast if t > 1

    cat(paste0('Added forecast catch thru year ',forecast_start+(t-2),"\n"))

    ## save file
    SS_writeforecastMK(fore, file = './forecast.ss', overwrite = TRUE)
    ## execute model
    ## manual overwrite fleetrelF
    # if(execute == TRUE){
      system('ss3 -nohess') ## works
    # }

    # if(t == 10){
    #
    #   mod10 <- SS_output(paste0(rootdir,"/forecasts/forecast",forecast_end-1), covar = FALSE)
    #   YOI <- (replist0$endyr+1):(forecast_end); lYOI <- length(YOI)
    #   ## this will read the output of the first model and save the OFLs
    #   ## which will get used to comptue subsequent mods
    #   ## https://github.com/melmonk/StockAssessment_template/blob/master/8a_Tables.Rmd
    #   df[1:lYOI,"Year"] <- YOI
    #   df[1:lYOI,"PredOFL"] <-  mod10$derived_quants[grep(paste0("OFLCatch_",YOI,collapse = "|"), mod10$derived_quants$Label),"Value"]
    #   df[1:lYOI,"ForeCatch_ABC"] <- mod10$derived_quants[grep(paste0("ForeCatch_",YOI,collapse = "|"), mod10$derived_quants$Label),"Value"]
    #   endyrABC <- read.csv(paste0(rootdir,'/forecasts/forecast',forecast_end-1,"/tempForeCatch.csv")) ## the ABC which was used
    #   # df[1:lYOI,"ABC"] <-   endyrABC %>% filter(X.Year %in% YOI) %>% group_by(X.Year) %>% summarise(sumCatch = sum(dead.B.))
    #   ForecastC.dead = mod10$timeseries[, grepl('Yr|dead[(]B', names(mod10$timeseries))]
    #   ForecastC.dead$total = rowSums(ForecastC.dead[, -1])
    #   # ForecastC.ret = mod10$timeseries[, grepl('Yr|retain[(]B', names(mod10$timeseries))]
    #   # ForecastC.ret$total = rowSums(ForecastC.dead[, -1])
    #   df[1:lYOI,"ForecastC.dead"] <- subset(ForecastC.dead, Yr %in% YOI)$total ## should equal ForeCatch ABC
    #   # df[1:lYOI,"ForecastC.ret"] <- subset(ForecastC.ret, Yr %in% YOI)$total
    #   # df[1:lYOI,"ForecastC.dead+ret"] <- rowSums(cbind(df$ForecastC.dead,df$ForecastC.ret))
    #   df[1:lYOI,"Age10+Biomass"] <- subset(mod10$timeseries[, c('Yr', 'Bio_smry')], Yr %in% YOI)$Bio_smry
    #   df[1:lYOI,"SpawnBio"] <-mod10$derived_quants[grep(paste0("SSB_",YOI,collapse = "|"), mod10$derived_quants$Label),"Value"]
    #   df[1:lYOI,"Depletion"] <- paste0(round(mod10$derived_quants[grep(paste0("Bratio_",YOI,collapse = "|"), mod10$derived_quants$Label),"Value"],3)*100,"%")
    #   for(i in YOI){ ## grab ovserved and/or forecast catch for all fleets f
    #     df$FiveYrAvgCatch[df$Year == i] <- mean(c(mod10$catch$Obs[mod10$catch$Yr %in% c(i:(i-5))],
    #                                               mod10$derived_quants[grep(paste0("ForeCatch_",i:(i-5),collapse = "|"), mod10$derived_quants$Label),"Value"]))
    #   } ## end 5yr avg
    #   df$PredOFL[df$Year < forecast_start] <- df$ForeCatch_ABC[df$Year < forecast_start]<- NA
    #   df[,2:4] <- round(df[,2:4],2)
    #   write.csv(df,file =paste0(rootdir,"/forecasts/decision_table_base.csv"),row.names = FALSE)
    # }
    # cat(paste0('Executed model with forecast thru year ',forecast_start+(t-1),"\n"))

    # Step 5c. Iterate through 2030 -- the loop will continue making a new folder each time

  } ## end t loop
} ## end function



## not run testers
# compname = c('mkapur','maia kapur')[2]

# for(r in c('North','Central','South')){
#   for(state in c('low','base','high')){
#
#     rootdir.temp <- paste0("C:/Users/",compname,"/Dropbox/UW/assessments/china_2019_update/chinarock-update-2019/cr",r,"_ABC_",state)
#     catch_projections <- read.csv(paste0(rootdir.temp,"/cproj_",r,".csv"))
#
#     kaputils:::SS_autoForecast(rootdir = rootdir.temp,
#                                basedir = "base2015",
#                                catch_proportions = catch_projections[5,5:ncol(catch_projections)],
#                                state = state,
#                                forecast_start = 2021,
#                                forecast_end = 2031,
#                                fixed_catches = catch_projections[1:4,5:ncol(catch_projections)],
#                                Flimitfraction = catch_projections$PSTAR_0.45[catch_projections$YEAR >2020])
#     # read.csv(paste0(rootdir,"/forecasts/decision_table_base.csv"))
#   }
# }




# compname = c('mkapur','Maia Kapur')[2]
# rootdir.temp <- paste0("C:/Users/",compname,"/Dropbox/UW/assessments/china_2019_update/chinarock-update-2019/crNorth_ABC_low")
# catch_projections <- read.csv(paste0(rootdir.temp,"/cproj_North.csv"))
# rootdir = rootdir.temp
# basedir = "base2015"
# catch_proportions = catch_projections[7,5:ncol(catch_projections)]
# # catch_proportions = c(0.5,0.08426184,0.4157382),
# forecast_start = 2021
# forecast_end = 2031
# fixed_catches = catch_projections[1:4,5:ncol(catch_projections)]
# Flimitfraction = catch_projections$PSTAR_0.45[catch_projections$YEAR >2020]

