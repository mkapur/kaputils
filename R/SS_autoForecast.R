#' SS_autoForecast
#' beta version of tool to automate catch-only update iterations and development of decision tables
#' hopefully port to r4ss when ready
#' @param nsim  Number of simulated effort datasets
#' @param Esd  optional; if you already generated a MQ csv using extractResults it will read from that, otherwise need pattern/subpattern
#' @param nyears the character column name of the biomass ref point; can rename to change y lab
#' @param dFmin the character column name of the fishing ref point
#' @param dFmax 1st order directory character
#' @param bb 2nd order directory character
#' @param scale scaling for median F

## set your WD somewhere you'd like several models
rootdir <- "C:/Users/Maia Kapur/Dropbox/UW/chinarock_update"
devtools::source_url("https://raw.githubusercontent.com/r4ss/r4ss/development/R/SS_ForeCatch.R")


# source(paste0(rootdir,"/SS_writeforecastMK.R")); source(paste0(rootdir,"/forecatch_dev.R"))
# source(paste0(rootdir,"/SS_decision_table_stuff.R"))
catch_projections <- read.csv(paste0(rootdir,"/china_north_cproj.csv"))


## Step 1. First, take the base model input files from the assessment
# This includes: .dat, .ctl, forecast, starter, ss3.exe, .par
# (I will provide these, and you will also need: Report.sso)
basedir_executed <- "base2015"#; system("ss3") ## if you haven't executed yet
mod <- SS_output(paste0(rootdir,"/",basedir_executed))
basedir <- "base2015_inputfiles"
## Check spawning biomass and total like (there are other ways to extract)
# mod$derived_quants[mod$derived_quants$Label == "SSB_Virgin",]
# mod$derived_quants[mod$derived_quants$Label == "SSB_2013",]
# mod$likelihoods_used[[1]]


## Step 2 Change first numerical term in Starter file to 1
## I like to copy into a new folder just in case -- this will paste EVERYTHING from basedir into base_1
df <- data.frame()

for(t in 1:10){
  base_temp <- paste0("forecasts/forecast", (t-1)+2021)
  setwd(rootdir); if(exists(base_temp)) unlink(base_temp, force = TRUE)
  dir.create(base_temp)
  file.copy(list.files(
    paste0(rootdir,"/",basedir),
    full.names = TRUE,
    recursive = TRUE), base_temp)


  ## execute the changed model -- on a mac use "shell" instead of "system"
  setwd(base_temp)
  strt <- SS_readstarter(file = "starter.ss")
  strt$init_values_src <- 1
  strt$last_estimation_phase <- 10 ## could go as high as 20
  SS_writestarter(strt, file = "starter.ss", overwrite = TRUE)
  # system("ss3 -nohess") ## testing OK

  ## Steps 3. Par File - add zeroes
  # file.copy("C:/Users/mkapur/Dropbox/UW/chinarock_update/base2015_inputfiles/ss3.par",
  # "ss3.par", overwrite = TRUE)

  mpar <- readLines("ss3.par")
  LOI <- grep("Fcast",mpar)+1 ## get line(s) containing data after fcast
  NewLine <- strsplit(mpar[LOI],"0 ") ## split elements
  length(NewLine[[1]]);length(NewLine[[2]])

  for(a in 1:length(NewLine)){
    ltemp <- length(NewLine[[a]])
    NewLine[[a]][1:ltemp] <- " "  ## wipe all
    NewLine[[a]][1:(ltemp+(2019-mod$endyr))] <- 0.000000000000 ## add  zeroes to end
    mpar[LOI][a] = paste0(NewLine[[a]], collapse = " ")
  }
  NewLine <- strsplit(mpar[LOI],"0 ") ## split elements
  length(NewLine[[1]]);length(NewLine[[2]])
  writeLines(text=mpar, con="ss3.par") ## save it
  # system("ss3 -nohess")

  ## Step 4a. Add catch/projections through 2020.
  fore <- SS_readforecast(file = './forecast.ss',
                          Nareas = mod$nareas,
                          Nfleets = mod$nfleets,
                          version = '3.24',
                          readAll = TRUE)
  fore$Nforecastyrs <- 2031-mod$endyr
  fore$FirstYear_for_caps_and_allocations <- 2021+(t-1)
  fore$Ncatch <- 15+(3*t) ## check this
  fore$InputBasis <- 2

  ## Now Add Catch data/projections thru 2020 if this is base_1.
  ## Otherwise this gets copied.
  inityr <- max(fore$ForeCatch$Year)
  for(k in 1:(2020-inityr)){
    term <- nrow(fore$ForeCatch) ## intital final row
    for(i in 1:mod$nfleets){
      fore$ForeCatch[term+i,'Year'] <- inityr+k
      fore$ForeCatch[term+i,'Seas'] <- 1
      fore$ForeCatch[term+i,'Fleet'] <- i
      fore$ForeCatch[term+i,'Catch_or_F'] <- catch_projections[catch_projections$YEAR == inityr+k,5+(i-1)]
    } ## end nfleets
  } ## end yrs to 2020

  # Step 5b. Iterate the forecast file -- only if not first iter
  ## Find the total forecasted catch and OFL for specific years in the Report.sso file
  ## OR use the "mod1" object generated by SS_output below.
  ## Allocate this catch among the fleets according to the given proportions
  ## add this to forecast file in increments

  if(t > 1){ ## add a single year of catch
    tempForeCatch <- SS_ForeCatchDEV(mod1,
                                     yrs = 2021:(2021+(t-2)),
                                     average = FALSE,
                                     total = df$PredOFL[df$Year %in% (2021+(t-2))]) ## total are the total catches for each year, given by OFLcatch

    fore$ForeCatch[(nrow(fore$ForeCatch)+1):(nrow(fore$ForeCatch)+nrow(tempForeCatch)),] <- tempForeCatch[,1:4]

  } ## end forecast if t > 1

  cat(paste0('Added forecast catch thru year ',2021+(t-1),"\n"))

  ## Steps 4.b. fix forecast file to end year selectivity
  fore$Bmark_years[1:6] <- 0
  fore$Fcast_years[1:4] <- 0

  ## Step 4.d. Fix trawl relative F to reflect proportional catch amounts by fleet in forecast.
  fore$fleet_relative_F <- 2 ## will cause original r4ss write_forecast to fail
  fore$vals_fleet_relative_f <- paste(paste0(catch_projections[catch_projections$YEAR == 2021+(t-1),5:7]), collapse = " ")
  fore$basis_for_fcast_catch_tuning <- 2 ## deadbiomass

  ## Step 5a. Input correct buffer fraction
  fore$Flimitfraction <- catch_projections$PSTAR_0.45[catch_projections$YEAR == 2021+(t-1)]
  ## save file
  SS_writeforecastMK(fore, file = './forecast.ss', overwrite = TRUE)
  ## execute model
  system('ss3 -nohess') ## works

  if(t == 1){
    mod1 <- SS_output(paste0(rootdir,"/forecasts/forecast2021"), covar = FALSE)
    YOI <- 2015:2030
    ## this will read the output of the first model and save the OFLs
    ## which will get used to comptue subsequent mods
    ## https://github.com/melmonk/StockAssessment_template/blob/master/8a_Tables.Rmd
    df[1:16,"Year"] <- 2015:2030
    df[1:16,"PredOFL"] <- mod1$derived_quants[grep(paste0("ForeCatch_",2015:2030,collapse = "|"), mod1$derived_quants$Label),"Value"]
    ForecastC = mod1$timeseries[, grepl('Yr|retain[(]B[)]', names(mod1$timeseries))]
    ForecastC$total = rowSums(ForecastC[, -1])
    df[1:16,"Landings"] <- subset(ForecastC, Yr %in% YOI)$total
    df[1:16,"Age10+Biomass"] <- subset(mod1$timeseries[, c('Yr', 'Bio_smry')], Yr %in% YOI)$Bio_smry
    df[1:16,"SpawnBio"] <-mod1$derived_quants[grep(paste0("SSB_",2015:2030,collapse = "|"), mod1$derived_quants$Label),"Value"]
    df[1:16,"Depletion"] <- paste0(round(mod1$derived_quants[grep(paste0("Bratio_",2015:2030,collapse = "|"), mod1$derived_quants$Label),"Value"],3)*100,"%")
    for(i in 2015:2030){ ## grab ovserved and/or forecast catch for all fleets f
      df$FiveYrAvgCatch[df$Year == i] <- mean(c(mod1$catch$Obs[mod1$catch$Yr %in% c(i:(i-5))],
                                                mod1$derived_quants[grep(paste0("ForeCatch_",i:(i-5),collapse = "|"), mod1$derived_quants$Label),"Value"]))
    } ## end 5yr avg

  } ## end make df if t == 1
  if(t == 10){ ## when done, round and save
    df[,2:4] <- round(df[,2:4],2)
    write.csv(df,file =paste0(rootdir,"/decision_table_base.csv"),row.names = FALSE)
  }
  cat(paste0('Executed model with forecast thru year ',2021+(t-1),"\n"))

  # Step 5c. Iterate through 2030 -- the loop will continue making a new folder each time

}