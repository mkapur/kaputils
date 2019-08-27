#' SS_autoForecast
#'
#' \code{SS_autoForecast} beta version of tool to automate catch-only update iterations
#' hopefully port to r4ss when ready
#' @param rootdir  somewhere you'd like the forecasting iteration in folders created, eg forecast202X
#' @param basedir directory with executed base-case model -- right now, assumed to be inside \code{rootdir}
#' @param state one of low/med/high -- only works for natural mortality at present
#' @param statesex  0, 1 or 2 for female, male or both
#' @param statevals a dataframe with columns corresponding to state (labeled low/medium/high), and optionally rows corresponding to Female and Male values
#' @param catch_proportions  a single or vector of values denoting the allocation proportions for each fleet in order matching .dat file; assuming \code{_fleet_relative_F == 2}
#' @param forecast_start the first year to forecast; assume inputs before this
#' @param forecast_end last year to forecast
#' @param fixed_catches a matrix of input fixed catches from the end of the original model to forecast_start
#' @param Flimitfraction a value or fector of same length as forecast period with P* values corresponding to a given year
#' @export
SS_autoForecast <- function(rootdir,
                            basedir,
                            state = state,
                            statesex = 1,
                            statevals = 0.05,
                            catch_proportions = c(0.5,0.08426184,0.4157382),
                            forecast_start = 2021,
                            forecast_end = 2031,
                            fixed_catches = catch_projections[1:4,5:7],
                            Flimitfraction = catch_projections$PSTAR_0.45[catch_projections$YEAR >2020]){
  devtools::source_url("https://raw.githubusercontent.com/r4ss/r4ss/development/R/SS_ForeCatch.R") ## use dev version
  # devtools::source_url("https://raw.githubusercontent.com/mkapur/kaputils/master/R/SS_writeforecastMK.R") ## use dev version
  ABC <- FORECATCH <- NULL
  if(state != 'base'){
    ## copy from base 2030; everything should be updated

    base_temp <- paste0(dirname(rootdir),"/ABC_",state)
    if(!exists(base_temp)) dir.create(base_temp)

    file.copy(list.files(
      paste0(dirname(rootdir),"/ABC_base/forecasts/forecast2030"),
      full.names = TRUE,
      recursive = TRUE), to = base_temp, overwrite = TRUE)

    ## update CTL file with state of nature low/base/high (all fixed, reading from par)

    setwd(base_temp)
    terms <- c("NatM_p_1_Fem_GP_1","NatM_p_1_Mal_GP_1")
    mctl <- readLines(list.files(base_temp)[grep('_control', list.files(base_temp))])
    if(statesex == 0  | statesex == 1){
     term <- terms[statesex+1]
     LOI <- grep(term,mctl)[1] ## get line(s) containing data after natm, ignoring comment
     NewLine <- strsplit(mctl[LOI],"   ") ## split elements
     NewLine[[1]][3] <-  statevals[1,state] ## there will only be 1 row if 1 sex is specified
     # NewLine[[1]][3] <- ifelse(state == 'low', 0.05, ifelse(state == 'high', 0.09, 0.07))
     mctl[LOI][1] = paste0(NewLine[[1]], collapse = " ")



    } else if(statesex == 2){ ## do both sequentially
      for(t in 1:length(terms)){
        LOI <- grep(terms[t],mctl)[1] ## get line(s) containing data after natm, ignoring comment
        NewLine <- strsplit(mctl[LOI], "\\s+")[[1]] ## split elements by any amt of whitespace
        NewLine[3] <-  statevals[t,state] ## select by sex and state
        mctl[LOI] <- paste0(NewLine, collapse = "\t")
      } ## end loop t over sexes

    } ## end if stateSex == 2

    writeLines(text=mctl, con= paste(list.files(base_temp)[grep('_control', list.files(base_temp))])) ## save it

    ## change init_src to 0 (read from .par)
    strt <- SS_readstarter(file = "starter.ss")
    strt$init_values_src <- ifelse(state == 'base',1,0)
    SS_writestarter(strt, file = "starter.ss", overwrite = TRUE)
    system('ss3 -nohess')

  } else{
    df <- data.frame()
    foreyrs <- forecast_end-forecast_start
    if(!exists(paste0(rootdir,"/forecasts"))) dir.create(paste0(rootdir,"/forecasts"))
    replist0 <- SS_output(paste0(rootdir,"/",basedir), covar = F)
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

      ## copy from previous year so as to retain proper catches
      if(t>1){
        file.copy(list.files(
          paste0(rootdir,"/",paste0("forecasts/forecast2021")),
          full.names = TRUE,
          recursive = TRUE), to = base_temp, overwrite = TRUE)

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
      fore <- SS_readforecast(file = './forecast.ss',
                                Nareas = replist0$nareas,
                                Nfleets = replist0$nfishfleets,
                                nseas = 1,
                                version = paste(replist0$SS_versionNumeric),
                                readAll = TRUE)

      fore$Nforecastyrs <- forecast_end-replist0$endyr
      fore$FirstYear_for_caps_and_allocations <- forecast_start+(t-1)
      fore$Ncatch <- replist0$nfishfleets*(t+forecast_start-replist0$endyr-2)
      fore$InputBasis <- 2 ## discards
      fore$ControlRuleMethod <- ifelse(replist0$SS_versionNumeric < 3.30,1,3) ## 3: ramp does catch=f(SSB), buffer on catch

      ## Now Add Catch data/projections thru the year before forecast_start.
      ## We want to overwrite everything because the 2015 etc catches are no longer projections, they are known
      ## This acts similarly to SS_ForeCatch except it reads directly from your inputs.
      if(t == 1){
        # inityr <- max(fore$ForeCatch$Year)
        inityr <- min(catch_projections$YEAR)-1
        if(inityr == Inf   | inityr == -Inf) inityr <- catch_projections$YEAR[1]-1 ## overwrite if INF
        fore$ForeCatch <- data.frame('Year' = NA, 'Seas' = NA,'Fleet' = NA, 'Catch_or_F' =NA) ## overwrite entire forecatch
        for(k in 1:(forecast_start-1-inityr)){
          # if(class(term) =='NULL') term <- 0
          term <-  ifelse(k == 1, 0, nrow(fore$ForeCatch) ) ## start from zero first time

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
        ## get previous model
        mod_prev <- SS_output(paste0(rootdir,"/forecasts/forecast",(forecast_start+(t-2))), covar = FALSE) ## just load once

        ## get what that model indicated for the terminal year in question. This is computed using buffer AND control rule
        OFLCatch_thisyear <-  mod_prev$derived_quants[grep(paste0("OFLCatch_",(forecast_start+(t-2)),collapse = "|"), mod_prev$derived_quants$Label),"Value"]
        ForeCatch_thisyear <-  mod_prev$derived_quants[grep(paste0("ForeCatch_",(forecast_start+(t-2)),collapse = "|"), mod_prev$derived_quants$Label),"Value"]

         ## manually multiply OFL for this year by the buffer -- this is the ABC, for records only
        ABC[t] <- OFLCatch_thisyear*Flimitfraction[t-1]

        ## input forecatch -- treated as gospel; save because rounding can distort
        input_forecatch <- FORECATCH[t] <- ForeCatch_thisyear


        tempForeCatch <- SS_ForeCatch(mod_prev,
                                      yrs = forecast_start+(t-2), ## just do THIS year
                                      average = FALSE,
                                      total = input_forecatch)

        # total = df$PredOFL[df$Year %in% (forecast_start+(t-2))]) ## total are the total catches for each year, given by OFLcatch

        fore$ForeCatch[(nrow(fore$ForeCatch)+1):(nrow(fore$ForeCatch)+nrow(tempForeCatch)),] <- tempForeCatch[,1:4]
      }
      cat(paste0('Added forecast catch thru year ',forecast_start+(t-2),"\n"))

      ## save file
      # SS_writeforecastMK(fore, file = './forecast.ss', overwrite = TRUE) ## load this if needed!
      SS_writeforecastMK(fore, file = './forecast.ss',
                       overwrite = TRUE)
      ## execute model
      ## manual overwrite fleetrelF
      if(t < foreyrs){
        system('ss3 -nohess') ## works
      } else if(t==foreyrs){
        system('ss3') ## run w hessian last time
      }

      ## after all have run, save csv with catch values
      if(t == 10){

        mod.2030 <- SS_output(getwd())
        iterOFL <- data.frame('MOD' = NA,'YEAR' = NA, 'OFL' = NA, 'FORECATCH_ACL' = NA,
                              'DEADBIO' = NA,
                              'REALIZEDBUFFER' = NA,
                              'TRUEBUFFER_045' =  NA,
                              'TRUEBUFFER_025' = NA,
                              "SUMMARYBIO" = NA,
                              'SPAWNBIO' = NA,
                              'DEPL' = NA) ## sigma 45)
        i <- 1
        ABC[1] <-  mod.2030$derived_quants[grep(paste0("OFLCatch_",2021,collapse = "|"), mod.2030$derived_quants$Label),"Value"]*Flimitfraction[1]
        FORECATCH_ACL[1] <- mod.2030$derived_quants[grep(paste0("ForeCatch_",2021,collapse = "|"), mod.2030$derived_quants$Label),"Value"] %>% round(.,5)
        for(y in 2021:2030){
          # iterOFL[i,'MOD'] <- paste0(basename(list.dirs(rd, recursive = F)[l]))
          iterOFL[i,'YEAR'] <- y
          iterOFL[i,'OFL'] <- ifelse(y > 2020, mod.2030$derived_quants[grep(paste0("OFLCatch_",y,collapse = "|"), mod.2030$derived_quants$Label),"Value"],NA)
          iterOFL[i,'ABC'] <- ABC[i-2020] #ifelse(y > 2020,round(iterOFL[i,'OFL']*c(1,1,Flimitfraction)[y-2018],5),NA)
          iterOFL[i,'FORECATCH_ACL'] <- FORECATCH[i-2020] #mod.2030$derived_quants[grep(paste0("ForeCatch_",y,collapse = "|"), mod.2030$derived_quants$Label),"Value"] %>% round(.,5)

          iterOFL[i,'DEADBIO'] <-  mod.2030$timeseries[, grepl('Yr|dead[(]B', names(mod.2030$timeseries))] %>% filter(Yr == y) %>% select(-Yr) %>% rowSums(.) %>% round(.,2)
          iterOFL[i,'TRUEBUFFER_045'] <- c(1,1,Flimitfraction)[y-2018]

          iterOFL[i,'REALIZEDBUFFER'] <-    round(iterOFL[i,'ABC']/iterOFL[i,'OFL'],3)
          iterOFL[i,'SUMMARYBIO'] <- mod.2030$timeseries[mod.2030$timeseries$Yr == y,"Bio_smry"]

          ## FOR 2019
          iterOFL[i,'SPAWNBIO'] <-      round(mod.2030$derived_quants[grep(paste0("SSB_",y,collapse = "|"),mod.2030$derived_quants$Label),"Value"],2)
          iterOFL[i,'DEPL'] <-    round(mod.2030$derived_quants[grep(paste0("Bratio_",y,collapse = "|"), mod.2030$derived_quants$Label),"Value"],2) # round(qlnorm(0.25,0,0.5*(1+c(1:10)*0.075)),3)[y-2020]


          i <- i+1
        } ## end yrs

       iterOFL %>% select(YEAR, OFL, ABC, FORECATCH_ACL, TRUEBUFFER_045, DEPL) %>% mutate(isless = (DEPL < .40 & FORECATCH_ACL < ABC)  | DEPL >= 0.4 & FORECATCH_ACL ==ABC)
        write.csv(.,
                  file = "./tempForeCatch_OFL_ABC_ACL.csv",row.names = FALSE) ## save final year ABC catch. Can manually extract ACL later
      }
      # } ## end forecast if t > 1


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
  } ## end if state == base
} ## end function

# if(catch != 'ABC'  | state != 'base'){
#   tempdir <- paste0("C:/Users/",compname,"/Dropbox/UW/assessments/china_2019_update/chinarock-update-2019/cr",r,"_",catch,"_",state)
# } else if(catch == 'ABC'){
#   tempdir <- paste0("C:/Users/",compname,"/Dropbox/UW/assessments/china_2019_update/chinarock-update-2019/cr",r,"_",catch,"_",state,"/forecasts/forecast2030")
# }
# mod <- SS_output(tempdir, covar = F)
#
# mod$derived_quants[grep(paste0("SSB_",YOI,collapse = "|"),
#                         mod$derived_quants$Label),"Value"]
# ## not run testers
# compname = c('mkapur','maia kapur')[1]

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
# rootdir.temp <- rootdir <- paste0("C:/Users/",compname,"/Dropbox/UW/assessments/blackgill-2019-update/ABC_base")
# catch_projections <- read.csv(paste0(rootdir.temp,"/blackgill_proj.csv"))
# rootdir = rootdir.temp
# state = 'base'
# statesex = 2
# basedir = "base_2015"
# catch_proportions = catch_projections[catch_projections$YEAR == 2021,5:ncol(catch_projections)]
# forecast_start = 2021
# forecast_end = 2031
# fixed_catches = catch_projections[catch_projections$YEAR < 2021,5:ncol(catch_projections)]
# Flimitfraction = catch_projections$PSTAR_0.45[catch_projections$YEAR >2020]

# cr.statevals <- data.frame(matrix(NA, ncol = 3, nrow = 1))
# colnames(cr.statevals) <- c('low','base','high')
# row.names(cr.statevals) <- c('Fem')
# cr.statevals$low <- c(0.05)
# cr.statevals$base <- c(0.07)
# cr.statevals$high <- c(0.09)
# statevals <- cr.statevals
# rootdir.temp <- rootdir <- paste0("C:/Users/",compname,"/Dropbox/UW/assessments/china_2019_update/chinarock-update-2019/crNorth_ABC_high")
# catch_projections <- read.csv(paste0("C:/Users/",compname,"/Dropbox/UW/assessments/china_2019_update/chinarock-update-2019/crNorth_ABC_base/cproj_North.csv"))
# rootdir = rootdir.temp
# state = 'high'
# statesex = 1
# basedir = "base2015"
# catch_proportions = catch_projections[catch_projections$YEAR == 2021,5:ncol(catch_projections)]
# forecast_start = 2021
# forecast_end = 2031
# fixed_catches = catch_projections[catch_projections$YEAR < 2021,5:ncol(catch_projections)]
# Flimitfraction = catch_projections$PSTAR_0.45[catch_projections$YEAR >2020]

