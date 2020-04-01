#' extract_results
#' /code{extract_results} custom wrapper of to extract reference points nested SS3 simulations
#' @param rootdir root filepath where all subdirectories containing Report.sso are stored
#' @param terminal_year last year for reference pt extraction
#' @param suffix optional tag appended to end of CSV filenames; defaults to subpattern or pattern
#' @param pattern a string that specifically matches all directories with report files of interest.
#' @param subpattern type of file to be saved
#' @param writeTables logical. Should a csv of results be saved?
#' @export

extractResults <- function(rootdir,  terminal_year = 2015,   suffix = NA,
                           pattern = NA,  subpattern = NA,
                           writeTables = T){

  suff <- ifelse(is.na(suffix), ifelse(is.na(subpattern), pattern, subpattern), suffix)

  ## iterate avail. runs
  if (!is.na(pattern)) {
    mods <- list.dirs(rootdir, recursive = F, full.names = T) %>%
      .[grepl(pattern, .)]
  }  else{
    mods <- rootdir
  }

  # refList = data.frame(
  #   "MOD" = NA,
  #   "REP" = NA,
  #   "F_FMSY" = NA,
  #   "SPB_SSBMSY" = NA
  # )
  if (!exists(paste0(rootdir, "/results/")))
    dir.create(paste0(rootdir, "/results/"))


  if (!is.na(pattern) & is.na(subpattern)) {
    ## just extract all at once

    mtemp <- lapply(mods, SS_output) %>%
      SSsummarize()

    names(mtemp$quants)[1:length(mods)] <- basename(mods)
    names(mtemp$likelihoods)[1:length(mods)] <- basename(mods)
    names(mtemp$quantsSD)[1:length(mods)] <- paste(basename(mods))

    refListSD <-  mtemp$quantsSD %>%
      melt(id = c('Yr', 'Label')) %>%
      filter(is.na(.$Yr)) %>%
      # mutate(Label2 = gsub("_.*", "", Label) ,
      #        idcol  = paste0(variable, Label)) %>%
      pivot_wider(
        .,
        names_from = Label,
        # id_cols = idcol,
        values_from =  c(value)
      ) %>%
      mutate(
      #   'Yr' = NA,
        'MOD' = suff,
      #   "IDX" = NA,
        "REP" = paste0(suff,str_sub(variable, 6, -1))
      ) %>%
      select( -variable, -Yr)
    names(refListSD)[1:20] <- paste0('StdDev_', names(refListSD)[1:20])


    refListVal <-  mtemp$quants %>%
      melt(id = c('Yr', 'Label')) %>%
      filter(is.na(.$Yr)) %>%
      # mutate(Label2 = gsub("_.*", "", Label) ,
      #        idcol  = paste0(variable, Label)) %>%
      pivot_wider(
        .,
        names_from = Label,
        # id_cols = idcol,
        values_from =  c(value)
      ) %>%
      mutate(
        # 'Yr' = NA,
        'MOD' = suff,
        # "IDX" = NA,
        "REP" = paste0(suff,str_sub(variable, 6, -1))
      ) %>%
      select(-Yr, -MOD, REP, everything(), -variable)
    names(refListVal)[1:20] <- paste0('Value_',names(refListVal)[1:20])

    refList <- merge(refListVal, refListSD, by = 'REP') %>% mutate(MOD = MOD.x) %>% select(-MOD.x, -MOD.y) %>% select( MOD, REP, everything())

    mqsSD <-  mtemp$quantsSD %>%
      melt(id = c('Yr', 'Label')) %>%
      filter(!is.na(.$Yr)) %>%
      mutate(Label2 = gsub("_.*", "", Label) ,
             idcol  = paste0(variable, Yr)) %>%
      pivot_wider(
        .,
        names_from = Label2,
        id_cols = idcol,
        values_from = value
      ) %>%
      mutate(
        'Yr' = str_sub(idcol, -4, -1),
        'MOD' = suff,
        "IDX" = NA,
        "REP" = paste0(suff,str_sub(idcol, 6, -5))
      ) %>%
      select(-IDX, -idcol, -MOD) %>%
      select(Yr,  REP, everything()) #
    names(mqsSD)[3:7] <- paste0('StdDev_',   names(mqsSD)[3:7])

   mqsVal <-  mtemp$quants %>%
      melt(id = c('Yr', 'Label')) %>%
      filter(!is.na(.$Yr)) %>%
      mutate(Label2 = gsub("_.*", "", Label) ,
             idcol  = paste0(variable, Yr)) %>%
      pivot_wider(
        .,
        names_from = Label2,
        id_cols = idcol,
        values_from = value
      ) %>%
      mutate(
        'Yr' = str_sub(idcol, -4, -1),
        'MOD' = suff,
        "IDX" = NA,
        "REP" = paste0(suff,str_sub(idcol, 6, -5))
      ) %>%
      select(Yr, MOD, REP, IDX, everything(), -idcol) #%>%
   names(mqsVal)[5:9] <- paste0('Value_',   names(mqsVal)[5:9])

   mqs <- merge(mqsVal,mqsSD, by = c('Yr','REP')) %>%
     select(Yr, MOD, REP, IDX, everything())
     #  merge(., refList, by.x = c('REP'), by.y = c('REP'), all.y = FALSE) %>%
     #  mutate(Yr = Yr.x, MOD = suff, IDX = NA) %>%
     #  select(-Yr.y, -Yr.x, -IDX.y, -IDX.x, -MOD.y, -MOD.x) %>%
     # select(Yr, MOD, IDX, REP, everything())

   mls <-  mtemp$likelihoods %>%
      melt(id = "Label") %>%
      pivot_wider(
        .,
        names_from = Label,
        id_cols = variable,
        values_from = value
      ) %>%
      mutate(
        'MOD' = suff,
        "IDX" = NA,
        "REP" = sub('OMRep',"",variable)
      ) %>%
      select(MOD, REP, everything(),-variable)


   if(writeTables == TRUE){
     write.csv(
       mqs,
       paste0(rootdir, "./results/management_quantities_", suff, ".csv"),
       row.names = FALSE
     )
     write.csv(
       refList,
       paste0(rootdir, "./results/reference_points_", suff, ".csv"),
       row.names = FALSE
     )
     write.csv(mls,
               paste0(rootdir, "/results/likelihoods_", suff, ".csv"),
               row.names = FALSE)
   }   else{
     return(list(mqs,mls))
   }


    # SSplotComparisons(mtemp, print = T, plotdir = paste0(rootdir,"/plots/"))
  }
  if (!is.na(subpattern)) {
    ## if subpattern provided loop once more
    sIDX <- 0
    for (m in 1:length(mods)) {

      ## loop into master file
      modname <- sub('.*///', '', mods)[m]
      ## use SS_output function to extract quantities
      subdirs <- mods[m] %>%
        list.dirs(., recursive = T) %>%
        .[grepl(subpattern, .)] %>%
        .[!grepl("plots", .)]

      ## jump to next mod if for some reason this is empty (happens w runtime)
      if(rlang::is_empty(subdirs)) next


      for (s in 1:length(subdirs)) {
        ## skip if it's just directory with folders inside
        if (length(list.dirs(subdirs[s], recursive = F)) > 0) next

        IDX <-  basename(subdirs)[s]
        sIDX <- sIDX + 1 ## will be one for first succesful hit

        ## pull out rep based on file name
        splitpath0 <- strsplit(subdirs[s], "/")[[1]]
        splitpath1 <- splitpath0[grep('Rep', splitpath0)]
        splitpath2 <-  sub("Rep*", "", splitpath1)
        splitpath <- ifelse(length(splitpath2) > 1, splitpath2[2], splitpath)

        mtemp <-   SS_output(subdirs[s], NoCompOK = TRUE, forecast = FALSE)

        ## REF POINTS
        refList <-  mtemp$derived_quants %>%
          select(Label, Value,StdDev) %>%
          mutate(Yr = gsub(".*_", "", Label)) %>%
          filter(!(Yr %in% mtemp$startyr:mtemp$endyr)) %>%
          select(-Yr) %>%
          pivot_wider(
            .,
            names_from = Label,
            # id_cols = idcol,
            values_from = c(StdDev,Value)
          ) %>%
          mutate(
            'Yr' = NA,
            'MOD' = splitpath1[1],
            "IDX" = IDX,
            "REP" = splitpath
          ) %>%
          select(-MOD, -REP, IDX, everything())


        ## time series
        mtq <- mtemp$derived_quants %>%
          select(Label, Value, StdDev) %>%
          mutate(Yr = gsub(".*_", "", Label)) %>%
          filter(Yr %in% mtemp$startyr:mtemp$endyr) %>%
          # filter(!is.na(.$Year)) %>%
          mutate(Label2 = gsub("_.*", "", Label) ) %>%
          pivot_wider(
            .,
            names_from = Label2,
            id_cols = Yr,
            values_from = c(StdDev,Value)
          ) %>%
          mutate('MOD' = splitpath2[1],
                 "IDX" = IDX,
                 "REP" = splitpath) %>%
          select(Yr, MOD, REP, IDX, everything()) #%>%

          # merge(., refList, by.x = c('IDX'), by.y = c('IDX'), all.y = FALSE) %>%
          # mutate(Yr = Yr.x, MOD = MOD.x, REP = REP.x, IDX = IDX) %>%
          # select(-Yr.y, -Yr.x, -REP.y, -REP.x, -MOD.y, -MOD.x) %>%
          # select(Yr, MOD, IDX, REP, everything())


       mtl <-  mtemp$likelihoods_used %>%
          mutate(Label = row.names(.)) %>%
          select(-lambdas) %>%
          pivot_wider(
            .,
            names_from = Label,
            values_from = values
          ) %>%
          mutate(
            'MOD' = suff,
            "IDX" = IDX,
            "REP" = splitpath
          ) %>%
          select(MOD, REP, IDX, everything())
        ## write and/or append SPRSeries
        if (writeTables == T) {
          # SPRseries <- data.frame(mtemp$sprseries,
          #                        "B.Bmsy" = mtemp$Kobe$B.Bmsy,
          #                        "F.Fmsy" = mtemp$Kobe$F.Fmsy,
          #                        "rep" =  splitpath,
          #                        "MOD" = modname,
          #                        'IDX' = IDX)
          #
          #
          # derivedquants <- mtemp$derived_quants %>%
          #   select(Label,Value,StdDev) %>%
          #   mutate(Year = sub("^[^_]*_", "", Label),
          #          Quant = gsub( "_.*", "", Label),
          #          MOD = modname)
          #
          # if(FleetName != 'All'){
          #   cpue.df <- mtemp$cpue %>% filter(Name %in% FleetName)
          # } else
          #   cpue.df <- mtemp$cpue
          #
          # cpue <- data.frame(cpue.df,
          #                   "rep" = splitpath,
          #                   "MOD" = modname,
          #                   'IDX' = IDX)



          if (m == 1 & sIDX == 1) {
            ## first mod, first rep
            write.table(
              mtq,
              append = F,
              file =  paste0(
                rootdir,
                "/results/management_quantities_",
                suff,
                ".csv"
              ),
              row.names = F,
              col.names = TRUE,
              sep = ","
            )
            write.table(
              refList,
              append = F,
              file =  paste0(
                rootdir,
                "/results/reference_points_",
                suff,
                ".csv"
              ),
              row.names = F,
              col.names = TRUE,
              sep = ","
            )

            write.table(
              mtl,
              append = F,
              file =  paste0(
                rootdir,
                "/results/likelihoods_",
                suff,
                ".csv"
              ),
              row.names = F,
              col.names = TRUE,
              sep = ","
            )
            # write.table(
            #   SPRseries,
            #   append = F,
            #   file =  paste0(rootdir, "/results/SPRseries.csv"),
            #   row.names = F,
            #   col.names = T,
            #   sep = ","
            # )
            # write.table(
            #   cpue,
            #   append = F,
            #   file =  paste0(rootdir, "/results/cpue.csv"),
            #   row.names = F,
            #   col.names = T,
            #   sep = ","
            # )
          } else{
            write.table(
              mtq,
              append = T,
              file =  paste0(
                rootdir,
                "/results/management_quantities_",
                suff,
                ".csv"
              ),
              row.names = F,
              col.names = F,
              sep = ","
            )
            write.table(
              refList,
              append = T,
              file =  paste0(
                rootdir,
                "/results/reference_points_",
                suff,
                ".csv"
              ),
              row.names = F,
              col.names = F,
              sep = ","
            )
            write.table(
              mtl,
              append = T,
              file =  paste0(
                rootdir,
                "/results/likelihoods_",
                suff,
                ".csv"
              ),
              row.names = F,
              col.names = F,
              sep = ","
            )
            # write.table(
            #   SPRseries,
            #   append = T,
            #   file =  paste0(rootdir, "/results/SPRseries.csv"),
            #   row.names = F,
            #   col.names = F,
            #   sep = ","
            # )
            # write.table(
            #   cpue,
            #   append = T,
            #   file =  paste0(rootdir, "/results/cpue.csv"),
            #   row.names = F,
            #   col.names = F,
            #   sep = ","
            # )

          } ## end other reps
        } ## end writeTables
        ## extract ref point estimates, in order of toMatch

        ## fancy indexing for sublist
        # idx <- (m-1)*length(subdirs) + s

        # refList[idx, "MOD" ] <- modname
        # refList[idx, "REP"] <- splitpath
        # refList[idx,'IDX'] <- IDX
        # refList[idx,"SPB_SSBMSY"] <- mtemp$Kobe[mtemp$Kobe$Year == terminal_year,"B.Bmsy"]
        # refList[idx,"F_FMSY"] <- mtemp$Kobe[mtemp$Kobe$Year == terminal_year,"F.Fmsy"]
        # refList[idx,"LIKELIHOOD_TOTAL"] <- mtemp$likelihoods_used['TOTAL','values']
        # refList[idx,"LIKELIHOOD_SURVEY"] <- mtemp$likelihoods_used['Survey','values']
        # refList[idx,"LIKELIHOOD_CATCH"] <- mtemp$likelihoods_used['Catch','values']
        # refList[idx,"EQUIL_CATCH"] <- mtemp$likelihoods_used['Equil_catch','values']

        # idx <- idx + 1
        # refList[idx,"RMSE_S4"] <- mtemp$index_variance_tuning_check %>% .$r.m.s.e %>% as.numeric()
      } ## end of subdir loop
    } ## end mods loop
  } ## end !isna subpattern
} ## end function

    # if(!is.na(pattern) & is.na(subpattern)){
    #
    #
    #     SPRseries <- data.frame(mtemp$sprseries,
    #                             "B.Bmsy" = mtemp$Kobe$B.Bmsy,
    #                             "F.Fmsy" = mtemp$Kobe$F.Fmsy,
    #                             "rep" =  sub('OMrep',"",modname),
    #                             "MOD" = modname,
    #                             'IDX' = IDX)
    #
    #
    #     derivedquants <- mtemp$derived_quants %>%
    #       select(Label,Value,StdDev) %>%
    #       mutate(Year = sub("^[^_]*_", "", Label),
    #              Quant = gsub( "_.*", "", Label),
    #              MOD = modname)
    #
    #     if(FleetName != 'All'){
    #       cpue.df <- mtemp$cpue %>% filter(Name %in% FleetName)
    #     } else
    #       cpue.df <- mtemp$cpue
    #
    #     cpue <- data.frame(cpue.df,
    #                        "rep" = splitpath,
    #                        "MOD" = modname,
    #                        'IDX' = IDX)
    #
    #
    #
    #     if (m == 1 & s == 1) { ## first mod, first rep
    #       write.table(
    #         derivedquants,
    #         append = F,
    #         file =  paste0(rootdir, "/results/derivedquants.csv"),
    #         row.names = F,
    #         col.names = T,
    #         sep = ","
    #       )
    #       write.table(
    #         SPRseries,
    #         append = F,
    #         file =  paste0(rootdir, "/results/SPRseries.csv"),
    #         row.names = F,
    #         col.names = T,
    #         sep = ","
    #       )
    #       write.table(
    #         cpue,
    #         append = F,
    #         file =  paste0(rootdir, "/results/cpue.csv"),
    #         row.names = F,
    #         col.names = T,
    #         sep = ","
    #       )
    #     } else{
    #       write.table(
    #         derivedquants,
    #         append = T,
    #         file =  paste0(rootdir, "/results/derivedquants.csv"),
    #         row.names = F,
    #         col.names = F,
    #         sep = ","
    #       )
    #       write.table(
    #         SPRseries,
    #         append = T,
    #         file =  paste0(rootdir, "/results/SPRseries.csv"),
    #         row.names = F,
    #         col.names = F,
    #         sep = ","
    #       )
    #       write.table(
    #         cpue,
    #         append = T,
    #         file =  paste0(rootdir, "/results/cpue.csv"),
    #         row.names = F,
    #         col.names = F,
    #         sep = ","
    #       )
    #
    #     } ## end other reps
    #   } ## end writeTables
    #   ## extract ref point estimates, in order of toMatch
    #
    #   ## fancy indexing for sublist
    #   # idx <- (m-1)*length(subdirs) + s
    #
    #   refList[idx, "MOD" ] <- modname
    #   refList[idx, "REP"] <- splitpath
    #   refList[idx,'IDX'] <- IDX
    #   refList[idx,"SPB_SSBMSY"] <- mtemp$Kobe[mtemp$Kobe$Year == terminal_year,"B.Bmsy"]
    #   refList[idx,"F_FMSY"] <- mtemp$Kobe[mtemp$Kobe$Year == terminal_year,"F.Fmsy"]
    #   refList[idx,"LIKELIHOOD_TOTAL"] <- mtemp$likelihoods_used['TOTAL','values']
    #   refList[idx,"LIKELIHOOD_SURVEY"] <- mtemp$likelihoods_used['Survey','values']
    #   refList[idx,"LIKELIHOOD_CATCH"] <- mtemp$likelihoods_used['Catch','values']
    #   refList[idx,"EQUIL_CATCH"] <- mtemp$likelihoods_used['Equil_catch','values']
    #
    #   idx <- idx + 1
    # } ## end if only pattern
    #
    # if(is.na(pattern) & is.na(subpattern)){
    #   mtemp <- mods[m] %>%
    #     .[!grepl("plots", .)] %>%
    #             SS_output(.,
    #               covar = F,
    #               forecast = F,
    #               ncols = 1000)
    # } ## end if just rootdir

    #   if(is.na(pattern) & is.na(subpattern) |(!is.na(pattern) & is.na(subpattern))){ ## use simple idx for no-sub cases
    #     modname <- basename(mods[m])
    #
    #     refList[m,"MOD"] <- modname
    #     refList[m,"SPB_SSBMSY"] <- mtemp$Kobe[mtemp$Kobe$Year == terminal_year,"B.Bmsy"]
    #     refList[m,"F_FMSY"] <- mtemp$Kobe[mtemp$Kobe$Year == terminal_year,"F.Fmsy"]
    #     refList[m,"LIKELIHOOD_TOTAL"] <- mtemp$likelihoods_used['TOTAL','values']
    #     refList[m,"LIKELIHOOD_SURVEY"] <- mtemp$likelihoods_used['Survey','values']
    #     refList[m,"LENGTH_COMP"] <- mtemp$likelihoods_used['Length_comp','values']
    #     refList[m,"LIKELIHOOD_CATCH"] <- mtemp$likelihoods_used['Catch','values']
    #     refList[m,"EQUIL_CATCH"] <- mtemp$likelihoods_used['Equil_catch','values']
    #
    #     if (writeTables == T) {
    #
    #       SPRseries <- data.frame(mtemp$sprseries,
    #                              "B.Bmsy" = mtemp$Kobe$B.Bmsy,
    #                              "F.Fmsy" = mtemp$Kobe$F.Fmsy,
    #                              "MOD" = modname)
    #
    #       derivedquants <- mtemp$derived_quants %>%
    #         select(Label,Value,StdDev) %>%
    #         mutate(Year = sub("^[^_]*_", "", Label),
    #                Quant = gsub( "_.*", "", Label),
    #                MOD = modname)
    #
    #       ## write and/or append CPUEseries
    #       if(FleetName != 'All'){ ## if you don't want everything, subset CPUE
    #         cpue.df <- mtemp$cpue %>% filter(Name %in% FleetName)
    #       } else
    #         cpue.df <- mtemp$cpue
    #
    #       cpue <- data.frame(cpue.df, "MOD" = modname)
    #
    #       if (m == 1) { ## first mod, first rep
    #         write.table(
    #           SPRseries,
    #           append = F,
    #           file =  paste0(rootdir, "/results/SPRseries.csv"),
    #           row.names = F,
    #           col.names = T,
    #           sep = ","
    #         )
    #         write.table(
    #           derivedquants,
    #           append = F,
    #           file =  paste0(rootdir, "/results/derivedquants.csv"),
    #           row.names = F,
    #           col.names = T,
    #           sep = ","
    #         )
    #         write.table(
    #           cpue,
    #           append = F,
    #           file =  paste0(rootdir, "/results/cpue.csv"),
    #           row.names = F,
    #           col.names = T,
    #           sep = ","
    #         )
    #       } else{
    #         write.table(
    #           SPRseries,
    #           append = T,
    #           file =  paste0(rootdir, "/results/SPRseries.csv"),
    #           row.names = F,
    #           col.names = F,
    #           sep = ","
    #         )
    #         write.table(
    #           derivedquants,
    #           append = T,
    #           file =  paste0(rootdir, "/results/derivedquants.csv"),
    #           row.names = F,
    #           col.names = F,
    #           sep = ","
    #         )
    #         write.table(
    #           cpue,
    #           append = T,
    #           file =  paste0(rootdir, "/results/cpue.csv"),
    #           row.names = F,
    #           col.names = F,
    #           sep = ","
    #         )
    #       } ## end other reps
    #     } ## end writeTables
    #   } ## end simplecase write
    # } ## end mods loop
    #
    #
    #
    #
    # if(writeTables == TRUE) write.csv(refList, paste0(rootdir,"/results/management_quantities.csv"), row.names = F)
    # return(refList)

  # } ## end function

  # kaputils:::extractResults(
# rootdir =   "C:/Users/mkapur/Dropbox/UW/sneak/runs/2020-04-01/";
# terminal_year = 2016;
# suffix = "OM";
# pattern = "OM";
# subpattern = "*SpaceLast";
# writeTables = T
  # )

  # kaputils:::extractResults(
  #   rootdir =   "C:/Users/mkapur/Dropbox/UW/sneak/runs/2020-04-01/",
  #   terminal_year = 2016,
  #   pattern = "OM",
  #   # suffix = "EM",
  #   subpattern = NA,
  #   writeTables = T
  # )
  # ## test with subdir
  # kaputils:::extractResults(
  #   rootdir = "C:/Users/MKapur/Dropbox/UW/coursework/FISH-555/stm_mods/model_805",
  #   terminal_year = 2017,
  #   pattern = NA,
  #   subpattern = NA,
  #   writeTables = TRUE,
  #   FleetName = 'All'
  # )
