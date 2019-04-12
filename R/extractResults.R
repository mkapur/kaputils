#' extract_results
#'
#' \code{extract_results} custom wrapper of \code{\link{SS_summarize}} to extract reference points nested SS3 simulations
#' @param rootdir root filepath where all subdirectories containing Report.sso are stored
#' @param pattern a string that specifically matches all directories with report files of interest.
#' @param subpattern type of file to be saved
#' @param writeTables logical. Should a csv of results be saved?
#' @param FleetName optional vector of fleet names for which data should be extracted; all means all
#' @seealso \code{\link[r4ss]}

extractResults <- function(rootdir,
                            pattern = NA,
                            subpattern = NA,
                            writeTables = T,
                            FleetName = c("S4_JPN_SS","S7_JPN_GEO","All")[3]){



  ## iterate avail. runs
  if (!is.na(pattern)) {
    mods <- list.dirs(rootdir, recursive = F, full.names = T) %>%
      .[grepl(pattern, .)]
  }  else{
    mods <- rootdir
  }

  refList = data.frame(
    "MOD" = NA,
    "REP" = NA,
    "F_FMSY" = NA,
    "SPB_SSBMSY" = NA
  )
  if(!exists(paste0(rootdir,"/results/"))) dir.create(paste0(rootdir,"/results/"))
  idx <- 1
  for (m in 1:length(mods)) { ## loop into master file

    ## use SS_output function to extract quantities
    if (!is.na(subpattern)) { ## if subpattern provided loop once more
      subdirs <- mods[m] %>%
        list.dirs(., recursive = T) %>%
        .[grepl(subpattern, .)] %>%
        .[!grepl("plots", .)] %>%


      for (s in 1:length(subdirs)) {

        ## skip if it's just directory with folders inside
        if(length(list.dirs(subdirs[s], recursive = F)) > 0)  next

        modname <- sub('.*\\/', '', mods)[m]
        IDX <-  basename(subdirs)[s]

        ## pull out rep based on file name
        splitpath0 <- strsplit(subdirs[s],"/")[[1]]
        splitpath1 <- splitpath0[grep('Rep',splitpath0)]
        splitpath <-  sub("Rep*","",splitpath1)

        mtemp <- subdirs[s] %>%
          SS_output(.,
                    covar = F,
                    forecast = F,
                    ncols = 1000)

        ## write and/or append SPRSeries
        if (writeTables == T) {
          SPRseries <- data.frame(mtemp$sprseries,
                                 "B.Bmsy" = mtemp$Kobe$B.Bmsy,
                                 "F.Fmsy" = mtemp$Kobe$F.Fmsy,
                                 "rep" =  splitpath,
                                 "MOD" = modname,
                                 'IDX' = IDX)

          if (m == 1 & s == 1) { ## first mod, first rep
            write.table(
              SPRseries,
              append = F,
              file =  paste0(rootdir, "/results/SPRseries.csv"),
              row.names = F,
              col.names = T,
              sep = ","
            )
          }
          else{
            write.table(
              SPRseries,
              append = T,
              file =  paste0(rootdir, "/results/SPRseries.csv"),
              row.names = F,
              col.names = F,
              sep = ","
            )
          }

          if(FleetName != 'All'){
            cpue.df <- mtemp$cpue %>% filter(Name == FleetName)
          } else
            cpue.df <- mtemp$cpue

          cpue = data.frame(cpue.df,
                            "rep" = splitpath,
                            "MOD" = modname,
                            'IDX' = IDX)
          if (m == 1 & s == 1) {
            write.table(
              cpue,
              append = F,
              file =  paste0(rootdir, "/results/cpue.csv"),
              row.names = F,
              col.names = T,
              sep = ","
            )
          }
          else{
            write.table(
              cpue,
              append = T,
              file =  paste0(rootdir, "/results/cpue.csv"),
              row.names = F,
              col.names = F,
              sep = ","
            )
          } ## end other reps
        } ## end writeTables
        ## extract ref point estimates, in order of toMatch

        ## fancy indexing for sublist
        # idx <- (m-1)*length(subdirs) + s

        refList[idx, "MOD" ] <- modname
        refList[idx, "REP"] <- splitpath
        refList[idx,'IDX'] <- IDX
        refList[idx,"SPB_SSBMSY"] <- mtemp$Kobe[nrow(mtemp$Kobe),"B.Bmsy"]
        refList[idx,"F_FMSY"] <- mtemp$Kobe[nrow(mtemp$Kobe),"F.Fmsy"]
        refList[idx,"LIKELIHOOD_TOTAL"] <- mtemp$likelihoods_used['TOTAL','values']
        refList[idx,"LIKELIHOOD_SURVEY"] <- mtemp$likelihoods_used['Survey','values']
        refList[idx,"LIKELIHOOD_CATCH"] <- mtemp$likelihoods_used['Catch','values']
        refList[idx,"EQUIL_CATCH"] <- mtemp$likelihoods_used['Equil_catch','values']

        idx <- idx + 1
        # refList[idx,"RMSE_S4"] <- mtemp$index_variance_tuning_check %>% .$r.m.s.e %>% as.numeric()
      } ## end of subdir loop
    } ## end !is na subpattern

    if(!is.na(pattern) & is.na(subpattern)){
      mtemp <- mods[m] %>%
        list.dirs(., recursive = T) %>%
        .[grepl(pattern, .)] %>%
        .[!grepl("plots", .)] %>%

        SS_output(.,
                  covar = F,
                  forecast = F,
                  ncols = 1000)
    } ## end if only pattern

    if(is.na(pattern) & is.na(subpattern)){
      mtemp <- mods[m] %>%
        SS_output(.,
                  covar = F,
                  forecast = F,
                  ncols = 1000)
    } ## end if just rootdir

    if(is.na(pattern) & is.na(subpattern) |(!is.na(pattern) & is.na(subpattern))){ ## use simple idx for no-sub cases
      modname <- basename(mods[m])

      refList[m,"MOD"] <- modname
      refList[m,"SPB_SSBMSY"] <- mtemp$Kobe[nrow(mtemp$Kobe),"B.Bmsy"]
      refList[m,"F_FMSY"] <- mtemp$Kobe[nrow(mtemp$Kobe),"F.Fmsy"]
      refList[m,"LIKELIHOOD_TOTAL"] <- mtemp$likelihoods_used['TOTAL','values']
      refList[m,"LIKELIHOOD_SURVEY"] <- mtemp$likelihoods_used['Survey','values']
      refList[m,"LIKELIHOOD_CATCH"] <- mtemp$likelihoods_used['Catch','values']
      refList[m,"EQUIL_CATCH"] <- mtemp$likelihoods_used['Equil_catch','values']

      if (writeTables == T) {
        SPRseries = data.frame(mtemp$sprseries,
                               "B.Bmsy" = mtemp$Kobe$B.Bmsy,
                               "F.Fmsy" = mtemp$Kobe$F.Fmsy,
                               "MOD" = modname)
        if (m == 1) { ## first mod, first rep
          write.table(
            SPRseries,
            append = F,
            file =  paste0(rootdir, "/results/SPRseries.csv"),
            row.names = F,
            col.names = T,
            sep = ","
          )
        }
        else{
          write.table(
            SPRseries,
            append = T,
            file =  paste0(rootdir, "/results/SPRseries.csv"),
            row.names = F,
            col.names = F,
            sep = ","
          )
        }

        ## write and/or append CPUEseries
        if(FleetName != 'All'){ ## if you don't want everything, subset CPUE
          cpue.df <- mtemp$cpue %>% filter(Name %in% FleetName)
        } else
          cpue.df <- mtemp$cpue

        cpue <- data.frame(cpue.df, "MOD" = modname)

        if (m == 1) {
          write.table(
            cpue,
            append = F,
            file =  paste0(rootdir, "/results/cpue.csv"),
            row.names = F,
            col.names = T,
            sep = ","
          )
        }
        else{
          write.table(
            cpue,
            append = T,
            file =  paste0(rootdir, "/results/cpue.csv"),
            row.names = F,
            col.names = F,
            sep = ","
          )
        } ## end other reps
      } ## end writeTables
    } ## end simplecase write
  } ## end mods loop

  if(writeTables) write.csv(refList, paste0(rootdir,"/results/management_quantities.csv"), row.names = F)
  return(refList)

  } ## end function


# rootdir <-"G:/TUNABOOT"
# pattern = 'TUNA_'
# subpattern = 'Rep'

## not run:
## test without subdir
# extract_results(rootdir = "G:\\MAKO\\Base_Case_S4",
#                             pattern = NA,
#                             subpattern = NA,
#                             writeTables = T,
#                             FleetName = c("S4_JPN_SS","S7_JPN_GEO","All")[3])
# ## test with subdir
# extractResults(rootdir =  "G:/SSBOOT/",
#                 pattern = 'MAK',
#                 subpattern = 'Rep',
#                 writeTables = T,
#                 FleetName = c("S4_JPN_SS","S7_JPN_GEO","All")[3])
