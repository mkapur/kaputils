#' extract_results
#'
#' \code{extract_results} custom wrapper of \code{\link{SS_summarize}} to extract reference points nested SS3 simulations
#' @param rootdir root filepath where all subdirectories containing Report.sso are stored
#' @param pattern a string that specifically matches all directories with report files of interest.
#' @param subpattern type of file to be saved
#' @param writeTables logical. Should a csv of results be saved?
#' @param FleetName optional vector of fleet names for which data should be extracted; all means all
#' @seealso \code{\link[r4ss]}

extract_results <- function(rootdir,
                            pattern = NA,
                            subpattern = NA,
                            writeTables = T,
                            FleetName = c("S4_JPN_SS","S7_JPN_GEO","All")[3]){

  ## iterate avail. runs
  if(!is.na(pattern)) {mods <- list.dirs(rootdir, recursive = F, full.names = T) %>%
    .[grepl(pattern, .)]
  }  else{mods <- rootdir}

  refList = data.frame(
    "MOD" = NA,
    "REP" = NA,
    "F_FMSY" = NA,
    "SPB_SSBMSY" = NA
  )
  if(!exists(paste0(rootdir,"/results/"))) dir.create(paste0(rootdir,"/results/"))
  for (m in 1:length(mods)) { ## loop into master file

    ## use SS_output function to extract quantities

    if (!is.na(subpattern)) { ## if subpattern provided loop once more
      subdirs <- mods[m] %>%
        list.dirs(., recursive = T) %>%
        .[grepl(subpattern, .)]


      for (s in 1:length(subdirs)) {
        modname <- sub(".*/(.*)/.*","\\1",subdirs)[s]

        mtemp <- subdirs[s] %>%
          SS_output(.,
                    covar = F,
                    ncols = 1000)

        ## write and/or append SPRSeries
        if (writeTables == T) {
          SPRseries = data.frame(mtemp$sprseries, "rep" = s,
                                 "MOD" = modname)

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

          cpue = data.frame(cpue.df, "rep" = s,
                            "MOD" = modname)
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
        idx <- (m-1)*length(subdirs) + s
        refList[idx, "MOD" ] <- modname
        refList[idx, "REP"] <- s
        refList[idx,"SPB_SSBMSY"] <- mtemp$Kobe[nrow(mtemp$Kobe),"B.Bmsy"]
        refList[idx,"F_FMSY"] <- mtemp$Kobe[nrow(mtemp$Kobe),"F.Fmsy"]
        refList[idx,"LIKELIHOOD_TOTAL"] <- mtemp$likelihoods_used['TOTAL','values']
        refList[idx,"LIKELIHOOD_SURVEY"] <- mtemp$likelihoods_used['Survey','values']
        refList[idx,"LIKELIHOOD_CATCH"] <- mtemp$likelihoods_used['Catch','values']
        refList[idx,"EQUIL_CATCH"] <- mtemp$likelihoods_used['Equil_catch','values']
        # refList[idx,"RMSE_S4"] <- mtemp$index_variance_tuning_check %>% .$r.m.s.e %>% as.numeric()
      } ## end of subdir loop
    } ## end !is na subpattern

    else if(!is.na(pattern) & is.na(subpattern)){
      mtemp <- mods[m] %>%
        list.dirs(., recursive = T) %>%
        .[grepl(pattern, .)] %>%
        SS_output(.,
                  covar = F,
                  ncols = 1000)
    } else if(is.na(pattern) & is.na(subpattern)){
      mtemp <- mods[m] %>%
        SS_output(.,
                  covar = F,
                  ncols = 1000)
    }

      modname <- basename(mods[m])

      if (writeTables == T) {
        SPRseries = data.frame(mtemp$sprseries, "MOD" = modname)
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

      refList[m,"MOD"] <- modname
      refList[m,"SPB_SSBMSY"] <- mtemp$Kobe[nrow(mtemp$Kobe),"B.Bmsy"]
      refList[m,"F_FMSY"] <- mtemp$Kobe[nrow(mtemp$Kobe),"F.Fmsy"]
      refList[m,"LIKELIHOOD_TOTAL"] <- mtemp$likelihoods_used['TOTAL','values']
      refList[m,"LIKELIHOOD_SURVEY"] <- mtemp$likelihoods_used['Survey','values']
      refList[m,"LIKELIHOOD_CATCH"] <- mtemp$likelihoods_used['Catch','values']
      refList[m,"EQUIL_CATCH"] <- mtemp$likelihoods_used['Equil_catch','values']
      # refList[m,"RMSE_S4"] <- mtemp$index_variance_tuning_check %>% .$r.m.s.e %>% as.numeric()
    }
  if(writeTables) write.csv(refList, paste0(rootdir,"/results/management_quantities.csv"), row.names = F)
  return(refList)
  } ## end function




## not run:
## test without subdir
# extract_results(rootdir = "G:\\MAKO\\Base_Case_S4",
#                             pattern = NA,
#                             subpattern = NA,
#                             writeTables = T,
#                             FleetName = c("S4_JPN_SS","S7_JPN_GEO","All")[3])
# ## test with subdir
# extract_results(rootdir = "G:\\SSBOOT-archive\\eachoff",
#                 pattern = 'MAK',
#                 subpattern = 'Rep',
#                 writeTables = T,
#                 FleetName = c("S4_JPN_SS","S7_JPN_GEO","All")[3])
