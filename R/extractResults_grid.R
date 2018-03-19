#' extractResults_grid
#'
#' \code{extractResults_grid} custom wrapper for Mako simulation studyof \code{\link{SS_summarize}} to extract reference points nested SS3 simulations
#' @param rootdir root filepath where all subdirectories containing Report.sso are stored
#' @param pattern a string that specifically matches all directories with report files of interest.
#' @param writeTables logical. Should a csv of results be saved?
#' @param FleetName optional vector of fleet names for which data should be extracted; all means all
#' @seealso \code{\link[r4ss]}

extractResults_grid <- function(rootdir,
                                pattern = NA,
                                writeTables = T,
                                FleetName = c("S4_JPN_SS", "S7_JPN_GEO", NA)[1]) {



  ## iterate avail. runs
  mods <- list.dirs(rootdir, recursive = F) %>%
    .[grepl(pattern, .)]

  if (!exists(paste0(rootdir, "/results/")))
    dir.create(paste0(rootdir, "/results/"))

  refList = data.frame(
    "MOD" = NA,
    "BETA" = NA,
    "SFRAC" =  NA,
    "VECTOR" =  NA,
    "BINIT" =  NA,
    "SPB_SSBMSY" = NA,
    "F_FMSY" = NA,
    "LIKELIHOOD_TOTAL" = NA,
    "LIKELIHOOD_SURVEY" = NA,
    "LIKELIHOOD_CATCH" = NA,
    "EQUIL_CATCH" = NA,
    "RMSE_S4" = NA
  )



  for (m in 1:length(mods)) {
    ## loop into master file

    ## use SS_output function to extract quantities
    mtemp <- mods[m] %>%
      SS_output(.,
                covar = F,
                ncols = 313,
                verbose = F)

    modname <- basename(mods)[m]
    ## write and/or append SPRSeries and CPUE
    if (writeTables == T) {
      SPRseries = data.frame(
        mtemp$sprseries,
        "MOD" = modname,
        "BETA" =   as.numeric(sapply(
          strsplit(sapply(
            strsplit(modname, "_", fixed = TRUE), "[", 4
          ), "-", fixed = TRUE), "[", 1
        )),
        "SFRAC" = as.numeric(sapply(
          strsplit(modname, "-", fixed = TRUE), "[", 2
        )),
        "VECTOR" = as.numeric(sapply(
          strsplit(modname, "-", fixed = TRUE), "[", 3
        )),
        "BINIT" = as.numeric(sapply(
          strsplit(modname, "-", fixed = TRUE), "[", 4
        ))
      )


      if (m == 1) {
        ## first mod, first rep
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
      if (FleetName != 'All') {
        ## if you don't want everything, subset CPUE
        cpue.df <- mtemp$cpue[mtemp$cpue$Name == FleetName, ]
      } else
        cpue.df <- mtemp$cpue

      cpue <- data.frame(
        cpue.df,
        "MOD" = modname,
        "BETA" = as.numeric(sapply(
          strsplit(sapply(
            strsplit(modname, "_", fixed = TRUE), "[", 4
          ), "-", fixed = TRUE), "[", 1
        )),
        "SFRAC" = as.numeric(sapply(
          strsplit(modname, "-", fixed = TRUE), "[", 2
        )),
        "VECTOR" = as.numeric(sapply(
          strsplit(modname, "-", fixed = TRUE), "[", 3
        )),
        "BINIT" = as.numeric(sapply(
          strsplit(modname, "-", fixed = TRUE), "[", 4
        ))
      )

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

    refList[m, "MOD"] <- modname
    refList[m, "BETA"] <-
      as.numeric(sapply(strsplit(sapply(
        strsplit(modname, "_", fixed = TRUE), "[", 4
      ), "-", fixed = TRUE), "[", 1))
    refList[m, "SFRAC"] <-
      as.numeric(sapply(strsplit(modname, "-", fixed = TRUE), "[", 2))
    refList[m, "VECTOR"] <-
      paste(sapply(strsplit(modname, "-", fixed = TRUE), "[", 3))
    refList[m, "VECTOR"] <-
      ifelse(refList[m, "VECTOR"] == 1,
             'Original',
             ifelse(refList[m, "VECTOR"] == "2", 'Fixed', 'Cortes'))
    refList[m, "BINIT"] <-
      as.numeric(sapply(strsplit(modname, "-", fixed = TRUE), "[", 4))

    refList[m, "SPB_SSBMSY"] <-
      mtemp$Kobe[nrow(mtemp$Kobe), "B.Bmsy"]
    refList[m, "F_FMSY"] <- mtemp$Kobe[nrow(mtemp$Kobe), "F.Fmsy"]
    refList[m, "LIKELIHOOD_TOTAL"] <-
      mtemp$likelihoods_used['TOTAL', 'values']
    refList[m, "LIKELIHOOD_SURVEY"] <-
      mtemp$likelihoods_used['Survey', 'values']
    refList[m, "LIKELIHOOD_CATCH"] <-
      mtemp$likelihoods_used['Catch', 'values']
    refList[m, "EQUIL_CATCH"] <-
      mtemp$likelihoods_used['Equil_catch', 'values']
    refList[m, "RMSE_S4"] <-
      mtemp$index_variance_tuning_check %>% filter(Fleet == 'S4_JPN_SS') %>% .$r.m.s.e %>% as.numeric()

    refList[m, "STATUS"] <- ifelse(refList[m, "SPB_SSBMSY"]   > 1 &
                                     refList[m, "F_FMSY"] < 1,
                                   'green',
                                   ifelse(
                                     refList[m, "SPB_SSBMSY"]  < 1 &
                                       refList[m, "F_FMSY"]  > 1,
                                     'red',
                                     ifelse(refList[m, "SPB_SSBMSY"]  > 1  &
                                              refList[m, "F_FMSY"]  > 1, 'orange',
                                            "yellow")
                                   ))

  } ## end loop
  if (writeTables)
    write.csv(refList,
              paste0(rootdir, "/results/management_quantities.csv"),
              row.names = F)
  return(refList)
}

## not run
## test on mako

# extractResults_grid(rootdir = "G:\\MAKO\\mako_sim-archive\\dummyrun_2ems_0308",
#                      pattern = "MAK_",
#                      writeTables = T,
#                      ## subset fleets for CPUE csv
#                      FleetName = c("S4_JPN_SS","S7_JPN_GEO","All")[1])
