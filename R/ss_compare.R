#' ss_compare
#' custom wrapper to loop through nested operating/estimation models and plot using r4ss
#' @param rootdir master location of all models and (optional) replicates; will search for Report.sso files therein
#' @param pattern a string which identifies the first-order folders, e.g "EM" "OM" -- applicable to grep().
#' @param subpattern can be NA; a secondary string by which models will be grouped, e.g. "Replicate"
#' @param llabels logical. If T, will write a CSV with results.
#' @export
ss_compare <- function(rootdir,
                       subplots = 1:20,
                       plotloc = NA,
                       pattern = "Rep",
                       llabels =c(paste0("OMR",1:2),paste0("EMR",1:2)),
                       likeCSV = T,
                       likeLabel = c('Surv_like','SizeFreq_like:_2'),
                       lambdaLabel = c('Surv_lambda','SizeFreq_lambda:_2'),
                       fishery = "Deep7",
                       fleetIdx = c(2,3),
                       dolegend = T){



  setwd(rootdir)
  dir.create("plots") ## make storage for plots

  ## make summary object
  if(!is.na(pattern)){
    mods = list.dirs(rootdir) %>%
      .[grepl(pattern, .)]
  }else{ mods = list.files(rootdir,  pattern = "EM|OM")}

  ## skip if it's just directory with folders inside
  moddrop <- NA
  for(m in 1:length(mods)){
  if(length(list.dirs(mods[m], recursive = F)) > 0)  moddrop[m] <- m
  }
  moddrop <- moddrop[!is.na(moddrop)]
  if(length(moddrop >0)) mods <- mods[-moddrop]

  summaryoutput <- mods %>%
    SSgetoutput(dirvec = .,
                getcovar = F,
                ncols = 1000) %>%
    SSsummarize()


  ## dump plots to file
  SSplotComparisons(
    summaryoutput,
    subplots = subplots,
    plot = T,
    pdf = F,
    png = F,
    print = T,
    plotdir = "plots/",
    indexfleets = length(unique(summaryoutput$indices$FleetName)),
    lty = c(rep(1, summaryoutput$n-1),3),
    legend = dolegend,
    legendloc = 'bottomright',
    legendlabels = basename(mods)
  )

}
