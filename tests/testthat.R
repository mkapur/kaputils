# library(testthat)

install.packages(
  "G:\\kaputils",
  repos = NULL,
  type = "source",
  dependencies = T
)
library(kaputils)

# test_check("kaputils")

## master directory where all EMS/OMS are stored
masterfile <- "G:\\MAKO\\mako_sim"
basecase <- "MAK_OM"


## will plot panels in each EM folder
plotInputSel(
  rootdir = masterfile,
  pattern = NA,
  year = 2016,
  lmin = 35,
  lmax = 350,
  linc = 5,
  pdfrows = 3,
  pdfcols = 2
)

## plots SRR panel for individual models and a panel PDF in plots/
plotSRR_panel(
  rootdir = masterfile,
  saveplot = T,
  pattern = NA,
  plotloc = NA,
  pdfrows = 2,
  pdfcols = 2
)

## test without subdir
# extract_results(
#   rootdir = basecase,
#   pattern = NA,
#   subpattern = NA,
#   writeTables = T,
#   FleetName = c("S4_JPN_SS", "S7_JPN_GEO", "All")[3]
# )

extractResults_grid(rootdir = masterfile,
                     pattern = "MAK_",
                     writeTables = T,
                     ## subset fleets for CPUE csv
                     FleetName = c("S4_JPN_SS","S7_JPN_GEO","All")[3])


cpue_results <- read.csv(paste0(masterfile,"\\results\\cpue.csv"))

## all models on panel
plotCPUE_panel(
  cpue_results,
  saveplot = T,
  mods = NA,
  plotloc = paste0(masterfile,"/plots/"),
  pdfrows = 2,
  pdfcols = 2
)

## base case only
# plotCPUE_panel(
#   cpue_results,
#   saveplot = T,
#   mods = basecase,
#   plotloc = paste0(masterfile,"/plots/"),
#   pdfrows = 2,
#   pdfcols = 2
# )


SPRseries <-
  read.csv(paste0(masterfile,"/results/SPRseries.csv"))

## if mods = NA will plot all trajectories; legend only prints if less than 5 mods
plotF(
  SPRseries,
  saveplot = T,
  mods = NA,
  plotloc = paste0(masterfile,"/plots/"),
  pdfrows = 4,
  pdfcols = 2
)


ss_compare(
  rootdir = masterfile,
  plotloc =  paste0(masterfile,"/plots/"),
  subplots = c(1, 9),
  pattern = NA,
  llabels = 'basecase',
  likeCSV = F,
  likeLabel = c('Surv_like', 'SizeFreq_like:_2'),
  lambdaLabel = c('Surv_lambda', 'SizeFreq_lambda:_2'),
  fishery = "Generalized Shark",
  dolegend = T
)

## plots panel kobe pdf to plots/ 
## can change pattern to a single model (e.g. "basecase")
plotKobe_panel(
  rootdir = masterfile,
  pattern = "MAK_",
  plotloc = paste0(masterfile,"/plots/"),
  plotrows = 1,
  plotcols = 3,
  saveplot = T)
