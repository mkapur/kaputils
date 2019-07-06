#' reweight_discards
#' beta
#' a function to re-weight observed discard rates from WCGOP data based on pre/post catch-shares years
#' returns a table that can be saved/copied into a stock synthesis file. Assumes the ncs/cs dataframes
#' are in the traditional format as from the Observer program
#' @param ncs  a data frame of non-catch-shares observer data
#' @param cs a data frame of catch-shares observer data
#' @param fleet a string or vector of strings matching values from column gear3
#' @param fleetno optional; fill in any number to the "FltSvy" column of the resultant table
#' @param month optional; fill in a month into the month column of the resultant table
#' @param years optional; if you'd like to only take a subset of years from the final dataframe
#' @param units  one of lbs or kg, if kg LBS will be converted
#' @param writeTable logical. if T, requires writeloc
#' @param writeloc a filepath to where you want the table saved.
#' @export

reweight_discards <- function(ncs,cs,fleet, fleetno = 1, month = 7,years,units,writeTable = F,
                              writeloc = paste0(getwd(),"discard_rates_ss_",Sys.Date(),".csv")){


  for(i in 1:length(fleet)){
    discard <-  ncs  %>% filter(gear3 == fleet[i]) %>%
      select(yr = ryear, "OBS" = Observed_Ratio, 'sd' = StdDev.Boot_Ratio ) ## raw ncs, with SDs

    ## compute re-weighted values for cs years
    discard_late <- merge(cs %>% select(ryear,'CS_LBS' = Observed_RETAINED.LBS, "CS_RATIO" = Observed_Ratio ) ,
                          ncs  %>% select(ryear,'NCS_LBS' = Median.Boot_RETAINED.LBS , "NCS_RATIO" = Observed_Ratio ) , by = 'ryear') %>%
      mutate(tot = CS_LBS+ NCS_LBS) %>%
      group_by(ryear) %>%
      summarise(cs_prop = CS_LBS/tot,
                ncs_prop = NCS_LBS/tot,
                cs_propxrate  = cs_prop*CS_RATIO,
                ncs_propxrate  = ncs_prop*NCS_RATIO,
                total_disrate = cs_propxrate+ncs_propxrate)

    ## overwrite ncs years, retaining SDs
    discard[discard$yr > 2010 ,"OBS"] <- discard_late$total_disrate

    ## format to match SS
    discard <- discard %>%
      mutate(fleet = fleetno, month = month, OBS = round(OBS,6), sd = round(sd,6),
                                  note = paste0("#",fleet[i]) ) %>%
      select(yr, month, fleet, OBS,sd, note) %>%
      filter(yr %in% years)
    if(writeTable){
      write.table(discard,file = paste0(writeloc,"/discard_rates_SS_",Sys.Date(),"_",fleet,".csv"),row.names = FALSE, sep = " ", quote = F)
      cat(paste0('wrote discard rate table for ', fleet[i],' to ',writeloc,"\n"))
    }
    return(discard)
    rm(discard); rm(discard_late)

  } ## end loop fleets
} ## end function

## not run:
# setwd("C:/Users/mkapur/Dropbox/UW/assessments/sab_2019/200.00_base_files_29May/100.00_base_files/hklpot_agg")
# ## read in CSVs and drop  'trawl'
# cs <- read.csv("sablefish_OB_DisRatios_cs_2019_Coastwide_trawl_fixed_2019-06-27.csv")
# ncs <- read.csv("sablefish_OB_DisRatios_ncs_2019_Coastwide_trawl_fixed_2019-06-27.csv")
#
reweight_discards(ncs,cs,fleet = 'HLandPot', fleetno = 1, month = 7,years,writeTable = F,
                  # writeloc = getwd(), years = 2009:2015)






