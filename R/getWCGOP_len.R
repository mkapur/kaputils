#' getWCGOP_len
#' beta
#' a function to transpose data from the WCGOP into a usable form for SS.
#' returns a table that can be saved/copied into a stock synthesis file.
#' are in the traditional format as from the Observer program
#' @param lcomps  a data frame of length-comps from the WCGOP program
#' @param Nsamp a vector of # samples. Typically this is N_Unique_trips from the WCGOP data.
#' @param fleet a string or vector of strings matching values from column Gear
#' @param fleetno optional; fill in any number to the "FltSvy" column of the resultant table; defaults to 1
#' @param lbin_lo minimum bin
#' @param lbin_hi maximum bin
#' @param lbin_inc increments of bins
#' @param month optional; fill in a month into the month column of the resultant table
#' @param sex As in SS: # sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
#' @param part # partition codes:  (0=combined; 1=discard; 2=retained) will be written to table
#' @param writeTable logical. if T, requires writeloc
#' @param writeloc a filepath to where you want the table saved.
#' @export

## Get lengths from discard. Going to use the "weighted" columns

getWCGOP_len <- function(lcomps,
                         Nsamp = round(runif(13, 113, 554)),
                         fleet,
                         fleetno = 1,
                         lbin_lo = NA,
                         lbin_hi = NA,
                         lbin_inc = NA,
                         month = 1,
                         sex = 0,
                         part,
                         writeTable = F,
                         writeloc = getwd()) {
  ## make sure the year column is named readably


  names(lcomps)[1] <- 'Year'
  for(i in 1:length(fleet)){
    lcomps.temp <- lcomps %>% filter(Gear == fleet[i]) %>% select(Year, Prop.wghtd,Lenbin)  %>%
      pivot_wider(id_cols = c(Year), names_from = c(Lenbin), values_from= Prop.wghtd) %>%
      select(Year, everything())
    tidx <- ncol(lcomps.temp)
    if(missing(lbin_lo)){
      message("lbin_lo not specified. defaulting to min present in dataframe \n")
      lbin_lo = with(subset(lcomps, Gear == fleet[i]), min(Lenbin))
    }
    if(missing(lbin_hi)){
      message("lbin_hi not specified. defaulting to max present in dataframe \n")
      lbin_hi = with(subset(lcomps, Gear == fleet[i]), max(Lenbin))
    }
    if(missing(lbin_inc)){
      message("lbin_hi not specified. defaulting to 2")
      lbin_inc <- 2
    }
    if(sex == 0){ ## duplicate records if sex = 0
      names(lcomps.temp)[2:ncol(lcomps.temp)] <- paste0('X',seq(lbin_lo,lbin_hi,lbin_inc))
      lcomps.temp2 <- cbind(lcomps.temp, lcomps.temp[,-1])
      lcomps.temp2[,-1] <- round(lcomps.temp2[,-1],6)
      names(lcomps.temp2[(tidx+1):ncol(lcomps.temp2)]) <- paste0(names(lcomps.temp2)[2:(tidx)],".1")
    } else if(sex != 0){
      stop('this function not yet ready for sex != 0, sorry')
    }

    new.ldis <- cbind(lcomps.temp2,Nsamp) %>% data.frame() %>%
      ## sex combined -- just duplicated as in examples
      mutate(month = month, fleet = fleetno, sex = sex, part = part, Nsamp = Nsamp ) %>%
      select(Year, month, fleet, sex, part, Nsamp, everything())

    if(writeTable){
      write.table(new.ldis, file = paste0(writeloc,"/lcomp_discard_SS_",Sys.Date(),"_",fleet[i],".csv"),row.names = FALSE, sep = " ", quote = F)
      cat(paste0('wrote discard rate table for ', fleet[i],' to ',writeloc,"\n"))
    }
    rm(lcomps.temp);rm(lcomps.temp2); rm(new.ldis)
    return(new.ldis)
  }## end loop fleets
}

# ldis <- read.csv("C:/Users/mkapur/Dropbox/UW/assessments/sab_2019/200.00_base_files_29May/100.00_base_files/hklpot_agg/sablefish_discard_lengths.csv")
# getWCGOP_len(lcomps = ldis, fleet = "HookAndPot", writeTable = F, part = 0)
# ndis  <- read.csv("./sablefish_discard_nuniquetrips.csv") ## use N_unique_trips for nsamp
# names(ldis)[1] <- 'Year'
# names(ndis)[1] <- 'Gear'
