#' NEP_Extrap
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



EBS_extrap = make_extrapolation_info( Region = "Eastern_Bering_Sea", strata.limits = strata.limits, zone = 32, flip_around_dateline = F )
NBS_extrap = make_extrapolation_info( Region = "Northern_Bering_Sea", strata.limits = strata.limits, zone = 32, flip_around_dateline = F )
GOA_extrap = make_extrapolation_info( Region = "gulf_of_alaska", strata.limits = strata.limits, zone = 32, flip_around_dateline = T )
BC_extrap = make_extrapolation_info( Region = "british_columbia", strata.limits = strata.limits, zone = 32, flip_around_dateline = F )
BC_extrap$Data_Extrap$Area_km2 <- BC_extrap$Area_km2_x
names(BC_extrap$a_el) <- "All_areas"
CC_extrap = make_extrapolation_info( Region = "california_current", strata.limits = strata.limits, zone = 32, flip_around_dateline = TRUE )
CC_extrap$Data_Extrap$Area_km2 <- CC_extrap$Area_km2_x

Extrapolation_List = combine_extrapolation_info("EBS" = EBS_extrap, "NBS" = NBS_extrap, "GOA" = GOA_extrap, "BC" = BC_extrap, "CC" = CC_extrap)
