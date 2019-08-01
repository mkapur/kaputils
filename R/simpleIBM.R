#' simpleIBM
#' generalized version of IBM used for Kapur et al. growth study (Submitted to Fisheries Research). Right now this has constant M,
#' and is not set-up to intake fishing mortality (therefore there is no selectivity). The population is initialized at equilibrium.
#' @param nboot  the number of unique simulations of length simu_year you'd like to generate
#' @param simu_year number of years to conduct simulation
#' @param burn_year can be zero; number of year's youd like to remove from the beginning of the simulation
#' @param Inst_M  natural mortality (single value applied to all ages)
#' @param L1 start length
#' @param a1 start age
#' @param L2 terminal length
#' @param a2 terminal age
#' @param lw_a coefficient of length-weight exponential equation
#' @param lw_b exponent of length-weight exponential equation#'
#' @param r_mat slope of maturity ogive
#' @param lw_b exponent of length-weight exponential equation
#' @param L50 length at 50% maturity
#' @param steepBH Beverton-holt steepness
#' @param VBGF_K K as in the von bertalanffy growth function
#' @export
simpleIBM <- function(nboot = 50,
           simu_year = 100,
           burn_yr = 20,
           Inst_M = 0.2,
           L1 = 10,
           a1 = 1,
           L2 = 100,
           a2 = 15,
           VBGF_K = 0.25,
           lw_a = 2e-4,
           lw_b = 3,
           r_mat = 0.1,
           L50 = 44,
           growth_lognormal_SD = 0.015,
           steepBH = 0.9,
           R0 = 10) {

    ## some presets ----
    ## Beverton-Holt SRR
    sigma_R = 0.1
     ## bev holt steepness

    # B-H S-R FUNCTION
    BH_SR = function(SSBy){
      y = (4*steepBH*R0*SSBy/(SSB0*(1-steepBH)+SSBy*(5*steepBH-1)))
      y
    }

    Linf = L1+(L2-L1)/(1-exp(-VBGF_K*(a2-a1)))

    ## Growth increment FUNCTION, NON TV: FROM V7

    growth_incre  <- function(fish_size){
      y=(Linf-fish_size)*(1-exp(-VBGF_K))
      y
    }

    lw  = function(fish_size){
      y=lw_a*fish_size^lw_b
      y
    }

    # Maturity ogive FUNCTION
    prob_mature = function(fish_size){
      y = 1/(1+exp(r_mat*(fish_size-L50)))
      y
    }

  for(boot_number in 1:nboot) {

    ## Generate Equilibrium Population ----
    Neqn <- rep(0,a2)
    S <- exp(-Inst_M)
    Neqn[1] <- R0 ## AEP set to 100
    for (Iage in 2:a2) Neqn[Iage] <- Neqn[Iage-1]*S ## survivorship
    Neqn[a2] <- Neqn[a2]/(1-S) ## plus group

    # Length-at-age
    init_length = NULL
    error = exp(rnorm(1,0,growth_lognormal_SD)-growth_lognormal_SD^2/2)
    init_length[1] = L1*error

    for(g in 1:(a2)){
      init_length[g+1] = init_length[g]+growth_incre(init_length[g])*error
    }
    init_length <- init_length[1:a2]
    # Weight-at-age
    init_weight = lw(init_length)
    # Maturity-at-age
    init_mat = round(prob_mature(init_length) ,1)
    SSB0 <- sum(Neqn*init_mat*init_weight) ## What we'd expect from a single population

    NIBM <- data.frame("Age" = NA, "Length_cm" = NA, "Mature" = NA, "Weight" = NA,
                       "Year" = NA, 'UniqueID' = NA)
    ## Populate initial individuals @ age. this number will not change, it is fixed, scaled by R0
    Ipnt <- 0
    for (Iage in 1:a2){ ## loop possible ages
      Nage <- round(Neqn[Iage]) # Integer numbers
      for (Inum in 1:Nage){ ## loop lifespan
        Ipnt <- Ipnt + 1 ## counting up individuals, new row for each
        NIBM[Ipnt,1] <- Iage-1 ## age
        # Length (initial)
        NIBM[Ipnt,2] <- init_length[Iage]+growth_incre(init_length[Iage])*exp(rnorm(1,0,growth_lognormal_SD)-growth_lognormal_SD^2/2) ## recalc error each time
        # Is this animal mature off the bat
        NIBM[Ipnt,3] <- runif(1,0,1) < prob_mature(NIBM[Ipnt,2])  ## logistic ogive, coinflip here gives x
        # Weight
        NIBM[Ipnt,4] <- lw( NIBM[Ipnt,2])
        NIBM[Ipnt,5] <- 0
        NIBM[Ipnt,6] <- Ipnt

      }
    }
    Ipnt0 <- Ipnt

    # Compute the Ps -- this is the conditional probability step, based on raw...expectation?
    P <- rep(1,a2); P[1] <- 0
    for (Iage in 2:a2)
      if (init_mat[Iage-1] < 1)
        P[Iage] <- (init_mat[Iage]-init_mat[Iage-1])/(1-init_mat[Iage-1])

    # Lognormal recruitment deviation module
    recruit_dev = rnorm(simu_year,0,sigma_R)
    recruit_dev_adj = (recruit_dev-0.5*sigma_R^2)
    RECy <- NULL
    SSBy = NULL
    SSBy[1] = SSB0
    for(ry in 1:simu_year){ ## iterate sim years

      # Compute SSB
      # Compute recruits here, this is how many we need to add later
      if(ry == 1){
        SSBy[ry] <- SSB0 ## SSB from last yr informs recruits
        RECy[ry] <- round(BH_SR(SSB0) * exp(recruit_dev_adj[ry]))
      }else{
        SSBy[ry] <- sum(NIBM$Mature[NIBM$Year == (ry-1)] * NIBM$Weight[NIBM$Year == (ry-1)]) ## Mature of living x weight of living [survived]
        RECy[ry] <- round(BH_SR(SSBy[ry]) * exp(recruit_dev_adj[ry]))
      }

      if(SSBy == 0)stop(ry," SSBY = 0 \n")
      if(RECy[ry] < 1){ ## fail if less than one recruit
        RECy[ry]=1 ## coerce to 1
        print(paste0("!!! Recruitment Fail !!! year ",ry))
      }
      Ilast <- ifelse(ry == 1,1,min(which(NIBM$Year == (ry-1))))  ## where do individuals for prev year begin?
      Ipnt <- ifelse(ry == 1,Ipnt0, nrow(subset(NIBM, Year == (ry-1)))) ## number of individuals
      idx <- nrow(NIBM) ## last filled in row
      for (II in Ilast:(Ilast+Ipnt-1)){ ## loop individuals for this year [given by Ipnt]
        MyAge <- NIBM[II,1] ## check age
        NIBM[idx+1,'UniqueID'] <- NIBM[II,'UniqueID'] ## retain ID
        NIBM[idx+1,'Year'] <- ry ## update year

        # Died this year?
        NIBM[idx+1,'Age'] <- ifelse(runif(1,0,1) > S, -1, ifelse(NIBM[II,'Age']+1 <= 15,NIBM[II,'Age']+1,15 )) ## Update dead, if not increase age by 1 year, coerce to 15 if older
        # Now grow the animal and update its weight
        NIBM[idx + 1, 'Length_cm'] <- NIBM[II, 'Length_cm'] +growth_incre(NIBM[II, 'Length_cm'])*exp(rnorm(1,0,growth_lognormal_SD)-growth_lognormal_SD^2/2)
        NIBM[idx + 1, 'Weight'] <-  lw( NIBM[II,'Length_cm'])
        # Mature this year?
        if (is.na(NIBM[II,'Mature'])){NIBM[II,'Mature'] <- 0} ## replace NAs
        if (NIBM[II,'Mature'] == 1){NIBM[idx + 1,'Mature'] <- 1} ## cannot un-mature
        if(MyAge < 15 & NIBM[II,'Mature'] == 0){
          NIBM[idx + 1,'Mature'] <- ifelse(runif(1,0,1) < P[MyAge+1],1,0)
        } else if(MyAge >= 15 & NIBM[II,'Mature'] == 0){
          NIBM[idx + 1,'Mature'] <- ifelse(runif(1,0,1) < P[MyAge],1,0)
        }
        idx <- idx + 1 ## bump down a row

      } ## end individuals
      for(ir in 1:RECy[ry]){ ## now add individuals
        NIBM[idx + 1,'UniqueID'] <- max(NIBM$UniqueID) + 1 ## new id; DO FIRST OTHERWISE FILLS NA
        NIBM[idx + 1,'Year'] <- ry
        NIBM[idx + 1,'Age'] <- 0
        NIBM[idx + 1, 'Length_cm'] <- L1*exp(rnorm(1,0,growth_lognormal_SD)-growth_lognormal_SD^2/2) ## recalc error each time
        NIBM[idx + 1, 'Weight'] <-  lw(NIBM[II,'Length_cm'])
        NIBM[idx + 1,'Mature'] <- ifelse(runif(1,0,1)< P[1],1,0)
        idx <- idx + 1
      } ## end adding recruits

      NIBM <- NIBM %>% filter(Age != -1) # To speed up remove all Age -1 animals at each time-step. idx is invalid now.
    } ## end simu_year

  } ## end of boots
    NIBM <- NIBM %>% filter(Year > burn_yr )%>% mutate('boot' = boot_number)

    return(NIBM)
} ## end make NIBM

