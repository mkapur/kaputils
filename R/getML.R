## Code to prep data as used in SSplotMnwt
## Identical to r4ss::SSMethod.TA1.8() except it returns the actual CSV i naddition to plots
## kapurm@uw.edu
## Adapted from https://rdrr.io/cran/r4ss/src/R/SSplotMnwt.R and https://rdrr.io/cran/r4ss/src/R/SSMethod.TA1.8.R

SSMethod.TA1.8.MK <- function(fit,
           type,
           fleet,
           part = 0:2,
           pick.gender = 0:3,
           seas = NULL,
           method = NULL,
           plotit = TRUE,
           maxpanel = 1000)
  {

  # Check the type is correct and the pick.gender is correct
  is.in <- function (x, y)!is.na(match(x, y))
  if(!is.in(type,c('age','len','size','con'))){
    stop('Illegal value for type')
  }else{
    if(sum(!is.in(pick.gender,c(0:3)))>0){
      stop('Unrecognised value for pick.gender')
    }
  }

  # Select the type of datbase
  dbase <- fit[[paste(type,'dbase',sep='')]]
  sel <- is.in(dbase$Fleet,fleet) & is.in(dbase$Part,part)
  if(type!='con') sel <- sel & is.in(dbase$'Sex',pick.gender)
  # if(type!='con') sel <- sel & is.in(dbase$'Pick_gender',pick.gender)

  if(type=='size' & !is.null(method))
    sel <- sel & is.in(dbase$method,method)
  if(sum(sel)==0) return()
  dbase <- dbase[sel,]
  if(is.null(seas)){
    seas <- 'comb'
    if(length(unique(dbase$Seas))>1)
      cat('Warning: combining data from multiple seasons\n')
  }
  # create label for partitions
    partitions <- sort(unique(dbase$Part)) # values are 0, 1, or 2
    partition.labels <- c("whole","discarded","retained")[partitions+1]
    partition.labels <- paste("(",paste(partition.labels,collapse="&")," catch)",sep="")
    # gender.flag <- type!='con' & max(tapply(dbase$'Pick_gender',
    #                                         dbase$Fleet,function(x)length(unique(x))))>1
    gender.flag <- type!='con' & max(tapply(dbase$'Sex',
                                            dbase$Fleet,function(x)length(unique(x))))>1

    indx <- paste(dbase$Fleet,dbase$Yr,if(type=='con')dbase$'Lbin_lo' else
      '',if(seas=='sep')dbase$Seas else '')
    if(gender.flag)indx <- paste(indx,dbase$'Sex')
    method.flag <- if(type=='size') length(unique(dbase$method))>1 else FALSE
    if(method.flag)
      indx <- paste(indx,dbase$method)
    uindx <- unique(indx)
    if(length(uindx)==1){
      # presumably the method is meaningless of there's only 1 point,
      # but it's good to be able to have the function play through
      cat('Warning: only one point to plot\n')
      return()
    }

    pldat <- matrix(0,length(uindx),10,
                    dimnames=list(uindx,
                                  c('Obsmn','Obslo','Obshi','semn','Expmn','Std.res',
                                    'ObsloAdj','ObshiAdj','Fleet','Yr')))
    if(type=='con')pldat <- cbind(pldat,Lbin=0)
    # if(gender.flag)pldat <- cbind(pldat,pick.gender=0)
    if(gender.flag)pldat <- cbind(pldat,Sex=0)
    if(method.flag)pldat <- cbind(pldat,method=0)

    # Find the weighting factor for this combination of factors
    for(i in 1:length(uindx)){  # each row of pldat is an individual comp
      subdbase <- dbase[indx==uindx[i],]
      xvar <- subdbase$Bin
      pldat[i,'Obsmn'] <- sum(subdbase$Obs*xvar)/sum(subdbase$Obs)
      pldat[i,'Expmn'] <- sum(subdbase$Exp*xvar)/sum(subdbase$Exp)
      pldat[i,'semn'] <- sqrt((sum(subdbase$Exp*xvar^2)/sum(subdbase$Exp)-
                                 pldat[i,'Expmn']^2)/mean(subdbase$N))
      pldat[i,'Obslo'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']
      pldat[i,'Obshi'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']
      pldat[i,'Std.res'] <- (pldat[i,'Obsmn']-pldat[i,'Expmn'])/pldat[i,'semn']
      pldat[i,'Fleet'] <- mean(subdbase$Fleet)
      pldat[i,'Yr'] <- mean(if(seas=='comb')subdbase$Yr else subdbase$Yr.S)
      if(type=='con')pldat[i,'Lbin'] <- mean(subdbase$'Lbin_lo')
      if(gender.flag)
        # pldat[i,'pick.gender'] <- mean(subdbase$'Pick_gender')
        pldat[i,'Sex'] <- mean(subdbase$'Sex')

      if(method.flag)
        pldat[i,'method'] <- mean(subdbase$method)
    }
    Nmult <- 1/var(pldat[,'Std.res'],na.rm=TRUE)

    # Find the adjusted confidence intervals
    for(i in 1:length(uindx)){
      pldat[i,'ObsloAdj'] <- pldat[i,'Obsmn']-2*pldat[i,'semn']/sqrt(Nmult)
      pldat[i,'ObshiAdj'] <- pldat[i,'Obsmn']+2*pldat[i,'semn']/sqrt(Nmult)
    }

    Nfleet <- length(unique(pldat[,'Fleet']))
    # make plot if requested
    if(plotit){
      plindx <- if(type=='con'){
        paste(pldat[,'Fleet'],pldat[,'Yr'])
      }else{
        pldat[,'Fleet']
      }
      # if(gender.flag)plindx <- paste(plindx,pldat[,'pick.gender'])
      if(gender.flag)plindx <- paste(plindx,pldat[,'Sex'])

      if(method.flag)plindx <- paste(plindx,pldat[,'method'])
      uplindx <- unique(plindx)

      # Select number of panels
      Npanel <- length(uplindx)
      ## Ian T. 9/25/14: changing from having at least 4 panels to no minimum
      #NpanelSet <- max(4,min(length(uplindx),maxpanel))
      NpanelSet <- min(length(uplindx),maxpanel)
      Nr <- ceiling(sqrt(NpanelSet))
      Nc <- ceiling(NpanelSet/Nr)
      # save current graphical parameters
      par_current <- par()
      # set new parameters
      par(mfrow=c(Nr,Nc),mar=c(2,2,1,1)+0.1,mgp=c(0,0.5,0),oma=c(1.2,1.2,0,0),
          las=1)
      par(cex=1)
      for(i in 1:Npanel){
        subpldat <- pldat[plindx==uplindx[i],,drop=FALSE]
        x <- subpldat[,ifelse(type=='con','Lbin','Yr')]
        plot(x,subpldat[,'Obsmn'],pch='-',
             xlim=if(length(x)>1)range(x) else c(x-0.5,x+0.5),
             ylim=range(subpldat[,c('Obslo','Obshi','ObsloAdj','ObshiAdj','Expmn')],
                        na.rm=TRUE),
             xlab='',ylab='')
        segments(x,subpldat[,'Obslo'],x,subpldat[,'Obshi'],lwd=3)
        arrows(x,subpldat[,'ObsloAdj'],x,subpldat[,'ObshiAdj'],lwd=1,
               length=0.04, angle=90, code=3)
        points(x,subpldat[,'Obsmn'],pch=21,bg='grey80')
        ord <- order(x)
        if(length(x)>1){
          lines(x[ord],subpldat[ord,'Expmn'],col=4)
        }else{
          lines(c(x-0.5,x+0.5),rep(subpldat[,'Expmn'],2),col=4)
        }
        # Lines
        fl <- fit$FleetNames[subpldat[1,'Fleet']]
        yr <- paste(subpldat[1,'Yr'])
        lab <- if(type=='con')ifelse(Nfleet>1,paste(yr,fl),yr) else fl
        if(gender.flag)lab <-
          # paste(lab,ifelse(subpldat[1,'pick.gender']==0,'comb','sex'))
        paste(lab,ifelse(subpldat[1,'Sex']==0,'comb','sex'))

        if(method.flag)lab <- paste(lab,'meth',subpldat[1,'method'])
        lab <- paste(lab,partition.labels)
        mtext(lab,side=3,at=mean(x))
      }
      mtext(paste('Mean',ifelse(is.in(type,c('len','size')),'length','age')),
            side=2,las=0,outer=TRUE)
      mtext(ifelse(type=='con','Length','Year'),side=1,outer=TRUE)
      # restore previous graphics parameters
      par(mfrow=par_current$mfrow, mar=par_current$mar, mgp=par_current$mgp,
          oma=par_current$oma, las=par_current$las)
    }
    tmp <- matrix(sample(pldat[,'Std.res'],1000*nrow(pldat),replace=TRUE),nrow(pldat))
    confint <- as.vector(quantile(apply(tmp,2,function(x)1/var(x,na.rm=TRUE)),
                                  c(0.025,0.975),na.rm=TRUE))
    Output <- c(w=Nmult,lo=confint[1],hi=confint[2])
    Outs <- paste("Francis Weights - ", type, ": ", fit$FleetNames[fleet],": ",
                  round(Nmult,4), " (",round(confint[1],4),"-",round(confint[2],4),")",
                  sep="")
    print(Outs)
    # return(Output)
    return(pldat)
  }



## not run:
# require(r4ss)
# require(dplyr)

# mod <- SS_output("C:/Users/mkapur/Dropbox/UW/coursework/FISH-555/stm_mods/wp_test/Model_716", covar = F) ## folder where ss outputs are (rep file etc)
# francisdat <- SSMethod.TA1.8.MK(mod, type = 'len',
#                                 fleet = 1:5, pick.gender = 0:3, plotit = T) %>% as.data.frame()
# francisdat$Fleet <- mod$FleetNames[francisdat$Fleet] ## name the actual fleets
# write.csv(francisdat,"francisDat.csv",row.names=F) ## save it

## load and save to an RDATA if needed
# load("smabase.rdata",verbose=T)
# smabase$mnlen <- francisdat

