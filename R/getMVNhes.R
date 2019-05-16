#' get_mvnhes
#' Function to generate mvn from var and corr of log (F/Fmsy) and log(SSB/SSBmsy) extracted from the hessian. Author: Felipe Carvalho
#' @param rootdir home directory
#' @param mq_csv  optional; if you already generated a MQ csv using extractResults it will read from that, otherwise need pattern/subpattern
#' @param axes.limits x and y maximums for kobe plot, default to 2
#' @param kobe.type if 'ISC', will have pale orange coloring in place of orange/yellow
#' @param b.name the character column name of the biomass ref point; can rename to change y lab
#' @param f.name the character column name of the fishing ref point
#' @param pattern 1st order directory character
#' @param subpattern 2nd order directory character
#' @param saveplot logical, do you want to save the plot
#' @param plotloc where to save plot
#' @param doLegend logical. do you want to write a legend (not advised for huge rep sets)



getMVNhes <- function(data=out,pars=c('Bratio','F'),yr=2017,yr.cov=NULL,mc=10000){
  if(is.null(yr.cov)) yr.cov=yr
  out=data

  x= out$CoVar
  x <- x[x$label.i %in% paste0(pars,"_",yr.cov),]
  x$label.j[x$label.j=="_"] <- x$label.i[x$label.j=="_"]
  x <- x[x$label.j %in% paste0(pars,"_",yr.cov),]
  y = out$derived_quants
  y = y[y$Label %in% paste0(pars,"_",yr.cov),] # old version Label not LABEL
  varF = (y$StdDev[1]/y$Value[1])^2 # variance log(F/Fmsy)
  varB = (y$StdDev[2]/y$Value[2])^2 # variance log(SSB/SSBmsy)
  corr = log(1+x$corr[3]*sqrt(varF*varB))#/(y$Value[1]*y$Value[2])#*x$corr[3]*sqrt(varF*varB)) #log(1+x$corr[3])#/(y$Value[1]*y$Value[2])
  y = out$derived_quants
  y = y[y$Label %in% paste0(pars,"_",yr),] # old version Label not LABEL
  # MVN means of SSB/SBBmsy and F/Fsmy
  mvnmu = log(c(y$Value[2],y$Value[1])) # Assume order F_ then Bratio_
  # Create MVN-cov-matrix
  #corr1 = log(1+x$corr[3])/(y$Value[1]*y$Value[2])
  mvncov = matrix(c(varB,rep(corr,2),varF),ncol=2,nrow=2)
  kb = data.frame(exp(rmvnorm(mc ,mean = mvnmu,sigma = mvncov))) # random  MVN generator
  colnames(kb) = c("stock","harvest")
  return(list(kb=kb,mle=rev(y$Value)))}




