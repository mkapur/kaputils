ss_trjs = function(summaryoutput,pars=c('SSB','Bratio','F'),plot.path = getwd()){
  summaryoutput=data
  yrs = unique(summaryoutput$timeseries$Yr[summaryoutput$timeseries$Era=="TIME"])
  d. = summaryoutput$derived_quants
  logCIs = function(mu,se){
    log.sd = sqrt(log(1+(se/mu)^2))
    logCIs =data.frame(exp(log(mu)+(1.96*log.sd)%o%c(-1,1)))
    colnames(logCIs) = c("lci","uci")
    return(logCIs)
  }
  ssb =  d.[d.$Label %in% paste0(paste0(pars[1],"_"),yrs),2:3]
  stock = d.[d.$Label %in% paste0(paste0(pars[2],"_"),yrs),2:3] # old version Label not LABEL
  harvest = d.[d.$Label %in% paste0(paste0(pars[3],"_"),yrs),2:3]
  ssb = data.frame(yr=yrs, ssb,logCIs(ssb[,1],ssb[,2]))
  stock = data.frame(yr=yrs,stock,logCIs(stock[,1],stock[,2]))
  harvest = data.frame(yr=yrs,harvest,logCIs(harvest[,1],harvest[,2]))
  return(list(ssb=ssb,stock=stock,harvest=harvest))
}
# get_trj
get_trj <- function(summaryoutput,pars = c('Bratio','F')){
  yrs <- summaryoutput$sprseries$Yr[-length(summaryoutput$sprseries$Yr)]
  y <- summaryoutput$derived_quants
  stock <- y[y$Label %in% paste0(paste0(pars[1],"_"),yrs),2] # old version Label not LABEL
  harvest <- y[y$Label %in% paste0(paste0(pars[2],"_"),yrs),2]
  return(data.frame(year<-yrs,stock,harvest))
}
