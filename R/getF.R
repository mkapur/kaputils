#' getF
#' Simple effort model from Carruthers et al. (2012) - function extracted from his package DLMtool in R
#' @param nsim  Number of simulated effort datasets
#' @param Esd  optional; if you already generated a MQ csv using extractResults it will read from that, otherwise need pattern/subpattern
#' @param nyears the character column name of the biomass ref point; can rename to change y lab
#' @param dFmin the character column name of the fishing ref point
#' @param dFmax 1st order directory character
#' @param bb 2nd order directory character
#' @param scale scaling for median F
#' @export

getF <- function(nsim,Esd,nyears,dFmin,dFmax,bb,scale){
  ne<-nsim*10
  dEfinal<-runif(ne,dFmin,dFmax)                        # Sample the final gradient in effort
  a<-(dEfinal-bb)/nyears                                # Derive slope to get there from intercept
  a<-array(a,dim=c(ne,nyears))                          # Slope array
  bb<-array(bb,dim=c(ne,nyears))                        # Intercept array
  x<-array(rep(1:nyears,each=ne),dim=c(ne,nyears))      # Year array
  dE<-a*x+bb                                            # Change in effort
  E<-array(NA,dim=c(ne,nyears))                         # Define total effort array
  E[,1]<-dE[,1]
  for(y in 2:nyears){
    E[,y]<-apply(dE[,1:y],1,sum)
  }

  E<-scale*E/array(apply(E,1,mean),dim=c(ne,nyears))          # Standardise Effort to average 1

  cond<-apply(E,1,min)>0
  pos<-(1:ne)[cond]
  pos<-pos[1:nsim]

  E<-E[pos,]                                            # Sample only those without negative effort
  Emu<--0.5*Esd^2
  Eerr<-array(exp(rnorm(nyears*nsim,rep(Emu,nyears),rep(Esd,nyears))),c(nsim,nyears))
  E*Eerr
}


