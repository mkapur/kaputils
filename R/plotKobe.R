#' plotKobe
#' terminal kobe plot for a singly model. check out FLR.
#' @param rootdir home directory
#' @param terminal_only  Do you only want to show the final year or the whole timeseries?
#' @param terminal_year  Either a value or vector corresponding to the terminal year in each model.
#' @param axes.limits x and y maximums for kobe plot, default to 2
#' @param kobe.type if 'ISC', will have pale orange coloring in place of orange/yellow
#' @param b.name the character column name of the biomass ref point; can rename to change y lab
#' @param f.name the character column name of the fishing ref point
#' @param pattern 1st order directory character
#' @param subpattern 2nd order directory character
#' @param saveplot logical, do you want to save the plot
#' @param plotloc where to save plot
#' @param doLegend logical. do you want to write a legend (not advised for huge rep sets)



plotKobe <- function(summaryoutput,
                     terminal_only = c(T,F)[1],
                     terminal_year = 2015,
                     axes.limits = c(2,2),
                     kobe.type = c(NA,'ISC')[1],
                     b.name = NA,
                     f.name = NA,
                     saveplot = T,
                     plotloc = NA,
                     doDistrib = FALSE,

                     doLegend = T) {

  ## plotting defaults
  graphics.off()

  if (is.na(plotloc)) {
    if (!exists(paste0(getwd(), "/plots/"))) {
      dir.create(paste0(getwd(), "/plots/"))
    }
    plotloc <- paste0(getwd(), "/plots/")
  } ## end plotploc

  if (saveplot){
    jpeg(
      paste0(plotloc, "kobe.jpg"),
      width = 8,
      height = 6,
      units = 'in',
      res = 420
    )} ## end saveplot

  par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

  # Kobe plot layout setting
  x_max = axes.limits[1]
  x_min = 0
  y_max = axes.limits[2]
  y_min = 0


  plot(
    c(x_min, x_max),
    c(y_min, y_max),
    type = "n",
    ylab = "",
    xlab = "",
    xaxs = "i",
    yaxs = "i",
    bty = "n"
  )
  mtext(
    side = 1,
    expression(SSB / SSB[MSY]),
    line = 2.5,
    cex = 1
  )
  mtext(side = 2,
        expression(F / F[MSY]),
        line = 2.5,
        cex = 1)

  if(is.na(kobe.type)){
    polygon(c(x_min, 1, 1, x_min),
            c(1, 1, x_min, x_min),
            col = "gold",
            border = NA)
    polygon(c(1, x_max, x_max, 1),
            c(1, 1, 0, 0),
            col = "forestgreen",
            border = NA)
    polygon(c(0, 1, 1, 0),
            c(1, 1, y_max, y_max),
            col = "red",
            border = NA)
    polygon(c(1, x_max, x_max, 1),
            c(1, 1, y_max, y_max),
            col = "goldenrod",
            border = NA)
  }else if(kobe.type == 'ISC'){
    polygon(c(x_min,1,1,x_min), c(1,1,y_min,y_min),col="khaki1")
    polygon(c(1,x_max,x_max,1), c(1,1,y_min,y_min),col="palegreen")
    polygon(c(x_min,1,1,x_min), c(1,1,y_max,y_max),col="salmon")
    polygon(c(1,x_max,x_max,1), c(1,1,y_max,y_max),col="khaki1")
  }

  if(terminal.only){
    with(mtemp$Kobe[nrow(mtemp$Kobe), ],
         points(
           B.Bmsy,
           F.Fmsy,
           pch = 25,
           bg = "grey88",
           col = 'blue'
         ))
  } else if(terminal.only == FALSE){
    # if(is.na( df[i,b.name])  | is.na( df[i,f.name])) stop("Your biomass or F column in management_quantities is NA. \n Did your model(s) estimate F/FMSY or B/BMSY for the final year? Check SPRSeries.csv")

    B_BMSY = summaryoutput$Kobe$B.Bmsy
    F_FMSY = summaryoutput$Kobe$F.Fmsy
    lines(
      B_BMSY,
      F_FMSY, col = 'grey22', lty = 'dashed'
    )

    points(
      B_BMSY,
      F_FMSY,
      pch = 21,
      col = c(rep('grey44',(length(B_BMSY)-2)),'blue'),
      bg =  c(rep('white',(length(B_BMSY)-2)),'black'),
      cex = 1.1
    )
    text(summaryoutput$Kobe$B.Bmsy[summaryoutput$Kobe$Yr == summaryoutput$endyr],
         summaryoutput$Kobe$F.Fmsy[summaryoutput$Kobe$Yr == summaryoutput$endyr]*0.9,
         paste(summaryoutput$endyr))

    text(summaryoutput$Kobe$B.Bmsy[summaryoutput$Kobe$Yr == summaryoutput$startyr + which.max(F_FMSY)],
         summaryoutput$Kobe$F.Fmsy[summaryoutput$Kobe$Yr == summaryoutput$startyr + which.max(F_FMSY)],
         paste(summaryoutput$startyr + which.max(F_FMSY)))

    text(summaryoutput$Kobe$B.Bmsy[summaryoutput$Kobe$Yr == summaryoutput$startyr],
         summaryoutput$Kobe$F.Fmsy[summaryoutput$Kobe$Yr == summaryoutput$startyr]*0.9,
         paste(summaryoutput$startyr))

    if(doDistrib == TRUE){
      kb_hes <-
        getMVNhes(
          data = mtemp,
          pars = c('Bratio', 'F'),
          yr = ifelse(length(terminal_year) > 1, terminal_year[m], terminal_year),
          yr.cov = ifelse(length(terminal_year) > 1, terminal_year[m], terminal_year),
          mc = 10000
        )
      kbs <- mles <- NULL
      kb <- kb_hes$kb
      kb$run <-  modnames[m]
      mle <- kb_hes$mle #  Bratio, F/Fmsy
      mle <- data.frame(t(mle))
      mle$run <-  unique(kb$run)
      colnames(mle) <- c("stock","harvest","run")
      kbs <- rbind(kbs,kb)
      mles <- rbind(mles,mle)

      # Get Kobes
      pkb  <- kbs[kbs$run == runs[m],1:2]
      pmle <- (mles[mles$run==runs[m],1:2])
      trj <- get_trj(mtemp)

      # fit kernel function
      kernelF <-
        ci2d(
          pkb[, 1],
          pkb[, 2],
          nbins = 151,
          factor = 1.5,
          ci.levels = c(0.50, 0.80, 0.75, 0.90, 0.95),
          show = "none",
          col = 1,
          xlab = ifelse(f.name == "Fmsy", expression(paste(F / F[MSY])), expression(paste(H /
                                                                                            H[MSY]))),
          ylab = expression(paste(B / B[MSY]))
        )
      plot(1000,1000,type="b", xlim=c(0,max(2.5,quantile(pkb$stock,0.98),trj[,2]*1.05)), ylim=c(0,max(2.5,quantile(pkb$harvest,0.98),trj[,3])),lty=3,ylab=ifelse(f.name=="Fmsy",expression(paste(F/F[MSY])),expression(paste(H/H[MSY]))),xlab=expression(paste(SSB/SSB[MSY])),xaxs="i",yaxs="i")
      c1 <- c(-1,100)
      c2 <- c(1,1)
      # extract interval information from ci2d object
      # and fill areas using the polygon function
      zb2 = c(0,1)
      zf2  = c(1,100)
      zb1 = c(1,100)
      zf1  = c(0,1)
      polygon(c(zb1,rev(zb1)),c(0,0,1,1),col="green",border=0)
      polygon(c(zb2,rev(zb2)),c(0,0,1,1),col="yellow",border=0)
      polygon(c(1,100,100,1),c(1,1,100,100),col=ifelse(KOBE.type=="ICCAT","yellow","orange"),border=0)
      polygon(c(0,1,1,0),c(1,1,100,100),col="red",border=0)

      lines(c1,c2,lty=3,lwd=0.7)
      lines(c2,c1,lty=3,lwd=0.7)

      polygon(kernelF$contours$"0.95",lty=2,border=NA,col="cornsilk4")
      polygon(kernelF$contours$"0.8",border=NA,lty=2,col="grey")
      polygon(kernelF$contours$"0.5",border=NA,lty=2,col="cornsilk2")
      points(trj[,2:3],pch=16,cex=1)
      lines(trj[,2:3],lty=2,col=1)
      sel.yr = c(1,round(quantile(1:nrow(trj),0.7),0),nrow(trj))
      points(trj[sel.yr,2],trj[sel.yr,3],col=
               1,pch=c(22,21,24),bg="white",cex=1.9)


      b = pkb[,1]; f = pkb[,2]
      # Get Propability
      Pr.green = sum(ifelse(b>1 & f<1,1,0))/length(b)*100
      Pr.red = sum(ifelse(b<1 & f>1,1,0))/length(b)*100
      if(KOBE.type=="ICCAT"){
        Pr.yellow = (sum(ifelse(b<1 & f<1,1,0))+sum(ifelse(b>1 & f>1,1,0)))/length(b)*100} else {
          Pr.yellow = sum(ifelse(b<1 & f<1,1,0))/length(b)*100
          Pr.orange = sum(ifelse(b>1 & f>1,1,0))/length(b)*100
        }
      sel.years = c(trj[sel.yr,1])
      ## Add legend
      if(KOBE.type=="ICCAT"){
        legend('topright',
               c(paste(sel.years),"50% C.I.","80% C.I.","95% C.I.",paste0(round(c(Pr.red,Pr.yellow,Pr.green),1),"%")),
               lty=c(1,1,1,rep(-1,7)),pch=c(22,21,24,rep(22,7)),pt.bg=c(rep("white",3),"cornsilk2","grey","cornsilk4","red","yellow","green"),
               col=1,lwd=1.1,cex=0.9,pt.cex=c(rep(1.3,3),rep(1.7,3),rep(2.1,3)),bty="n")
      }else{
        legend('topright',
               c(paste(sel.years),"50% C.I.","80% C.I.","95% C.I.",paste0(round(c(Pr.red,Pr.yellow,Pr.orange,Pr.green),1),"%")),
               lty=c(1,1,1,rep(-1,8)),pch=c(22,21,24,rep(22,8)),pt.bg=c(rep("white",3),"cornsilk2","grey","cornsilk4","red","yellow","orange","green"),
               col=1,lwd=1.1,cex=0.9,pt.cex=c(rep(1.3,3),rep(1.7,3),rep(2.2,4)),bty="n")

      }
    } ## end doDistrib == True
  } ## end not terminal only
  graphics.off()

} ## end function

## not run:

# plotKobe(summaryoutput,
#                      terminal.only = c(T,F)[2],
#                      axes.limits = c(3,3),
#                      kobe.type = c(NA,'ISC')[2],
#                      b.name = 'B/BMSY',
#                      f.name = 'F/FMSY',
#                      saveplot = T,
#                      plotloc = "C:/Users/Maia Kapur/Dropbox/UW/coursework/FISH-555/stm_mods/Model_716/")
