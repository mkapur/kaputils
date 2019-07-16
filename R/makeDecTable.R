#'  makeDecTable
#'
#' \code{makeDecTable} given a directory or set of directories, will stitch together info for decision table of 3x3 dimensions
#' @param rootdir  a master folder with all nine executed models.
#' @param state_names three names referring to the states of nature, which must match names of the directories. defaults to low/base/high
#' @param catch_names three names referring to the catch scenarios, which must match names of the directories. defaults to constant/ABC/upper
#' @param years a vector of years for which you'd like output
#' @param baseCatch a vector of catch values used for the base case model(s) (center row) -- must match length(years)
#' @param row1Catch a vector of catch values used for the first row model(s) (typically constant or lower stream)
#' @param row3Catch a vector of catch values used for the first row model(s) (typically constant or lower stream)
#' @param round logical, defaults to TRUE
#' @param writeTable logical, defaults to FALSE
#' @param writeloc if \code{writeTable == T}, directory to write output
#' @export


makeDecTable <- function(rootdir,state_names = c("low","base","high"),  catch_names = c('constant','ABC','upper'),
                         years,baseCatch,row1Catch,row3Catch, round = T, writeTable = F,
                         writeloc = getwd()){



  ## error trapping
  if(length(baseCatch) != length(years) | length(row1Catch) != length(years)   |length(row3Catch) != length(years)) {
    stop("length of input catch vectors must match length of input years \n")
  }
  if(length(dir(rootdir)) < 9) {
    stop("there are less than 9 directories in your root directory, not enough to fill table \n")
  }

  YOI <- years
  dec_table <- matrix(NA, nrow = length(YOI)*3, ncol = 9)
  dec_table <- data.frame(dec_table)
  names(dec_table) <- c('Scenario','Year','catch',paste(c("spawnbio","depl"),rep(state_names,each = 2)))
  dec_table$Year <- rep(YOI,3)
  idxr <- idxc <- 1 ## setup row and column indices
  for(catch in seq_along(catch_names)){ ## loop catch scen
    idxc <- 1 ## reset to initial column for new catch scenario
    for(state in seq_along(state_names)){
      tempdir <- dir(rootdir, full.names = T)[grepl(paste0(state_names[state],".*",catch_names[catch],"|",catch_names[catch],".*",state_names[state]), dir(rootdir))]
      mod <- SS_output(tempdir, covar = F); rm(tempdir)
      if(catch == 1 & idxc ==2){
        dec_table$catch[idxr:(idxr+length(YOI)-1)] <- round(row1Catch,2)
      } else if(catch == 3 & idxc ==2){
        dec_table$catch[idxr:(idxr+length(YOI)-1)] <- round(row3Catch,2)
      } else if (catch == 1 &  idxc ==2){
        dec_table$catch[idxr:(idxr+length(YOI)-1)] <- round(baseCatch,2)

      }

      ## input what was given to forecast file
      dec_table$Scenario[idxr:(idxr+length(YOI)-1)] <- rep(catch_names[catch], length(idxr:(idxr+length(YOI)-1)))
      dec_table[idxr:(idxr+length(YOI)-1),idxc*2+2] <-  mod$derived_quants[grep(paste0("SSB_",YOI,collapse = "|"),
                                                                                mod$derived_quants$Label),"Value"]
      dec_table[idxr:(idxr+length(YOI)-1),idxc*2+3] <-  mod$derived_quants[grep(paste0("Bratio_",YOI,collapse = "|"),
                                                                                mod$derived_quants$Label),"Value"]
      idxc <- idxc+1 ## move to next set of columns as state updates


    } ## end state
    idxr <- idxr+length(YOI) ## jump down to next set of years when catch scenario updates
  } ## end catch

  if(round  == T){
    dec_table[,c(5,7,9)] <- round(dec_table[,c(5,7,9)],2)
    dec_table[,c(4,6,8)] <- round(dec_table[,c(4,6,8)],0)
  }
  ## save dec_table
  if(writeTable == T){
    write.csv(dec_table,
              file = paste0(writeloc,"/decision_table_",
                            Sys.Date(),".csv"),
              row.names = F)
  } ## end if writeTable == T
  return(dec_table)

}
