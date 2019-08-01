#' errorTables
#' parsing of input selectivity plots to show by-fleet, by-sex curves as proposed in SS3 Control File
#' @param moddir filepath where Report.sso is stored
#' @param write logical. should plots be saved to model directory
#' @param type type of file to be saved
#' @export
errorTables <- function(refList, write = T){
  ## extract OM
  opmod <- refList[grepl("OM", refList$MOD), ] %>%    melt(id = c("MOD","REP"))

  ## extract EM
  emod <-  refList[grepl("EM", refList$MOD), ] %>%
    melt(id = c("MOD","REP"))

  relErr <- merge(opmod, emod, by = c("variable","REP")) %>%
    plyr::rename(c("value.x" = "OM_Value", 'MOD.y' = "Estimation_Model", "value.y" = "EM_Value")) %>%
    select(-MOD.x) %>%
    mutate("RelErr" = EM_Value - OM_Value)

  if(write) write.csv(relErr, paste0(RootFile,"/results/relative_error.csv"), row.names = F)

  RMSE <- relErr %>%
    group_by(Estimation_Model, variable) %>%
    summarise('RMSE' = sqrt(mean(RelErr^2)))
  if(write) write.csv(RMSE, paste0(RootFile,"/results/RMSE.csv"), row.names = F)

  MARE <- relErr %>%
    group_by(Estimation_Model) %>%
    summarise('RMSE' = median(abs(RelErr)))
  if(write) write.csv(RMSE, paste0(RootFile,"/results/MARE.csv"), row.names = F)
  return(list(relErr, RMSE, MARE))
}
