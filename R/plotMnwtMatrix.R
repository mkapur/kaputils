#' plotMnwtMatrix
#' \code{plotMnwtMatrix} make a colored matrix showing years/sexes where model-estimated mean weight differed from input.
#' @param summaryoutput  generated via SS_output()
#' @param surveydata raw data frame -- in format from NWFSCAssess/survey
#' @param fleet defaults to 1; can currently only take one value at a time
#' @param maxage  the maximum age you'd like the plot to generate to
#' @param print do you want the plot to save in y
#' @export

plotMnwtMatrix <- function(summaryoutput, surveydata, fleet = 1, maxage = 30, png = F, printfolder){
  ## for each age-year-sex combo, compute the difference between SS mean and rawdat
  ## will drop NA sex/age/weights likely losing years, leading to blank
  survey_summary <- surveydata %>%
    filter(!is.na(Sex) & !is.na(Age) & !is.na(Weight)) %>%
    group_by(Sex, Year, Age, Project) %>%
    summarise(survey_meanwt = mean(Weight))

  model_summary <- summaryoutput$wtatage %>%
    filter(Fleet == fleet) %>%
    mutate(Year = Yr) %>%
    select(-Yr, -Seas, -Bio_Pattern, -BirthSeas, -Fleet) %>%
    melt(id = c("Year","Sex")) %>%
    mutate(Age = variable, SS_meanwt = as.numeric(value),
           Sex = ifelse(Sex == 1, 'F','M')) %>%
    select(-variable, - value) #%>% head()

  fulldat <-  merge(survey_summary, model_summary,
                    by = c("Year","Sex","Age")) %>%
    mutate(surv_minus_SS = survey_meanwt - SS_meanwt) %>%
    filter(Age <= maxage)
  fulldat$Sex <- as.character(factor(fulldat$Sex,
                                     levels = c("F", "M", "U"), labels = c("Female", "Male", "Unsexed")))

  p <- ggplot(fulldat, aes(x = Year, y = Age)) +
    theme_bw()+
    theme(panel.grid = element_blank(),
          legend.position = 'bottom') +
    geom_tile(aes(fill = surv_minus_SS)) +
    scale_fill_gradient2() +
    # scale_fill_gradient(low = "white", high = "red") +
    labs(fill = "Survey Mean Weight - SS mean Weight") +
    coord_flip() +
    scale_y_continuous(breaks = seq(0,30,5), sec.axis = dup_axis()) +
    scale_x_reverse() +
    geom_text(size = 2, aes(label = round(surv_minus_SS, 2))) +
    facet_wrap(~Sex)
  print(p)
  if(png) ggsave(plot = p, filename = file.path(printfolder, "/meanweight_plot.png"))
  return(fulldat)
}
