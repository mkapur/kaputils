#' makeSeltxt
#'
#' \code{makeSeltxt} reshapes a dataframe with size-at-capture data into a selectivity text file suitable for SS3
#' @param size_csv csv with capture information; each row should be a fish, sex optional
#' @param size_col character name of column with measurement information; will drop NA rows
#' @param fishery_col character name of column with fishery names; used to form cuts
#' @param time_col character name of column by which time should be grouped; currently only supports one value (e.g. 'year')
#' @param sex logical. should we aggregate by sex? if so, the resultant text file will print females and males in that order
#' @param sex_col character name of column with sexes
#' @param sex_vals vector of sex designations, female then male
#' @param lBins vector of bin breaks



makeSeltxt <- function(size_csv,
                       size_col = "Measured.Length..cm.",
                       fishery_col = 'Fishery',
                       time_col = 'Year',
                       sex = F,
                       sex_col = 'Sex',
                       sex_vals = c('F','M'),
                       lBins = seq(50,250,5)){

  df <- read.csv(size_csv) %>% filter(!is.na(size_col))
  lbinwidth <- ceiling((last(lBins)-first(lBins))/length(lBins))

if(sex == T){

  df0 <- df %>%
    transform(group = cut(.[, size_col], breaks = lBins)) %>%
    group_by(Year, .[, fishery_col], group, .[, sex_col]) %>%
    dplyr::summarise(n = n())
  names(df0)[c(2, 4)] <- c(fishery_col, sex_col)

  df1 <- df0 %>%
    as.data.frame() %>%
    dcast(Year + .[, fishery_col] + .[, sex_col] ~ group)

    df1 %>% filter(sex_col == sex_vals[1]) -> females
    df1 %>% filter(sex_col == sex_vals[2]) -> males


  }

}
