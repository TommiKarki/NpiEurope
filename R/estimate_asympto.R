#' Estimate the proportion of asymptomatic cases by capture/recapture
#'
#' @param country_data The output of [load_country_data()]
#' @param smooth Odd integer. Number of days on which smoothing for PropAsympto
#'  should be performed. Defaults to 1 (no smoothing).
#'
#' @export
#'
#' @importFrom dplyr %>% mutate
#' @importFrom slider slide_dbl
#'
estimate_asympto <- function(country_data, smooth = 1) {

  if (smooth %% 2 == 0) {
    stop("smooth must be odd", call. = FALSE)
  }

  country_data <- country_data %>%
    mutate(NewAsymptoCases = round(NewCases*(NewCases-1) / (1 + pmax(0, dplyr::lag(NewCases) - NewDeaths)))) %>%
    mutate(NewAsymptoCases = ifelse(is.na(NewAsymptoCases), 0, NewAsymptoCases),
           TotNewCases = NewCases + NewAsymptoCases,
           PropAsympto = ifelse(TotNewCases !=0, NewAsymptoCases / TotNewCases, 0)) %>%
    mutate(PropAsympto = slide_dbl(PropAsympto, mean, .before = (smooth-1)/2, .after = (smooth-1)/2, .complete = FALSE))


}
