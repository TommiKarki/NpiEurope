#' @inheritParams LHS_init
#'
#' @importFrom slider slide_chr
get_strats <- function(country_data, min_duration = 1) {

  strat <- country_data[, 6:31]
  list_strats <- names(strat)

  dataStrat <- apply(strat, 1, function(s) paste(list_strats[as.logical(s)], collapse = " "))

  # Loop until all strategies reach at least min_duration
  # EXCEPTED the first and last strategy! (or you get an infinite loop)
  while(any(table(dataStrat)[!names(table(dataStrat)) %in% dataStrat[c(1, length(dataStrat))]] < min_duration)) {
    dataStrat <- slider::slide_chr(dataStrat, Mode, .after = ceiling(min_duration/2)+1, .before = ceiling(min_duration/2)+1)
  }

  return(dataStrat)

}

# From https://stackoverflow.com/a/8189441
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
