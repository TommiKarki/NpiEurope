#' Plot the proportion of asymptomatic
#'
#' @param country_data the result of [estimate_asympto()]
#'
#' @export
#'
plot_asympto <- function(country_data) {

  ggplot(country_data, aes(x = Date, y = PropAsympto)) +
    geom_line() +
    ylim(c(0, 1))

}
