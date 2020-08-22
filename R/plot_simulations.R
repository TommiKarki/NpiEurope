#' Plot both the simulations from estimated parameters and empirical data
#'
#' @importFrom dplyr mutate group_by summarise
#' @importFrom tibble tibble
#' @import ggplot2
#' @importFrom stats quantile
#'
#' @inheritParams LHS_init
#' @param outreprod Result of the simulations
#'
#' @export
#'
plot_simulations <- function(outreprod, country_data) {

  tibble("sim_cases" = c(outreprod)) %>%
    mutate(sim_id = rep(seq_along(country_data$Date), length.out = length(sim_cases)),
           sim_dates = rep(country_data$Date, each = length(sim_cases) / length(country_data$Date))) %>%
    group_by(sim_dates) %>%
    summarise(low_cases = quantile(sim_cases, 0.05,na.rm=TRUE),
              high_cases = quantile(sim_cases, 0.95,na.rm=TRUE)) %>%
    ggplot(aes(x = sim_dates)) +
      geom_ribbon(aes(ymin = low_cases, ymax = high_cases)) +
      geom_line(data = country_data, aes(x = Date, y = NewCases), col = "red") +
    labs(x = "Date", y = "New cases") +
    theme_minimal()

}
