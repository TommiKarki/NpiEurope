#' Compare simulated trajectory with estimated parameters to empirical data
#'
#' @inheritParams LHS_init
#'
#' @export
compare_simdata <- function(country_data, contact_data, age_data, Np = 100, Niter = 1e5) {

  test <- LHS_init(country_data, contact_data, age_data,
                   task = "estimate", Np = Np, Niter)

  message(sprintf(
    "Estimated params:\n propAsympto = %.2f\n transmRate = %.2e\n vecEff = %s",
    test$par[1], test$par[2], paste(test$par[3: length(test)], collapse = " "))
  )

  sims <- LHS_init(country_data, contact_data, age_data,
                   task = "run",
                   propAsympto = test$par[1], transmRate = test$par[2], vecEff = test$par[3:length(test$par)],
                   Np = Np)

  plot_simulations(sims, country)
}

