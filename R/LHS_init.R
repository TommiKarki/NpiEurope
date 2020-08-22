#' Run particle filter or stochastic simulations
#'
#' @param country_data Epidemiological dataset
#' @param contact_data Contact dataset
#' @param age_data Age dataset
#' @param prop_asympto Proportion of undetected cases
#' @param Np Integer. Number of simulations. This also corresponds to the number
#' of particles in the particle filter.
#' @param Niter Integer. Number of iterations in the MCMC when `task = "estimate`
#' @param task The task to perform. Either `"estimate"` to estimate parameters
#' or `"run` to run simulations (`transmRate`, and `vecEff` must
#' be provided in this case).
#' @param transmRate,vecEff,R0 Epidemiological parameters. Only one of
#' `transmRate` and `R0` can be provided.
#'
#' @export
#'
LHS_init <- function(country_data, contact_data, age_data,
                     Np = 20, Niter = 10e5, task = c("estimate", "run"),
                     transmRate, vecEff, R0) {

  if (!missing(transmRate) & !missing(R0)) {
    stop("Only one of `transmRate` and `R0` can be provided.", call. = FALSE)
  }

  task <- match.arg(task)

  dates <- as.Date(country_data$Date)
  cases <- country_data$NewCases
  prop_asympto <- country_data$PropAsympto

  pMatrix <- matrix(NA_real_, ncol = 8, nrow = 8)
  for (i in 1:8) {
    for (j in 1:8) {
      pMatrix[i, j] <- sum(contact_data[c((i - 1) * 2 + 1, (i - 1) * 2 + 2), c((j - 1) * 2 + 1, (j - 1) * 2 + 2)])
    }
  }

  dataStrat <- get_strats(country_data, min_duration = 5)

  t <- c(0, seq_along(cases))

  # Fixed parameters
  epsilon <- 1 / 3
  sigma <- 1 / 5

  popAge <- rep(0, 8)
  for (i in 1:8) {
    popAge[i] <- sum(age_data[c((i - 1) * 2 + 1, (i - 1) * 2 + 2), c(2, 3)])
  }
  propAge <- popAge / sum(popAge)
  popSize <- sum(popAge)
  lethalityAge <- rep_len(0.01, length(propAge))
  severityAge <- rep_len(0.1, length(propAge))

  if (task == "estimate") {

    # Parameters to estimate: initial values
    if (missing(transmRate) & missing(R0)) {
      transmRate <- 2.5 * (epsilon + sigma) / sum((popAge * rowSums(pMatrix)))
    } else if (missing(transmRate)) {
      transmRate <- R0 * (epsilon + sigma) / sum((popAge * rowSums(pMatrix)))
    }

    if (missing(vecEff)) {
      # cat("R0", sum(transmRate0 * (popAge * rowSums(pMatrix))) / (epsilon + sigma))
      vecEff <- rep(0.8, length(unique(dataStrat)) - 1)
    } else if (length(vecEff) == 1 & length(unique(dataStrat) != 1)) {
      message("Starting value for vecEff is copied for all mitigation strategies")
      vecEff <- rep(vecEff, length(unique(dataStrat)) - 1)
    }

    guess0 <- c(transmRate, vecEff)

    MLE <- run_MCMC(
      data = cases, tspan = t, dataStrat, guess0,
      pIsol = 0, timeIsol = 0,
      matrixCtctAge = pMatrix, propAge, popSize, lethalityAge, severityAge,
      epsilon, sigma, prop_asympto, Np, Niter,
      as.character(country_data$Country[1]))

    return(MLE)

  } else if (task == "run") {

    outReprod <- SICR_smc2_par(c(transmRate, vecEff),
                               t = t, Y = cases, dataStrat,
                               pIsol = 0, timeIsol = 0, matrixCtctAge = pMatrix,
                               propAge, popSize, lethalityAge, severityAge,
                               epsilon, sigma, prop_asympto, Np,
                               out = "cases")

    return(outReprod)

  }

}
