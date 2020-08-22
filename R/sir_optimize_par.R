#' @param data empirical counts
#' @param tspan times
#' @param p0 inital guess
#' @param y0 inital state of the system
# @param fixed_params fixed params
#' @param Np number of particles
#'
#' @importFrom stats optim
#'
sir_optimize_par <- function(data, tspan, dataStrat, guess_params0, pIsol, timeIsol, matrixCtctAge, propAge, popSize, lethalityAge, severityAge,
                             epsilon, sigma, Np) {
  res <- optim(guess_params0, # c(propAsympto0, transmRate0, vecEff0)
    SICR_smc2_par,
    t = tspan, Y = data, dataStrat = dataStrat,
    pIsol = pIsol, timeIsol = timeIsol, matrixCtctAge = matrixCtctAge,
    propAge = propAge, popSize = popSize, lethalityAge = lethalityAge, severityAge = severityAge,
    epsilon = epsilon, sigma = sigma,
    Np = Np,
    out = "lkl",
    # control = list(fnscale = -1,parscale=c(0.1,guess_params0[2]/5,rep(0.1,length(guess_params0)-2))), # to turn it into a maximisation problem
    control = list(fnscale = -1, parscale = c(10, 5, rep(10, length(guess_params0) - 2))), # to turn it into a maximisation problem
    lower = c(0, guess_params0[2] / 5, rep(0, length(guess_params0) - 2)),
    upper = c(1, guess_params0[2] * 5, rep(1, length(guess_params0) - 2)),
    method = "L-BFGS-B"
  )

  return(res)
}
