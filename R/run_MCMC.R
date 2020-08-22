#' @importFrom mvtnorm rmvnorm
#' @importFrom stats runif
run_MCMC <- function(data, tspan, dataStrat, guess_params0, pIsol, timeIsol, matrixCtctAge, propAge, popSize, lethalityAge, severityAge,
                     epsilon, sigma, prop_asympto, Np, Niter, pCountry) {
print(pCountry)
  MCMC_params <- list()
  MCMC_params$iterations <- Niter  # MCMC steps
  MCMC_params$log_steps <- 1  # log MCMC samples every x steps
  MCMC_params$save_steps <- 10  # save MCMC samples every x steps
  MCMC_params$burn_in <- 100

  theta_cov_start <- rep_len(0.01, length.out = length(guess_params0))
  theta_cov_start[1] <- 1e-15
  theta_cov_start <- diag(theta_cov_start)

  theta_cov_final <- rep_len(0.005, length.out = length(guess_params0))
  theta_cov_final[1] <- 1e-18
  theta_cov_final <- diag(theta_cov_final)

  theta_now <- guess_params0

  # Get initial likelihoods, samples
  sicr_res <- SICR_smc2_par(
    theta_now,
    t = tspan, Y = data, dataStrat,
    pIsol, timeIsol, matrixCtctAge, propAge, popSize, lethalityAge, severityAge,
    epsilon, sigma, prop_asympto,
    Np,
    out = "mcmc"
  )
  p_theta <- sicr_res[[1]]
  X_traj_sample <- sicr_res[[2]]

  p_samples <- rep(0, MCMC_params$iterations)  # p_sample holds the marginal likelihoods

  p_theta_now <- p_theta
  X_traj_now <- X_traj_sample

  p_samples[1] <- p_theta_now
  # Set up MCMC sampling
  log_m <- seq(MCMC_params$log_steps, MCMC_params$iterations, by = MCMC_params$log_steps)
  if (MCMC_params$iterations > MCMC_params$save_steps) {
    save_m <- seq(MCMC_params$save_steps, MCMC_params$iterations, by = MCMC_params$save_steps)
    if (!file.exists(sprintf("MCMC_%s.csv", pCountry))) {
      write(c("Iteration", "Likelihood", "transmRate", paste0("vecEff", seq_len(length(theta_now)-1))),
            ncolumns = length(theta_now) + 2,
            file = sprintf("MCMC_%s.csv", pCountry),
            sep = ",")
    }
  } else {
    save_m <- NULL
  }

  proposals <- matrix(NA_real_, nrow = length(log_m), ncol = length(guess_params0))
  theta_samples <- matrix(NA_real_, nrow = length(log_m), ncol = length(guess_params0))
  X_traj_samples <- matrix(NA_real_, nrow = length(log_m), ncol = length(guess_params0))

  accept <- 0
  i <- 1

  ### This is the MCMC part###
  for (m in 2:MCMC_params$iterations) {
    MCMC_params$m <- m

    # parameter proposals
    #theta_new <- runif(n=length(theta_now));
    #theta_new[2] <- rnorm(1,mean = theta_now[2], sd = theta_cov[2]);
    if (m < 0) {
      theta_cov <- theta_cov_start
    } else {
      theta_cov <- theta_cov_final
    }
    theta_new <- rmvnorm(n = 1, mean = theta_now, sigma = theta_cov)

    #while (any(theta_new < 0, theta_new > 1) && (theta_new[1]<(guess_params0[1]/3) && theta_new[1]>(guess_params0[1]*3))) {
    while (any(theta_new < 0, theta_new > 1)) {
      # if any parameter is negative OR if rho is greater than one, propose new theta
      theta_new <- rmvnorm(n = 1, mean = theta_now, sigma = theta_cov)
    }
    curr_proposal <- theta_new

    # Run SMC to get the marginal likelihood for theta_new
    sicr_res <- SICR_smc2_par(
      theta_new,
      t = tspan, Y = data, dataStrat,
      pIsol, timeIsol, matrixCtctAge, propAge, popSize, lethalityAge, severityAge,
      epsilon, sigma, prop_asympto,
      Np,
      out = "mcmc"
    )
    p_theta <- sicr_res[[1]]
    X_traj_sample <- sicr_res[[2]]
    p_theta_new <- p_theta
    X_traj_new <- X_traj_sample
    p_samples[m] <- p_theta_new  # these are the marginal likelihoods

    # Accept or reject theta proposal
    #a <- 10^((p_theta_new - p_theta_now)/100)
    a <- exp(p_theta_new - p_theta_now)
    z <- runif(1)
    if (isTRUE(z < a)) {
      accept <- accept + 1
      message("- MCMC_step = ", m, "; accept ", sprintf("(acceptance rate = %.2f(%.2f))", accept / m,a))
      X_traj_now <- X_traj_new
      theta_now <- theta_new
      p_theta_now <- p_theta_new
      message(
        "  Likelihood value: ", p_theta_now, "\n  ",
        sprintf(
          "transmRate = %.2e, ",
          theta_now[1]
        ),
        "vecEff = ", sprintf("%.2f ", theta_now[2:length(theta_now)])
      )
    } else {
      message("- MCMC_step = ", m, "; reject ", sprintf("(acceptance rate = %.2f (:)%.2f))", accept / m,a))
      message(
        "  Likelihood value: ", p_theta_new, "\n  ",
        sprintf(
          "transmRate = %.2e, ",
          theta_new[1]
        ),
        "vecEff = ", sprintf("%.2f ", theta_new[2:length(theta_new)])
      )
    }

    if (m %in% log_m) {
      theta_samples[i, ] <- theta_now
      proposals[i, ] <- curr_proposal
      X_traj_samples[i, ] <- X_traj_now
      i <- i + 1
    }
    if (m %in% save_m) {  # save samples
      write(c(m, p_theta_now, theta_now),
            file = sprintf("MCMC_%s.csv", pCountry),
            ncolumns = length(theta_now)+2,
            append = TRUE, sep = ",")
    }
  }

  MCMC_out <- list()
  MCMC_out$accept_rate <- accept / m  # acceptance rate
  MCMC_out$p_samples <- p_samples
  MCMC_out$proposals <- proposals
  MCMC_out$X_samples <- X_traj_samples

  list(theta_samples = theta_samples, MCMC_out = MCMC_out)
}
