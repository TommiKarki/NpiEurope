#' Run simulations using parameter estimation from the MCMC
#'
#' @inheritParams LHS_init
#' @inheritParams summarise_estimation
#' @param Npost Integer. Number of values to draw from the posterior distribution
#' of the parameters
#'
#' @export

simulate_trajectory <- function(MCMC, country_data, contact_data, age_data, Npost = 10) {

  if (is.character(MCMC)) {
    if (!file.exists(MCMC)) {
      stop(
        "You specified a file path but no file was found there. ",
        "Please try again.", call. = FALSE)
    }
    df <- as.matrix(read.csv(MCMC))
    lkl <- df[, 2]
    df <- df[, 3:ncol(df)]
  } else {
    df <- MCMC$MCMC_out$X_samples
    lkl <- MCMC$MCMC_out$p_samples
  }

  #par_index <- sample(seq_len(nrow(df)), size = Npost)
  par_index <- which.max(lkl)

  res <- lapply(par_index, function(x) {
    LHS_init(country_data, contact_data, age_data,
             task = "run",
             transmRate = df[x, 1],
             vecEff = df[x, 2:ncol(df)])
  })

  res <- do.call(rbind, res)

  return(res)
}
