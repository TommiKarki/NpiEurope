#' Get 95% credibility intervals for estimations
#'
#' @inheritParams plot_MCMC
#' @inheritParams plot_simulations
#'
#' @export
summarise_estimation <- function(MCMC, country_data, contact_data, age_data) {

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

  pMatrix <- matrix(NA_real_, ncol = 8, nrow = 8)
  for (i in 1:8) {
    for (j in 1:8) {
      pMatrix[i, j] <- sum(contact_data[c((i - 1) * 2 + 1, (i - 1) * 2 + 2), c((j - 1) * 2 + 1, (j - 1) * 2 + 2)])
    }
  }

  popAge <- rep(0, 8)
  for (i in 1:8) {
    popAge[i] <- sum(age_data[c((i - 1) * 2 + 1, (i - 1) * 2 + 2), c(2, 3)])
  }
  propAge <- popAge / sum(popAge)

  dataStrat <- get_strats(country_data, min_duration = 5)

  # Fixed parameters
  epsilon <- 1 / 3
  sigma <- 1 / 5
  df[, 1] <- rowSums(tcrossprod(df[, 1], popAge * rowSums(pMatrix))) / (epsilon + sigma)
  ci_params <- apply(df, 2, quantile, c(0.025, 0.975), na.rm  = TRUE)

  ci_params <- t(ci_params)

  rownames(ci_params) <- c("R0", unique(dataStrat)[-1])

  resConf <- fillConfin(dataStrat, rep_len(0.5, ncol(df) - 1))

  cat(
    "Credibility intervals:\n",
    sprintf("R0: [%.2f, %.2f]\n", 0, 0),
    sprintf("%s (%s-%s): [%.2f, %.2f]\n", rownames(ci_params)[-1], unique(resConf$pConfinementBegin)[-1], (resConf$pConfinementDuration + resConf$pConfinementBegin)[!duplicated(resConf$pConfinementBegin)][-1], ci_params[-1, 1], ci_params[-1, 2])
  )

  ci_params <- data.frame(ci_params,
                          "start" = c(NA, unique(resConf$pConfinementBegin)[-1]),
                          "end" = c(NA, (resConf$pConfinementDuration + resConf$pConfinementBegin)[!duplicated(resConf$pConfinementBegin)][-1]))

  return(ci_params)
}
