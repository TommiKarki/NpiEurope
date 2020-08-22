#' Plot the MCMC trace
#'
#' @param MCMC the result of `run_MCMC()`
#'
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#'
#' @export
plot_MCMC <- function(MCMC) {

  if (is.character(MCMC)) {
    if (!file.exists(MCMC)) {
      stop(
        "You specified a file path but no file was found there. ",
        "Please try again.", call. = FALSE)
    }
    df <- read.csv(MCMC)
    colnames(df)[1] <- "step"
  } else {
    df <- as.data.frame(cbind(MCMC$MCMC_out$X_samples, MCMC$MCMC_out$p_samples))

    colnames(df) <- c("transmRate", paste0("vecEff", seq_len(ncol(df)-2)), "logLik")

    df$step <- seq_len(nrow(df))
  }

  df %>%
    pivot_longer(-step) %>%
    ggplot(aes(x = step, y = value)) +
    geom_line() +
    facet_wrap(~ name, scales = "free")
}
