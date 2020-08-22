#' @importFrom stats rpois
SICR_simulateState <- function(t0, t1,
                               P,
                               pIsol, timeIsol, matrixCtctAge, propAge, transmRate, popSize, lethalityAge, severityAge, confinementBegin, confinementDuration, confinementEfficiency, confinementTarget, epsilon, sigma, propAsympto,
                               Np) {
  dt <- 1 / 2
  n <- length(propAge)
  nbCases <- rep_len(0L, n)

  while (t0 + dt <= t1) {

    # Main body of equations
    P[P < 0] <- 0

    # Calculating force of direct transmission
    lambda <- crossprod(matrixCtctAge, P$I + P$A) * transmRate * P$S
    if (t0 >= timeIsol) {
      # lambda <- lambda * (1-pIsol)
    }
    if (any(t0 >= confinementBegin & t0 < confinementBegin + confinementDuration)) {
      lambda <- lambda * (1 - confinementEfficiency[t0 >= confinementBegin & t0 < (confinementBegin + confinementDuration)])
    }

    # Rates calculation
    # FIXME: it would be faster to not create this object and then subset it
    # in each rpois() call but rather use the rate directly in the rpois() call
    rates <- cbind(
      lambda, # S-1,E+1
      propAsympto * epsilon * P$E, # E-1,A+1
      (1 - propAsympto) * epsilon * P$E, # E-1,I+1
      sigma * P$A, # A-1,U+1
      sigma * (1 - lethalityAge - severityAge) * P$I, # I-1,R+1
      sigma * lethalityAge * P$I, # I-1,D+1
      sigma * severityAge * P$I # I-1,M+1
    )

    # Rates application
    Num <- rpois(n, rates[, 1] * dt)
    P$S <- P$S - Num
    P$E <- P$E + Num
    Num <- rpois(n, rates[, 2] * dt)
    P$E <- P$E - Num
    P$A <- P$A + Num
    Num <- rpois(n, rates[, 3] * dt)
    P$E <- P$E - Num
    P$I <- P$I + Num
    nbCases <- nbCases + Num
    Num <- rpois(n, rates[, 4] * dt)
    P$A <- P$A - Num
    P$U <- P$U + Num
    Num <- rpois(n, rates[, 5] * dt)
    P$I <- P$I - Num
    P$R <- P$R + Num
    Num <- rpois(n, rates[, 6] * dt)
    P$I <- P$I - Num
    P$D <- P$D + Num
    Num <- rpois(n, rates[, 7] * dt)
    P$I <- P$I - Num
    P$M <- P$M + Num

    t0 <- t0 + dt
    P[P < 0] <- 0
  }
  P$NbCases <- nbCases
  return(P)
}
