#' @importFrom stats dnbinom
SICR_smc2_par <- function(guess_params,
                          t, Y, dataStrat,
                          pIsol, timeIsol, matrixCtctAge, propAge, popSize, lethalityAge, severityAge,
                          epsilon, sigma, prop_asympto,
                          Np,
                          out = c("lkl", "cases", "mcmc")) {

  N <- length(Y) # length of data
  if (length(t) != N + 1) {
    stop("mismatch between time and data")
  }

  transmRate <- guess_params[1]
  vecEff <- c(0, guess_params[2:length(guess_params)])

  resConf <- fillConfin(dataStrat, vecEff)
  confinementBegin <- resConf$pConfinementBegin
  confinementDuration <- resConf$pConfinementDuration
  confinementEfficiency <- resConf$pConfinementEfficiency
  confinementTarget <- resConf$pConfinementTarget

  ell <- rep_len(NA_real_, N) # conditional log likelihoods

  n <- length(propAge)

  P0 <- data.frame(
    S = rep_len(1, n) * popSize * propAge,
    E = rep_len(0, n),
    A = rep_len(1, n),
    I = rep_len(1, n),
    R = rep_len(0, n),
    U = rep_len(0, n),
    D = rep_len(0, n),
    M = rep_len(0, n)
  )

  particles <- rep_len(list(P0), Np)
  cases <- matrix(NA_real_, ncol = N, nrow = Np)
  weights <- rep(1/Np,Np)
  for (k in seq_len(N)) {
    ########## NEW VERSION ##########
    # temp=0;
    # #indexSampled=sample(x=Np,size=Np,replace=TRUE,prob=weights);
    # #particles=particles[indexSampled];
    # for(ip in 1:Np){
    #   P=SICR_simulateState(
    #     t[k], t[k + 1],
    #     particles[[ip]],
    #     pIsol, timeIsol, matrixCtctAge, propAge, transmRate, popSize, lethalityAge, severityAge, confinementBegin, confinementDuration, confinementEfficiency, confinementTarget, epsilon, sigma, prop_asympto,
    #     Np
    #   )
    #   #weights[ip]=abs(Y[k]-sum(P$NbCases))
    #   #temp=temp+sum(P$NbCases);
    #   #weights[ip]=dnorm(Y[k], sum(P$NbCases))
    #   weights[ip]=dnbinom(Y[k],sum(P$NbCases),0.5);
    #   #weights[ip]=dnbinom(Y[k], sum(P$NbCases), 0.5)
    #   particles[[ip]]=P;
    #   #print(P$NbCases)
    # }
    # ell[k] <- -log(mean(weights)+1)
    # #ell[k] <- abs(Y[k]-(temp/Np))
    # cases[, k] <- vapply(particles, function(p) sum(p$NbCases), numeric(1))
    # # ########## END NEW VERSION ##########
    ########## OLD VERSION ##########
    # advance particles according to state process model
    particles <- lapply(particles, function(P) {
      SICR_simulateState(
        t[k], t[k + 1],
        P,
        pIsol, timeIsol, matrixCtctAge, propAge, transmRate, popSize, lethalityAge, severityAge, confinementBegin, confinementDuration, confinementEfficiency, confinementTarget, epsilon, sigma, prop_asympto[k],
        Np
      )
    })

    # compute weigths according to measurement model
    weights <- vapply(particles, function(p) {
      dnbinom(Y[k], sum(p$NbCases), 0.5)
      #dnorm(Y[k], sum(p$NbCases))
      #abs(Y[k]-sum(p$NbCases))
    }, numeric(1))
    # particles
    # k
    # compute conditional log likelihood
    ell[k] <- log(mean(weights))
    if (is.infinite(ell[k])) {
       ell[k] <- -1e6
    }
    #resample particles to update filtering distribution
    cases[, k] <- vapply(particles, function(p) sum(p$NbCases), numeric(1))

    ########## END OLD VERSION ##########
  }
  #ell[which(is.infinite(ell))]=mean(ell[which(!is.infinite(ell))]);
logL <- sum(ell)
  if (out == "lkl") {
    cat("Likelihood value: ", logL, "\n Param :", guess_params, "\n")
    return(logL)
  } else if (out == "cases") {
    return(cases)
  } else if (out == "mcmc") {
    return(list(logL, guess_params))
  }

}
