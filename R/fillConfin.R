fillConfin <- function(pDataStrat, pVecEff) {

  # Confinement applied
  stratConfin <- unique(pDataStrat)

  pConfinementBegin <- rep_len(NA_real_, 8 * length(stratConfin))
  pConfinementDuration <- rep_len(NA_real_, 8 * length(stratConfin))
  pConfinementEfficiency <- rep_len(NA_real_, 8 * length(stratConfin))
  pConfinementTarget <- rep_len(NA_real_, 8 * length(stratConfin))

  for (i in seq_along(stratConfin)) {
    tBegin <- which(pDataStrat == stratConfin[i])[1]
    tend <- which(pDataStrat == stratConfin[i + 1])[1]
    if (is.na(tend)) {
      tend <- length(pDataStrat)
    }
    duration <- tend - tBegin
    pConfinementBegin[seq((i - 1) * 8 + 1, i * 8)] <- rep_len(tBegin, 8)
    pConfinementDuration[seq((i - 1) * 8 + 1, i * 8)] <- rep_len(duration, 8)
    pConfinementEfficiency[seq((i - 1) * 8 + 1, i * 8)] <- rep_len(pVecEff[i], 8)
    pConfinementTarget[seq((i - 1) * 8 + 1, i * 8)] <- 1:8
  }
  dfOut <- data.frame(
    pConfinementBegin = pConfinementBegin,
    pConfinementDuration = pConfinementDuration,
    pConfinementEfficiency = pConfinementEfficiency,
    pConfinementTarget = pConfinementTarget
  )
  return(dfOut)
}
