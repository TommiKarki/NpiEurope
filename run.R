rm(list=ls())

devtools::load_all("~/NpiEurope")

folder <- paste0("/scratch/gruson-", Sys.getenv("$SLURM_JOB_ID", Sys.Date()))
dir.create(folder)
setwd(folder)

library(foreach)

cl <- parallel::makeForkCluster(24)
doParallel::registerDoParallel(cl)

temp <- read.csv(system.file("extdata", "COVID_time_series_v4_2020-06-26.csv", package = "NpiEurope"),
                 stringsAsFactors = FALSE)
countryVec <- unique(temp$Country)

foreach(country=countryVec) %dopar% {

  country_data <- load_country_data(country)
  contact_data <- load_contact_data(country)
  age_data <- load_age_data(country)
  country_data <- estimate_asympto(country_data, smooth = 7)

  res <- LHS_init(country_data, contact_data, age_data, task = "estimate",
                  Np = 50, Niter = 5000)
  saveRDS(res, sprintf("fullmcmc_%s.rds", country))

}

parallel::stopCluster(cl)
