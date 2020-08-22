rm(list=ls())

devtools::load_all()

temp <- read.csv(system.file("extdata", "COVID_time_series_v4_2020-06-26.csv", package = "NpiEurope"),
                 stringsAsFactors = FALSE)
countryVec <- sort(unique(temp$Country))

i=24
country=countryVec[i]

country_data <- load_country_data(country)
contact_data <- load_contact_data(country)
age_data <- load_age_data(country)
country_data <- estimate_asympto(country_data, smooth = 7)

dataStrat <- get_strats(country_data, min_duration = 5)
transmRate <- 4.5e-9;
vecEff <- c(0.2,0.5,0.8,0.7,0.7,0.7,0.7,0.7);

sims <- LHS_init(country_data, contact_data, age_data, task = "run",
                Np = 10, Niter = 2000,transmRate,vecEff)
plot_simulations(sims, country_data)

res <- LHS_init(country_data, contact_data, age_data, task = "estimate",
                Np = 10, Niter = 2000,transmRate,vecEff)
#saveRDS(res, sprintf("fullmcmc%s_%s.rds", Sys.Date(), i, country))

