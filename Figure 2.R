rm(list = ls())
library(patchwork)
library(dplyr)
devtools::load_all()

temp <- read.csv(system.file("extdata", "COVID_time_series_v4_2020-06-26.csv", package = "NpiEurope"),
                 stringsAsFactors = FALSE)
countryVec <- sort(unique(temp$Country))

temp <- temp %>%
  group_by(Country) %>%
  mutate(cum_cases = cumsum(NewCases),
         simulated = NA_real_)

pGraph <- list()
for (i in 1:length(countryVec)) {
  country <- countryVec[i]
  print(country)
  country_data <- load_country_data(country)
  country_data$NewCases[which(country_data$NewCases < 0)] <- -country_data$NewCases[which(country_data$NewCases < 0)]
  country_data <- estimate_asympto(country_data)
  contact_data <- load_contact_data(country)
  age_data <- load_age_data(country)

  folder <- "MCMCOK"
  plot_asympto(country_data)
  ggsave(sprintf("%s/Report_%s.pdf", folder, country))

  res <- read.csv(sprintf("%s/MCMC_%s.csv", folder, country))

  plot_MCMC(sprintf("%s/MCMC_%s.csv", folder, country))
  ggsave(sprintf("%s/mcmc_%s.pdf", folder, country))

  write.csv(summarise_estimation(sprintf("%s/MCMC_%s.csv",folder,country), country_data,contact_data,age_data),sprintf("%s/estim_%s.csv",folder,country))
  # knitr::kable(summarise_estimation(sprintf("MCMCNew/MCMC_%s.csv",country), country_data))

  sims <- simulate_trajectory(sprintf("%s/MCMC_%s.csv", folder, country),
    country_data, contact_data, age_data,
    Npost = 5
  )

  temp$simulated[temp$Country==country & !is.na(temp$cum_cases) & temp$cum_cases > 0] <- apply(sims, MARGIN = 2, median)

  tempG <- plot_simulations(sims, country_data) + ggtitle(country)
  pGraph[[length(pGraph) + 1]] <- tempG
  ggsave(sprintf("%s/sim_%s.pdf", folder, country))
}

p <- ggplot(temp, aes(x = NewCases, y = simulated, color = Country)) +
  geom_point() +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  geom_abline(slope = 1, col = "black", linetype = "dashed") +
  xlab("Observed cases") + ylab("Simulated cases") +
  theme_minimal()

pGraph[[length(pGraph) + 1]] <- p

layout <- mapply(area, t = c(rep(1:4, each = 6), rep(5:6, each = 4)), l = c(rep(1:6, times = 4), rep(1:4, times = 2)), SIMPLIFY = FALSE)
layout <- do.call(patchwork:::c.patch_area, layout)

layout <- c(layout, area(5,5,6,6))

wrap_plots(pGraph, design = layout)
ggsave(sprintf("figure2.pdf", folder, country))
