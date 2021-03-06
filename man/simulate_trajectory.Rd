% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_trajectory.R
\name{simulate_trajectory}
\alias{simulate_trajectory}
\title{Run simulations using parameter estimation from the MCMC}
\usage{
simulate_trajectory(MCMC, country_data, contact_data, age_data, Npost = 10)
}
\arguments{
\item{MCMC}{the result of \code{run_MCMC()}}

\item{country_data}{Epidemiological dataset}

\item{contact_data}{Contact dataset}

\item{age_data}{Age dataset}

\item{Npost}{Integer. Number of values to draw from the posterior distribution
of the parameters}
}
\description{
Run simulations using parameter estimation from the MCMC
}
