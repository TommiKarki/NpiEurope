#' Load contact dataset from specific country
#'
#' @inheritParams load_country_data
#'
#' @importFrom utils read.csv
#'
#' @export

load_age_data <- function(country) {

  age_data <- read.csv(
    system.file("extdata", paste0("age_", country, ".csv"), package = "NpiEurope")
  )

  return(age_data)
}
