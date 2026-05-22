
#' Read dena Gebäudereport
#'
#' @param subtype name of variable
#' @returns MagPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom utils read.csv
#' @importFrom dplyr .data %>% rename mutate filter
#' @importFrom tidyr pivot_longer
#' @importFrom magclass as.magpie
readDenaGebaeudereport <- function(subtype) {
  report <- 2026
  if (subtype == "salesHeatingSystems") {
    file <- "data-22rWD.csv"
    data <- read.csv(file.path(report, file), encoding = "UTF-8") %>%
      pivot_longer(-1) %>%
      rename(tech = 1, period = 2) %>%
      mutate(build = case_when(grepl("^X\\d{4}$",   .data$period) ~ "renovation",
                               grepl("^X\\d{4}\\.", .data$period) ~ "new"),
             .after = 1) %>%
      filter(!is.na(.data$build)) %>%
      mutate(region = "DEU",
             period = as.numeric(sub("^X(\\d{4}).*$", "\\1", .data$period)),
             .before = 1)
  }
  as.magpie(data)
}
