#' Read data from the Danish Energy Agency
#'
#' @param subtype character, report
#' @returns MagPIE object with data for DNK
#'
#' @author Robin Hasse
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr %>% .data mutate case_when select
#' @importFrom magclass as.magpie

readDEA_TechnologyCatalogues <- function(subtype) { # nolint: object_name_linter.
  if (subtype == "individualHeatingPlants") {
    file <- "technology_data_heating_installations - 07.xlsx"
    sheet <- "alldata_flat"
    path <- file.path(subtype, "v7", file)
    data <- read_xlsx(path, sheet = sheet)


    data <- data %>%
      mutate(region = "DNK",
             tech = sub("^(.*) - (.*) - (.*)$", "\\1", .data$Technology),
             buildingVin = case_when(grepl("ex ",  .data$ws) ~ "existing",
                                     grepl("new ", .data$ws) ~ "new",
                                     grepl("new building", .data$Technology) ~ "new"),
             buildingType = case_when(grepl("apart",  .data$ws) ~ "MFH",
                                      grepl("single", .data$ws) ~ "SFH"),
             variable = sub("^(.*) \\[(.*)\\]$", "\\1", .data$par)) %>%
      select("region", period = "year", "tech", "buildingType", "buildingVin",
             "variable", "unit", estimate = "est", value = "val") %>%
      as.magpie(spatial = 1, temporal = 2)
  }
  return(data)
}
