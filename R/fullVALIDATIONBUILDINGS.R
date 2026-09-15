#' create historical mif for buildings
#'
#' @param rev Unused parameter, but required by `madrat`.
#'
#' @author Robin Hasse
#'
#' @importFrom madrat calcOutput getConfig
fullVALIDATIONBUILDINGS <- function(rev = 0) {

  file <- "historicalBuildings.mif"



  # Set aggregated regions -----------------------------------------------------

  # get region mappings for aggregation ----
  # Determines all regions data should be aggregated to by examining the columns
  # of the `regionmapping` and `extramappings` currently configured.
  rel <- "global" # always compute global aggregate
  for (mapping in c(getConfig("regionmapping"), getConfig("extramappings"))) {
    columns <- setdiff(
      colnames(toolGetMapping(mapping, "regional", where = "mappingfolder")),
      c("X", "CountryCode")
    )

    if (any(columns %in% rel)) {
      warning(
        "The following column(s) from ", mapping,
        " exist in another mapping an will be ignored: ",
        paste(columns[columns %in% rel], collapse = ", ")
      )
    }
    rel <- unique(c(rel, columns))
  }

  columnsForAggregation <- gsub(
    "RegionCode", "region",
    paste(rel, collapse = "+")
  )



  # References -----------------------------------------------------------------


  ## IEA Energy balance ====

  calcOutput("FEBuildings", aggregate = columnsForAggregation,
             file = file, signif = 4,
             writeArgs = list(scenario = "historical", model = "IEA"))


  ## IEA EEI ====

  calcOutput("IEA_EEI", subtype = "buildings_reporting", aggregate = columnsForAggregation,
             file = file, signif = 4,
             writeArgs = list(scenario = "historical", model = "IEA EEI"),
             append = TRUE)


  ## Eurostat ====

  calcOutput("EurostatBuildings", aggregate = columnsForAggregation,
             file = file, signif = 4,
             writeArgs = list(scenario = "historical", model = "Eurostat"),
             append = TRUE)


  ## Odyssee ====

  calcOutput("Odyssee", aggregate = columnsForAggregation,
             file = file, signif = 4,
             writeArgs = list(scenario = "historical", model = "Odyssee"),
             append = TRUE)


  ## IDEES ====

  calcOutput("IDEESBuildings", aggregate = columnsForAggregation,
             file = file, signif = 4,
             writeArgs = list(scenario = "historical", model = "IDEES"),
             append = TRUE)
}
