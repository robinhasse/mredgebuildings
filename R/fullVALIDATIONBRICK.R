fullVALIDATIONBRICK <- function(rev = 0) {
  file <- "historicalBuildings.mif"

  # References -----------------------------------------------------------------

  ## Eurostat ====
  calcOutput("EurostatBuildings", file = file, round = 5,
             writeArgs = list(scenario = "historical", model = "Eurostat"))
}
