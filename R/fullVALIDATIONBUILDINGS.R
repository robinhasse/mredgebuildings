fullVALIDATIONBUILDINGS <- function(rev = 0) {

  file <- "historicalBuildings.mif"



  # References -----------------------------------------------------------------

  ## IEA ====
  calcOutput("FEBuildings", file = file, round = 5,
             writeArgs = list(scenario = "historical", model = "IEA"))

  ## IEA EEI ====
  calcOutput("IEA_EEI", subtype = "buildings_reporting", file = file, round = 5,
             writeArgs = list(scenario = "historical", model = "IEA EEI"),
             append = TRUE)

  ## Eurostat ====
  calcOutput("EurostatBuildings", file = file, round = 5,
             writeArgs = list(scenario = "historical", model = "Eurostat"),
             append = TRUE)

  ## Odyssee ====
  calcOutput("Odyssee", file = file, round = 5,
             writeArgs = list(scenario = "historical", model = "Odyssee"),
             append = TRUE)

  ## IDEES ====
  calcOutput("IDEESBuildings", file = file, round = 5,
             writeArgs = list(scenario = "historical", model = "IDEES"),
             append = TRUE)
}
