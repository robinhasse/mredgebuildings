#' Demolition cost
#'
#' @author Robin Hasse
#'
#' @importFrom magclass as.magpie mselect
#' @importFrom madrat calcOutput
#' @export

calcCostDemolition <- function() {

  x <- as.magpie(15) %>%
    toolCountryFillAvg(verbosity = 2, no_remove_warning = "GLO")

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    mselect(period = "y2020") %>%
    dimSums(c(2, 3))

  return(list(x = x,
              weight = feBuildings,
              unit = "USD2017/m2",
              min = 0,
              description = "Floor-space specific demolition cost"))
}
