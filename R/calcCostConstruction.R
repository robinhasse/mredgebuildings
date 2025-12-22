#' calculate construction cost
#'
#' floor-space specific construction cost
#'
#' @param granularity character, name of BRICK granularity
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolGetMapping toolCountryFill calcOutput
#' @importFrom quitte inline.data.frame as.quitte
#' @importFrom magclass mbind getSets<- setNames as.magpie time_interpolate
#'   mselect dimSums
#' @importFrom dplyr %>% mutate group_by reframe select left_join ungroup filter
#'   select rename left_join inner_join
#' @export
#'
calcCostConstruction <- function(granularity = NULL) {

  # floor space ----------------------------------------------------------------

  # construction cost (only floor space) from Mastrucci et al. 2021
  floorCost <- inline.data.frame(
    "standard;  region;        value",
    "Standard;  Global North;  1614.59",
    "Advanced;  Global North;  1937.50",
    "Standard;  Global South;   454.78",
    "Advanced;  Global South;   591.21"
  )

  # map regions
  mapMessage <- toolGetMapping("regionmappingMessageIX.csv",
                               type = "regional", where = "mredgebuildings")
  floorCost <- floorCost %>%
    left_join(mapMessage %>% select(region = "X2regions", "CountryCode"),
              by = "region", relationship = "many-to-many") %>%
    select(region = "CountryCode", "standard", "value")

  # map efficiency standards
  floorCost <- floorCost %>%
    rename(bs = "standard", floorCost = "value") %>%
    revalue.levels(bs = c(Standard = "low", Advanced = "high")) %>%
    group_by(.data$region) %>%
    reframe(bs = c(.data$bs, "med"),
            floorCost = c(.data$floorCost, mean(.data$floorCost))) %>%
    ungroup()



  # heating system -------------------------------------------------------------

  # floor-space specific capacity in kW/m2
  heatingCapacity <- calcOutput("HeatingCapacity", swissFormular = TRUE,
                                aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE) %>%
    filter(.data$vin == "2021-2030") %>% # vins after 2010 are identical
    select("region", "typ", "bs", capacity = "value")

  # heating system installation cost: USD/kW -> USD/m2
  heatingCost <- calcOutput("HeatingSystem",
                            subtype = "Purchasing cost",
                            aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE) %>%
    left_join(heatingCapacity, by = c("region", "typ"), relationship = "many-to-many") %>%
    mutate(heatingCost = .data$value * .data$capacity) %>%
    select("region", "period", "bs", "hs", "typ", "heatingCost")



  # total construction ---------------------------------------------------------

  constructionCost <- inner_join(floorCost, heatingCost, by = c("region", "bs"),
                                 relationship = "many-to-many") %>%
    mutate(value = .data$floorCost + .data$heatingCost) %>%
    select("region", "period", "bs", "hs", "typ", "value")

  # convert to magpie object
  constructionCost <- constructionCost %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value") %>%
    toolCountryFill(NA, verbosity = 2)

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    time_interpolate(getItems(constructionCost, 2),
                     extrapolation_type = "constant")

  # aggregate to BRICK granularity
  agg <- toolAggregateBrick(constructionCost, granularity, feBuildings)



  return(list(x = agg$x,
              unit = "USD2017/m2",
              weight = agg$weight,
              min = 0,
              description = "Floor-space specific cost of new costruction"))
}
