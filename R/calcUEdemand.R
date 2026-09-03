#' Calculate useful energy demand for space heating in buildings
#'
#' @param granularity character, name of BRICK granularity
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource calcOutput toolGetMapping
#' @importFrom magclass mselect as.magpie collapseDim
#' @importFrom dplyr select right_join group_by across all_of summarise mutate
#'   cross_join
#' @importFrom quitte as.quitte
#' @export

calcUEdemand <- function(granularity = NULL) {

  # map Hotmaps vintages
  vinMap <- toolGetMapping("vintageMapping_Hotmaps.csv",
                           type = "sectoral", where = "mredgebuildings") %>%
    select("vin", "vinHotmaps")

  # map Hotmaps building types
  typMap <- toolGetMapping("buildingTypeMapping_Hotmaps.csv",
                           type = "sectoral", where = "mredgebuildings") %>%
    select("typ", "typHotmaps")

  # building shell map
  bsMap <- toolGetMapping("dim_bs.csv",
                          type = "sectoral", where = "brick")

  # Useful energy demand for space heating (kWh/yr/m2)
  vars <- c("ENERGY|Useful energy demand|Space heating" = "space_heating",
            "ENERGY|Useful energy demand|Domestic hot water" = "water_heating")
  ueDem <- readSource("Hotmaps") %>%
    mselect(variable = names(vars)) %>%
    toolCountryFillAvg(verbosity = 2) %>%
    as.quitte(na.rm = TRUE) %>%
    mutate(variable = unname(vars[.data$variable])) %>%
    right_join(typMap, by = c(building = "typHotmaps")) %>%
    right_join(vinMap, by = c(bage = "vinHotmaps"),
               relationship = "many-to-many") %>%
    select("region", "typ", "vin", enduse = "variable", "value") %>%
    group_by(across(-all_of(c("value")))) %>%
    summarise(value = mean(.data$value), .groups = "drop")

  # scale demand such that average relative demand is 1
  relDem <- bsMap %>%
    select("bs", "relDem", "initShare") %>%
    mutate(relDem = .data$relDem / sum(.data$initShare * .data$relDem)) %>%
    select(-"initShare")

  # add dimension: building shell
  ueDem <- ueDem %>%
    cross_join(relDem) %>%
    mutate(value = .data$value * case_match(.data$enduse,
                                            "space_heating" ~ .data$relDem,
                                            "water_heating" ~ 1)) %>%
    select(-"relDem")

  # convert to magpie object
  ueDem <- ueDem %>%
    as.magpie(spatial = "region", datacol = "value") %>%
    collapseDim()

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    mselect(period = "y2017", collapseNames = TRUE)

  # aggregate to BRICK granularity
  agg <- toolAggregateBrick(ueDem, granularity, feBuildings)



  return(list(x = agg$x,
              weight = agg$weight,
              unit = "kWh/yr/m2",
              min = 0,
              description = "Floor-space specific useful energy demand for space heating"))
}
