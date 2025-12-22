#' calculate renovation cost
#'
#' floor-space specific renovation cost
#'
#' @param energyLadder logical, should the calculation include renovation
#'   transitions that are considered a decline on the energy ladder?
#' @param granularity character, name of BRICK granularity
#' @returns MagPIE object with floor-space specific renovation cost depending on
#'   the initial and final state of the building
#'
#' @author Robin Hasse
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom magclass as.magpie
#' @importFrom dplyr %>% .data mutate select left_join filter select cross_join
#'   group_by ungroup
#' @importFrom tidyr pivot_wider
#' @export

calcCostRenovationHS <- function(energyLadder = FALSE, granularity = NULL) {

  # ASSUMPTIONS ----------------------------------------------------------------

  ## Heating mark down ====

  # decrease installation cost if there is no technology switch
  sameHsFactor <- 0.2



  # CALCULATE ------------------------------------------------------------------

  ## renovation transitions ====

  # heating systems hierarchy
  heatingLadder <- toolGetMapping("dim_hs.csv",
                                  type = "sectoral", where = "brick") %>%
    select("hs", ladder = "energyLadder")

  # heating system states
  .hs <- heatingLadder %>%
    getElement("hs") %>%
    unique()
  .hsr <- c(.hs, "0")
  heatingTransitions <- expand.grid(hs = .hs, hsr = .hsr,
                                    stringsAsFactors = FALSE)

  # do not allow to go down the energy ladder
  if (energyLadder) {
    heatingTransitions <- heatingTransitions %>%
      left_join(heatingLadder, by = "hs") %>%
      left_join(heatingLadder, by = c(hsr = "hs"),
                suffix = c(".initial", ".final")) %>%
      mutate(ladder.final = ifelse(.data$hsr == "0",
                                   .data$ladder.initial,
                                   .data$ladder.final)) %>%
      filter(.data$ladder.initial >= .data$ladder.final) %>%
      select("hs", "hsr")
  }


  ## calculate transition cost ====

  # preliminary floor-space specific capacity in kW/m2
  heatingCapacity <- calcOutput("HeatingCapacity", swissFormular = TRUE,
                                aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE) %>%
    select("region", "typ", "vin", "bs", capacity = "value")

  # heating system purchasing cost: USD/kW -> USD/m2
  heatingCost <- calcOutput("HeatingSystem",
                            subtype = "Purchasing cost",
                            aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE) %>%
    left_join(heatingCapacity, by = c("region", "typ"),
              relationship = "many-to-many") %>%
    mutate(heatingCost = .data$value * .data$capacity) %>%
    select("region", "period", "bs", "hs", "typ", "vin", "heatingCost")

  # add dimension: initial heating system,
  heatingCost <- heatingTransitions %>%
    mutate(hs.final = ifelse(.data$hsr == "0",
                             .data$hs,
                             .data$hsr)) %>%
    left_join(heatingCost, by = c(hs.final = "hs"),
              relationship = "many-to-many") %>%
    mutate(heatingCost = ifelse(.data$hsr == "0",
                                0,
                                .data$heatingCost)) %>%
    select(-"hs.final")

  # heating system installation cost (independent of capacity)
  # rough numbers from BDEW Heizkostenvergleich cost/floor
  installationCost <- inline.data.frame(
    "typ; installationCost",
    "SFH; 30",
    "MFH; 10",
    "Com; 6"
  )
  heatingCost <- heatingCost %>%
    left_join(installationCost, by = "typ") %>%
    mutate(installationCost = .data$installationCost *
             ifelse(.data$hsr == "0",
                    0,
                    ifelse(.data$hsr == "reel",
                           0.1,
                           ifelse(.data$hs == .data$hsr,
                                  sameHsFactor,
                                  1))),
           heatingCost = .data$heatingCost + .data$installationCost) %>%
    select(-"installationCost")

  # mark up for installation of central heating system (piping, etc.)
  # https://www.checkatrade.com/blog/cost-guides/central-heating-installation-cost/
  heatingCost <- heatingCost %>%
    mutate(heatingCost = .data$heatingCost +
             ifelse(.data$hs == "reel" & !.data$hsr %in% c("0", "reel"),
                    3000 / 150 * 1.21, # units: GBP / m2 * USD/GBP # nolint: commented_code_linter.
                    0))



  # RETURN ---------------------------------------------------------------------

  # convert to magpie object
  heatingCost <- heatingCost %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "heatingCost")

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    time_interpolate(getItems(heatingCost, 2),
                     extrapolation_type = "constant")

  # aggregate to BRICK granularity
  agg <- toolAggregateBrick(heatingCost, granularity, feBuildings)


  return(list(x = agg$x,
              unit = "USD2017/m2",
              weight = agg$weight,
              min = 0,
              description = "Floor-space specific cost of heating system replacement"))
}
