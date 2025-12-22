#' calculate renovation cost
#'
#' floor-space specific renovation cost
#'
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

calcCostRenovationBS <- function(granularity = NULL) {

  # ASSUMPTIONS ----------------------------------------------------------------


  ## Vintage mark up ====

  # quadratically increasing factor to increase shell cost for old buildings
  vinFactorMax <- 1.5
  vinFactorStart <- 2020
  vinFactor <- toolGetMapping("dim_vin.csv",
                              type = "sectoral", where = "brick") %>%
    mutate(avgYear = (.data$from + .data$to) / 2,
           factor = ifelse(.data$avgYear < vinFactorStart,
                           (vinFactorStart - .data$avgYear) ^ 2,
                           0),
           factor = 1 + (vinFactorMax - 1) * .data$factor / max(.data$factor)) %>%
    select("vin", "factor")



  # CALCULATE ------------------------------------------------------------------

  ## relative demand ====
  relDem <- toolGetMapping("dim_bs.csv",
                           type = "sectoral", where = "brick") %>%
    select("bs", "relDem") %>%
    unique()

  # building shell states
  bs <- getElement(relDem, "bs")
  bsr <- c(bs, "0")  # 0: no change of building shell

  ## renovation transitions ====

  # all theoretical building shell transitions
  shellTransitions <- expand.grid(bs = bs, bsr = bsr)

  # calculate renovation depth
  shellTransitions <- shellTransitions %>%
    left_join(relDem, by = "bs") %>%
    left_join(relDem, by = c(bsr = "bs"), suffix = c(".initial", ".final")) %>%
    mutate(depth = .data$relDem.initial - .data$relDem.final,
           depth = replace_na(.data$depth, 0)) %>%
    select("bs", "bsr", "depth")

  # remove transitions to worse states
  shellTransitions <- shellTransitions %>%
    filter(.data$depth >= 0)

  ## calculate transition costs ====

  # transition cost as a function of depth
  shellCostModel <- calcOutput("RenovationCostModel", aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE) %>%
    select(-"model", -"scenario", -"unit") %>%
    pivot_wider(names_from = "variable", values_from = "value")

  # calculate cost
  shellCost <- cross_join(shellCostModel, shellTransitions) %>%
    mutate(shellCost = ifelse(
      .data$bsr == "0",
      0,
      .data$intercept + .data$slope * .data$depth
    )) %>%
    select(-"intercept", -"slope", -"depth")

  # add vintage dimension and assume mark up for older buildings
  shellCost <- cross_join(shellCost, vinFactor) %>%
    mutate(value = .data$shellCost * .data$factor) %>%
    select(-"factor", -"shellCost")


  # RETURN ---------------------------------------------------------------------

  # convert to magpie object
  shellCost <- shellCost %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value")

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    time_interpolate(getItems(shellCost, 2),
                     extrapolation_type = "constant")

  # aggregate to BRICK granularity
  agg <- toolAggregateBrick(shellCost, granularity, feBuildings)


  return(list(x = agg$x,
              unit = "USD2017/m2",
              weight = agg$weight,
              min = 0,
              description = "Floor-space specific cost of building shell retrofit"))
}
