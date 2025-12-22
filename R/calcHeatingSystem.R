#' Calculate installation cost or efficiency of heating systems
#'
#' The data is taken from the EU reference scenario technology assumptions. We
#' map our technologies to the higher resolved PRIMES technologies. For missing
#' commercial technologies, we assume residential values but decrease purchasing
#' cost by 10%.
#'
#' @param subtype character, variable type (either 'Purchasing cost' or
#'   'Efficiency')
#' @param granularity character, name of BRICK granularity
#' @returns MagPIE object with capacity-specific purchasing cost or efficiency
#'   of heating systems
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolGetMapping readSource toolCountryFill
#' @importFrom magclass as.magpie getSets<- setNames mbind dimSums
#'   time_interpolate
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte inline.data.frame
#' @importFrom dplyr %>% right_join .data filter group_by across all_of
#'   summarise select
#' @importFrom tidyr pivot_wider pivot_longer
#' @export

calcHeatingSystem <- function(subtype = c("Purchasing cost", "Efficiency"),
                              granularity = NULL) {

  # FUNCTIONS ------------------------------------------------------------------

  completeServices <- function(x) {
    x %>%
      pivot_wider(names_from = "subsector") %>%
      mutate(Services = ifelse(
        is.na(.data$Services),
        .data$Residential * ifelse(.data$variable == "Purchasing cost", 0.9, 1),
        .data$Services
      )) %>%
      pivot_longer(c("Residential", "Services"), names_to = "subsector") %>%
      filter(!is.na(.data$value))
  }

  # COP data of heat pumps in EU_ReferenceScenario is low compared to other sources:
  # See e.g. Masiukiewicz et al. (10.1016/j.energy.2025.136001), Bogdanov et al. (10.1016/j.apenergy.2024.123647).
  # Detailed hourly data differentiated by heat pump type is provided by
  # when2Heat data source (Ruhnau, 10.25832/WHEN2HEAT/2019-08-06).
  # This could be used/referenced for a future more sophisticated solution.
  # Here we increase efficiency of air-sourced heat pumps by 1 and of other heat pumps by 0.6
  adjustHpEfficiency <- function(x, subtype) {
    if (subtype == "Efficiency") {
      x %>%
        mutate(value = case_when(
          .data$technology == "Heat pump air" ~ .data$value + 1,
          .data$technology == "Heat pump water" ~ .data$value + 0.6,
          .data$technology == "Heat pump ground" ~ .data$value + 0.6,
          .default = .data$value
        ))
    } else {
      x
    }
  }



  # CALCULATE ------------------------------------------------------------------

  subtype <- match.arg(subtype)

  # all heating technologies
  hsMap <- toolGetMapping("dim_hs.csv",
                          type = "sectoral", where = "brick")

  # map heating technologies
  euRefMap <- toolGetMapping("technologyMapping_EU_ReferenceScenario.csv",
                             type = "sectoral", where = "mredgebuildings",
                             returnPathOnly = TRUE) %>%
    read.csv(comment.char = "") %>%
    select(-"comment") %>%
    pivot_longer(matches("^weight"), names_to = "typ", values_to = "weight") %>%
    mutate(typ = sub("^weight(.*)$", "\\1", .data$typ)) %>%
    right_join(unique(hsMap["hs"]), by = c(technologyBRICK = "hs"))

  if (any(is.na(euRefMap))) {
    stop("Incomplete mapping of heating technologies.")
  }

  # map building types to subsectors
  typMap <- inline.data.frame(
    "typ; subsector;   FLOW",
    "SFH; Residential; RESIDENT",
    "MFH; Residential; RESIDENT",
    "Com; Services;    COMMPUB"
  )

  # map periods: assume exogenous learning
  periodMap <- inline.data.frame(
    "period; pointintime",
    "2020;   Current",
    "2030;   2030",
    "2050;   Ultimate"
  )

  data <- readSource("EU_ReferenceScenario", "techAssump.Domestic") %>%
    as.quitte(na.rm = TRUE) %>%
    select(-"period") %>%
    completeServices() %>%
    filter(.data$variable == subtype,
           .data$level %in% c("Current", "central")) %>%
    adjustHpEfficiency(subtype) %>%
    left_join(typMap, by = "subsector", relationship = "many-to-many") %>%
    left_join(periodMap, by = "pointintime") %>%
    left_join(euRefMap, by = c("typ", "technology"), relationship = "many-to-many") %>%
    select("region", "period", hs = "technologyBRICK", "typ", "unit", "value",
           "weight") %>%
    filter(!is.na(.data$hs)) %>%
    group_by(across(-all_of(c("value", "weight")))) %>%
    summarise(value = sum(proportions(.data$weight) * .data$value),
              .groups = "drop")

  # TODO: find data for biom, libo, reel and sobo in Com # nolint: todo_comment_linter

  switch(subtype,
    `Purchasing cost` = {
      # until here, H2 boilers are just condensing gas boilers. We add
      # 2500€/unit for the conversion to 100% H2 readiness (cf. ISE
      # Heizkostenvergleich) and assume 10kW/unit capacity
      # (cf. BDEW Heizkostenvergleich). The mark up reduces linearly to half its
      # value until 2050. The value is deflated (1.39) to be consistent.
      data <- data.frame(period = c(2020, 2050),
                         hs = "h2bo",
                         h2MarkUp = c(250, 125) / 1.39) %>%
        interpolate_missing_periods(unique(data[["period"]]),
                                    value = "h2MarkUp",
                                    expand.values = TRUE) %>%
        right_join(data, by = c("period", "hs")) %>%
        mutate(value = .data$value + replace_na(.data$h2MarkUp, 0)) %>%
        select(-"h2MarkUp")

      # As a first approach to heat pump subsidy, we assume a general subsidy on
      # heat pumps amounting to 30% of total purchasing costs (does not include installation costs)
      data <- data %>%
        mutate(value = .data$value * ifelse(.data$hs == "ehp1", 0.7, 1))

      # unit conversion EUR/kW -> USD/kW
      eur2usd <- GDPuc::toolConvertSingle(1, "DEU", 2015,
                                          unit_in = "current LCU",
                                          unit_out = "constant 2017 US$MER")
      data <- data %>%
        mutate(value = .data$value * eur2usd)
      unit <- "USD2017/kW"
    },
    `Efficiency` = {
      unit <- "1"
    }
  )
  data[["unit"]] <- NULL

  # convert to magpie object
  data <- data %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value") %>%
    toolCountryFill(NA, verbosity = 2)

  # fill missing non-European countries with average
  data <- toolCountryFillAvg(data, verbosity = 2)



  # RETURN ---------------------------------------------------------------------

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    time_interpolate(getItems(data, 2), extrapolation_type = "constant")

  # aggregate to BRICK granularity
  agg <- toolAggregateBrick(data, granularity, feBuildings)



  return(list(x = agg$x,
              weight = agg$weight,
              unit = unit,
              min = 0,
              description = subtype))
}
