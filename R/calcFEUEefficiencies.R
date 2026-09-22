#' Calculation and Projection of Final to Useful Energy
#'
#' Calculate Efficiencies of Final (FE) to Useful (UE) Energy Conversion for all
#' combinations of Energy Carriers and Enduses. Missing data points within the
#' time interval of the original data (PFUDB) are filled with estimates from a
#' non-linear regression w.r.t. GDP per capita.
#' The efficiency projections are based on a model by De Stercke et al. which is
#' mainly driven by GDP per Capita. It describes an S-shaped curve approaching
#' assumed efficiency levels. The parameters of that curve are derived by a
#' regression with observations of IEA data.
#'
#' @param gasBioEquality Determines if natural gas and modern biomass share the same efficiencies
#'
#' @references De Stercke, S. (2014). Dynamics of Energy Systems: A Useful
#' Perspective (Interim Report, p. 68). IIASA.
#' http://pure.iiasa.ac.at/id/eprint/11254/1/IR-14-013.pdf
#'
#' @author Hagen Tockhorn
#'
#' @importFrom stats SSasymp na.omit
#' @importFrom dplyr reframe mutate select left_join rename group_by across
#'   all_of ungroup filter semi_join case_when group_modify
#' @importFrom tidyr spread unite replace_na pivot_wider
#' @importFrom madrat calcOutput
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom magclass as.magpie
#'
#' @export


calcFEUEefficiencies <- function(gasBioEquality = TRUE) {


  # READ-IN DATA ---------------------------------------------------------------

  # Final and useful energy data
  pfu <- calcOutput("PFUDB", aggregate = FALSE) %>%
    as.quitte()

  #GDP per capita
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       aggregate = FALSE) %>%
    as.quitte()

  # Efficiency regression parameters
  regPars <- calcOutput("EfficiencyRegression",
                        gasBioEquality = gasBioEquality,
                        aggregate = FALSE) %>%
    as.quitte()

  # Final energy weights
  feWeights <- calcOutput("FEbyEUEC", aggregate = FALSE) %>%
    as.quitte()


  # Corrected regression parameters
  regParsCorrections <- toolGetMapping("correctEfficiencies.csv",
                                       type = "sectoral",
                                       where = "mredgebuildings")


  # Equal efficiency assumptions
  equalEfficiencies <- toolGetMapping("equalEfficiencies.csv",
                                      type = "sectoral",
                                      where = "mredgebuildings")


  # Cooling COP boundaries
  coolingPars <- toolGetMapping("coolingEfficiencyParameters.csv",
                                type = "sectoral",
                                where = "mredgebuildings")



  # PROCESS DATA ---------------------------------------------------------------

  coolingPars <- setNames(coolingPars$value, coolingPars$variable)

  # Weight given to log(gdppop) for space_cooling.elec input variable
  #   -> x = log(gdppop) * weight + period * (1 - weight)
  gdppopWeight <- coolingPars[["gdppopWeight"]]

  # Upper temporal boundary of data set
  maxPeriod <- max(pfu$period)

  # Combine necessary data for regression
  fullData <- pfu %>%
    # Replace vanishing demands with NA's to facilitate correction factor calculation
    mutate(value = ifelse(.data[["value"]] == 0, NA, .data[["value"]])) %>%

    # Combine with GDP per Cap for Period 1990-2020
    interpolate_missing_periods(period = seq(1990, maxPeriod)) %>%
    left_join(gdppop %>%
                select(-"model", -"scenario", -"unit", -"variable") %>%
                rename(gdppop = "value"),
              by = c("region", "period")) %>%
    select("region", "period", "unit", "carrier", "enduse", "gdppop", "value")



  ## Calculate Efficiency Estimates ====

  # Regression Parameters for enduse.carrier Combinations
  regPars <- regPars %>%
    select("variable", "carrier", "enduse", "value") %>%
    pivot_wider(names_from = "variable", values_from = "value")


  # Historical Efficiencies
  histEfficiencies <- fullData %>%
    # Calculate historical efficiencies
    pivot_wider(names_from = "unit", values_from = "value") %>%
    mutate(efficiency = .data[["ue"]] / .data[["fe"]]) %>%

    # Calculate missing efficiencies from regression parameters
    left_join(regPars, by = c("enduse", "carrier")) %>%
    group_by(across(all_of(c("carrier", "enduse")))) %>%

    # Distinguish between logistic and asymptotic model
    mutate(x = log(.data$gdppop) * gdppopWeight + .data$period * (1 - gdppopWeight),
           pred = ifelse((.data$carrier == "elec" & .data$enduse == "space_cooling"),
                         coolingPars[["min"]] + (coolingPars[["max"]] - coolingPars[["min"]]) /
                           (1 + exp(-.data$k * (.data$x + .data$x0))),
                         SSasymp(.data$gdppop, .data$Asym, .data$R0, .data$lrc))) %>%

    ungroup() %>%
    select("region", "period", "enduse", "carrier", "gdppop", "efficiency", "pred")


  ## Match predictions with existing historical data points ====

  # Correction factor to adjust projections
  correctionFactors <- histEfficiencies %>%
    mutate(factor = .data[["efficiency"]] / .data[["pred"]]) %>%
    select(-"gdppop", -"efficiency", -"pred") %>%
    filter((.data$enduse != "space_cooling" & .data$carrier != "elec")) %>%
    group_by(across(all_of(c("region", "enduse", "carrier")))) %>%

    # Linearly extrapolate factors for all periods
    group_modify(~ extrapolateMissingPeriods(.x, key = "factor", slopeOfLast = 5)) %>%
    ungroup() %>%

    # fill negative factors with very small positive values
    mutate(factor = ifelse(.data[["factor"]] < 0, 1e-2, .data[["factor"]]))


  # NOTE: Since we correct some regression parameters, we need to adopt the projections
  #       for the respective enduse.carrier combination for all regions to not induce
  #       inconsistencies due to existing or non-existing data points.
  euecToOverwrite <- c(unique(regParsCorrections$variable), "space_cooling.elec")


  # NOTE: Countries missing the entire period range for a EC-EU-combination
  #       will be filled-up w/ non-corrected efficiency projections.
  histEfficiencies <- histEfficiencies %>%
    left_join(correctionFactors, by = c("region", "period", "enduse", "carrier")) %>%
    unite(col = "variable", c("enduse", "carrier"), sep = ".") %>%
    mutate(value = case_when(is.na(.data[["factor"]]) ~ .data[["pred"]], # no historical data
                             is.na(.data[["efficiency"]]) ~ .data[["pred"]] * .data[["factor"]], # missing data points
                             .default = .data[["efficiency"]]),  # existing data
           value = ifelse(.data[["variable"]] %in% euecToOverwrite,  # hard overwrite
                          .data[["pred"]],
                          .data[["value"]])) %>%
    select("region", "period", "variable", "value") %>%
    filter(!is.na(.data[["value"]]))


  # Correction for efficiencies assumed to be equal
  if (isTRUE(gasBioEquality)) {
    histEfficiencies <- histEfficiencies %>%
      left_join(equalEfficiencies, by = "variable") %>%
      left_join(histEfficiencies,
                by = c("equalTo" = "variable", "period", "region"),
                suffix = c("", ".target")) %>%
      mutate(value = ifelse(!is.na(.data[["equalTo"]]), .data[["value.target"]], .data[["value"]])) %>%
      select(-"equalTo", -"value.target")
  }

  histEfficiencies <- histEfficiencies %>%
    separate(col = "variable", into = c("enduse", "carrier"), sep = "\\.")


  # FE weights for regional aggregation
  feWeights <- histEfficiencies %>%
    select("region", "period", "enduse", "carrier") %>%
    left_join(feWeights %>%
                filter(.data[["unit"]] == "fe") %>%
                select("region", "period", "enduse", "carrier", "value"),
              by = c("region", "period", "enduse", "carrier")) %>%
    group_by(across(all_of(c("region", "period")))) %>%
    reframe(value = sum(.data[["value"]], na.rm = TRUE),
            value = replace_na(.data[["value"]], 0))



  # OUTPUT ---------------------------------------------------------------------

  feWeights <- feWeights %>%
    as.quitte() %>%
    as.magpie()

  histEfficiencies <- histEfficiencies %>%
    as.quitte() %>%
    as.magpie()

  return(list(x = histEfficiencies,
              weight = feWeights,
              min = 0,
              unit = "",
              description = "Historical Conversion Efficiencies from FE to UE"))
}
