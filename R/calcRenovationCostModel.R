#' Calculate linear renovation cost model
#'
#' Specific renovation cost are estimated with a linear model with intercept
#' w.r.t. to the renovation depth.
#'
#' The specific investment cost for renovation depending on the depth of
#' renovation is calculated using a two-level statistical model fitted with
#' data from a European Commission (EC) report on renovation.
#' In a fist step, we fit a simple linear model with intercept to predict
#' specific investment (USD/m2) with relative PE savings. In a second step, this
#' linear model is scaled with one factor for each region. This scaling factor
#' is predicted with GDP/POP using a negative exponential curve. For EU regions
#' that are reported, we correct the result with a region-specific but
#' time-invariant factor to best match the linear model to the data. This way,
#' we get a linear model with intercept for each region that can be extrapolated
#' to other regions (using GDP/POP) which still matches data of EU regions very
#' well. The cost is finally disaggregated across residential building types
#' (SFH, MFH) based on a rough cost factor seen in the ENTRANZE data.
#' @source https://op.europa.eu/s/xnYt
#'
#' @author Robin Hasse
#'
#' @returns MAgPIE object with specific renovation cost
#'
#' @importFrom stats lm nls coef predict
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass mselect as.magpie getSets<- setNames collapseDim
#' @importFrom dplyr %>% .data filter mutate select group_by summarise ungroup
#' left_join group_modify reframe
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom quitte as.quitte calc_addVariable revalue.levels
#' inline.data.frame
#'
#' @export

calcRenovationCostModel <- function() {

  # ASSUMPTIONS ----------------------------------------------------------------

  # temporal scope of the report and of final data set
  periodsReport <- 2014:2017
  periods <- c(2000, 2020)

  # minimum specific investment relative to average in EU
  minFactor <- 0.15

  # cost ratio SFH/MFH cf. ENTRANZE data
  costRatio <- 1.5



  # READ DATA ------------------------------------------------------------------

  # EC report on renovation in EU member states
  renovation <- readSource("EuropeanCommissionRenovation") %>%
    as.quitte(na.rm = TRUE) %>%
    select(-"model", -"scenario", -"period")

  # average income in reporting period: 2014 - 2017
  pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE) %>%
    setNames("population") %>%
    as.quitte() %>%
    select(-"model", -"scenario") %>%
    mutate(variable = as.character(.data$variable), unit = "million cap")
  gdppop <- calcOutput("GDPpc", scenario = "SSP2", aggregate = FALSE, average2020 = FALSE) %>%
    setNames("gdppop") %>%
    as.quitte() %>%
    select(-"model", -"scenario") %>%
    mutate(variable = as.character(.data$variable), unit = "USD2017/cap")
  gdppopAvg <- rbind(gdppop, pop) %>%
    select(-"unit") %>%
    filter(.data$period %in% periodsReport) %>%
    pivot_wider(names_from = "variable") %>%
    group_by(.data$region) %>%
    summarise(value = sum(proportions(.data$population) * .data$gdppop),
              variable = "gdppop",
              unit = "USD2017/cap",
              .groups = "drop") %>%
    ungroup()

  # ratio of SFH
  typeCode <- c(nbrmpr = "SFH",
                nbripr = "MFH")
  typeShare <- readSource("Odyssee") %>%
    mselect(variable = names(typeCode)) %>%
    as.quitte(na.rm = TRUE) %>%
    select(-"model", -"scenario", -"unit") %>%
    filter(.data$period %in% periodsReport) %>%
    revalue.levels(variable = typeCode)
  typeShare <- typeShare %>%
    group_by(across(all_of(c("period", "variable")))) %>%
    reframe(region = setdiff(gdppop$region, typeShare$region),
            value = sum(.data$value)) %>%
    rbind(typeShare)
  typeShare <- typeShare %>%
    group_by(across(all_of(c("region", "variable")))) %>%
    summarise(value = sum(.data$value), .groups = "drop") %>%
    group_by(.data$region) %>%
    mutate(value = proportions(.data$value)) %>%
    pivot_wider(names_from = "variable", values_from = "value")

  # FE demand in residential and commercial buildings as weight
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE)[, periods, ]



  # TRAIN MODEL ----------------------------------------------------------------

  energyRelatedRenovations <- c("Energy related - below Threshold",
                                "Energy related - Light",
                                "Energy related - Medium",
                                "Energy related - Deep")
  modelVariables <- c(`relative PE savings` = "x",
                      `specific investment` = "y")
  subsectors <- as.character(unique(renovation$subsector))

  # independent variables: rel PE savings (x) and gdppop
  # dependent variable: specific investment (y)
  trainingData <- renovation %>%
    select(-"unit") %>%
    filter(.data$renovation %in% energyRelatedRenovations,
           .data$variable %in% names(modelVariables)) %>%
    revalue.levels(variable = modelVariables) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    select("region", "subsector", "x", "y") %>%
    left_join(gdppopAvg %>%
                select("region", gdppop = "value"),
              by = "region") %>%
    mutate(minFactor = minFactor)

  # fit model to report data for each subsector
  modelCalibration <- data.frame()
  correctionFactor <- data.frame()

  for (subsec in subsectors) {

    data <- trainingData %>%
      filter(.data$subsector == subsec)

    # global linear model with intercept
    globalLinearModel <- lm("y~x", data)
    data$yGlobalPredict <- predict(globalLinearModel, newdata = data)

    # regional scaling to best match regional data
    data <- data %>%
      group_by(.data$region) %>%
      group_modify(function(dataReg, reg) {
        regionalScaleModel <- lm("y~yGlobalPredict - 1", dataReg)
        dataReg[["factor"]] <- coef(regionalScaleModel)
        dataReg
      })

    # explain regional scaling with GDP/POP
    scalingModel <- nls(factor - minFactor ~ a * (1 - exp(-c * gdppop)),
                        unique(data[, c("region", "factor", "gdppop", "minFactor")]),
                        list(a = max(data$factor) - minFactor,
                             c = max(data$factor) / max(data$gdppop)))
    data$factorPredict <- predict(scalingModel, newdata = data)

    # model coefficients
    modelCalibration <- modelCalibration %>%
      rbind(data.frame(
        subsector = subsec,
        submodel = rep(c("global", "scaling"), each = 2),
        coef     = c("intercept", "slope", "Asym", "slope"),
        value    = c(coef(globalLinearModel), coef(scalingModel))
      ))

    # correction factor to reach best regional fit (independent of GDP/POP)
    correctionFactor <- data %>%
      mutate(subsector = subsec,
             correct   = .data$factor / .data$factorPredict) %>%
      select("subsector", "region", "correct") %>%
      unique() %>%
      rbind(correctionFactor)
  }



  # PREDICT --------------------------------------------------------------------

  # predict specific investment based on GDP/POP and extrapolate to other
  # regions and periods
  specificInvest <- gdppop %>%
    filter(.data$period %in% periods) %>%
    select("region", "period", gdppop = "value") %>%
    merge(data.frame(subsector = subsectors)) %>%
    left_join(modelCalibration %>%
                unite("coef", c("submodel", "coef")) %>%
                pivot_wider(names_from = "coef", values_from = "value"),
              by = "subsector") %>%
    left_join(correctionFactor,
              by = c("subsector", "region")) %>%
    mutate(factor    = .data$scaling_Asym * (1 - exp(-.data$scaling_slope * .data$gdppop)),
           correct   = replace_na(.data$correct, 1),
           intercept = .data$global_intercept * .data$factor * .data$correct,
           slope     = .data$global_slope  * .data$factor * .data$correct) %>%
    select("region", "period", "subsector", "intercept", "slope") %>%
    pivot_longer(c("intercept", "slope"),
                 names_to = "variable", values_to = "value")



  # FINALISE -------------------------------------------------------------------

  ## Disggregate building types ====

  specificInvest <- specificInvest %>%
    filter(.data$subsector == "residential") %>%
    left_join(typeShare, by = "region") %>%
    mutate(MFH = .data$value / (.data$SFH * costRatio + .data$MFH),
           SFH = costRatio * .data$MFH) %>%
    select(-"value") %>%
    pivot_longer(c("SFH", "MFH"),
                 names_to = "typ", values_to = "value") %>%
    select(-"subsector") %>%
    rbind(specificInvest %>%
            filter(.data$subsector == "commercial") %>%
            select(-"subsector") %>%
            mutate(typ = "Com"))


  ## Unit conversion ====

  # cost data was surveyed 2014 - 2017 but we use 2020 for currency conversion
  specificInvest <- specificInvest %>%
    mutate(value = .data$value * eur2usd(year = 2020))
  unit <- "USD2017/m2"



  # RETURN ---------------------------------------------------------------------

  # convert to magpie object
  specificInvest <- specificInvest %>%
    as.magpie()

  return(list(x = specificInvest,
              weight = feBuildings,
              min = 0,
              description = "floor-area specific renovation investment cost as a function of the renovation depth",
              unit = unit))
}
