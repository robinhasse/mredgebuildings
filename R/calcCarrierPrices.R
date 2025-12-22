#' Calculate carrier prices
#'
#' Final energy carrier prices including energy supply cost, transport and
#' distribution cost(T&D) and taxes (without VAT). CO2 price component and VAT
#' are added later depending on the scenario assumptions.
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource
#' @importFrom dplyr %>% .data select mutate group_by across all_of any_of
#'   summarise left_join full_join cross_join rename as_tibble
#' @importFrom tidyr pivot_longer complete
#' @importFrom quitte as.quitte inline.data.frame interpolate_missing_periods
#' @importFrom madrat toolGetMapping
#' @export

calcCarrierPrices <- function() {

  # temporal scope
  periodsPast <- 2000:2025
  periodsFuture <- seq(2030, 2100, 5)
  periods <- c(periodsPast, periodsFuture)

  # levels of future projections
  lvs <- c("low", "central", "high")

  # TODO: revisit unit conversion. This is extremely inefficient # nolint: todo_comment_linter.
  .eur2usd <- function(x, year, iso3c) {
    tryCatch(
      GDPuc::convertSingle(x = x,
                           iso3c = iso3c,
                           year = year,
                           unit_in = paste("constant", year, "EUR"),
                           unit_out = "constant 2017 US$MER"),
      error = function(e) {
        if (grepl("No information in source .+ for countries in 'gdp'\\.",
                  e$message)) {
          x
        } else {
          e
        }
      },
      warning = function(w) {
        if (grepl("NAs have been generated for countries lacking conversion factors",
                  w$message)) {
          x
        } else {
          w
        }
      }
    )
  }



  # Energy prices --------------------------------------------------------------

  # prices w/o VAT w/o CO2 price

  # assume coarse VAT of 1.2
  vat <- 1.2


  ## district heating ====

  # original prices incl. VAT that gets removed here
  dhPrices <- readSource("Energiforsk2016") %>%
    toolCountryFillAvg(verbosity = 2) %>%
    as.quitte(na.rm = TRUE) %>%
    select("region", "period", "value") %>%
    mutate(region = as.character(.data$region),
           carrier = "heat",
           value = unlist(Map(.eur2usd,
                              x = .data$value,
                              year = .data$period,
                              iso3c = .data$region)), # EUR/GJ to USD/GJ
           value = .data$value * 3.6E-3 / vat) # USD/GJ to USD/kWh

  # extrapolate prices linearly for past periods, constant in future
  extrapolate <- function(p, v, until) {
    linModel <- lm(value ~ period, data.frame(period = p, value = v))
    vUntil <- predict(linModel, newdata = data.frame(period = until))[[1]]
    return(c(v, vUntil))
  }
  dhPrices <- dhPrices %>%
    group_by(across(all_of(c("region", "carrier")))) %>%
    reframe(value = extrapolate(.data$period, .data$value,
                                max(periodsPast)),
            period = c(.data$period, max(periodsPast))) %>%
    ungroup() %>%
    interpolate_missing_periods(periods, expand.values = TRUE) %>%
    filter(.data$period %in% periods)


  ## liquids, elec, gas, coal ====

  # original prices incl. VAT that gets removed here
  oecdPrices <- readSource("IEA_OECD_Prices", "EPT_prices_NC_per_toe") %>%
    mselect(V3 = "HOUSEHOLDS",
            V4 = "NCPRICE/TOE",
            collapseNames = TRUE) %>%
    as_tibble()

  # map carriers
  carrierMap <- toolGetMapping("carrierMapping_IEA_OECD.csv",
                               type = "sectoral",
                               where = "mredgebuildings")
  oecdPrices <- oecdPrices %>%
    right_join(carrierMap, by = c(V2 = "carrier2")) %>%
    select(-"V2", -"carrier1")

  # convert to USD
  lcu2usd <- data.frame(region = unique(oecdPrices$region)) %>%
    mutate(lcu2usd = GDPuc::toolConvertSingle(1, .data$region, 2015,
                                              unit_in = "current LCU",
                                              unit_out = "constant 2017 US$MER"))
  oecdPrices <- oecdPrices %>%
    left_join(lcu2usd, by = "region") %>%
    mutate(value = .data$value / vat * .data$lcu2usd / 1.163E4, # lcu/toe to usd/kWh
           .keep = "unused") %>%
    interpolate_missing_periods(periods) %>%
    filter(.data$period %in% periods)


  ## project with ECEMF ====

  # map ECEMF carriers
  # assumption: liquids and gases remain fossil
  carrierMap <- toolGetMapping("carrierMapping_ECEMF.csv",
                               type = "sectoral",
                               where = "mredgebuildings")

  # all considered price components
  # (drop CO2 price as this is a scenario assumption)
  components <- c("energy",
                  "T&D&Sales markup",
                  "Tax markup (w/o VAT)",
                  "Pellet processing markup")

  # price assumptions for all carriers except district heating
  ecemfPrices <- readSource("ECEMF", "FEPrices") %>%
    toolCountryFillAvg(verbosity = 2) %>%
    as.quitte(na.rm = TRUE) %>%
    left_join(carrierMap, by = c(carrier = "carrierECEMF")) %>%
    filter(.data$sector %in% c("Res&Com", "Supply"),
           !is.na(.data$carrier.y),
           .data$component %in% components) %>%
    select("region", "period", carrier = "carrier.y", "component", "value") %>%
    group_by(across(-all_of(c("component", "value")))) %>%
    summarise(value = sum(.data$value), .groups = "drop")

  # convert to USD
  # TODO: revisit unit conversion. This is extremely inefficient # nolint: todo_comment_linter.
  eur2usdFactor <- ecemfPrices %>%
    select("region") %>%
    unique() %>%
    mutate(eur2usd = unlist(Map(.eur2usd, x = 1, year = 2020, iso3c = .data$region))) %>%
    replace_na(list(eur2usd = eur2usd(year = 2020)))

  ecemfPrices <- ecemfPrices %>%
    left_join(eur2usdFactor, by = "region") %>%
    mutate(value = .data$value * 1E-3 * .data$eur2usd, .keep = "unused") %>% # EUR/MWh to USD/kWh
    interpolate_missing_periods(periods, expand.values = TRUE) %>%
    filter(.data$period %in% periods)

  # all prices
  prices <- rbind(oecdPrices, dhPrices) %>%
    full_join(ecemfPrices,
              by = c("region", "period", "carrier"),
              suffix = c("", "ECEMF")) %>%
    group_by(across(all_of(c("region", "carrier")))) %>%
    mutate(eod = if (all(is.na(.data$value))) NA else  max(.data$period[!is.na(.data$value)]),
           value = case_when(!is.na(.data$value) ~ .data$value,
                             all(is.na(.data$value)) ~ .data$valueECEMF,
                             .default = .data$valueECEMF *
                               (.data$value / .data$valueECEMF)[.data$period == .data$eod]),
           variable = "price",
           unit = "USD/kWh",
           level = "central") %>%
    ungroup() %>%
    select(-"valueECEMF", -"eod") %>%
    filter(.data$period %in% periods)



  # Emission intensity ---------------------------------------------------------


  ## past ====

  # remind results
  emiHeat <- inline.data.frame("region; period; value",
                               "CAZ; 2005; 0.00038",
                               "CAZ; 2010; 0.00046",
                               "CAZ; 2015; 0.00051",
                               "CAZ; 2020; 0.00036",
                               "CAZ; 2025; 0.00022",
                               "CHA; 2005; 0.00046",
                               "CHA; 2010; 0.00042",
                               "CHA; 2015; 0.00036",
                               "CHA; 2020; 0.00033",
                               "CHA; 2025; 0.00031",
                               "DEU; 2005; 0.00031",
                               "DEU; 2010; 0.00031",
                               "DEU; 2015; 0.00032",
                               "DEU; 2020; 0.00029",
                               "DEU; 2025; 0.00027",
                               "ECE; 2005; 0.00041",
                               "ECE; 2010; 4e-04",
                               "ECE; 2015; 0.00036",
                               "ECE; 2020; 3e-04",
                               "ECE; 2025; 2e-04",
                               "ECS; 2005; 0.00034",
                               "ECS; 2010; 0.00035",
                               "ECS; 2015; 0.00034",
                               "ECS; 2020; 0.00029",
                               "ECS; 2025; 0.00023",
                               "ENC; 2005; 0.00015",
                               "ENC; 2010; 0.00013",
                               "ENC; 2015; 7.3e-05",
                               "ENC; 2020; 3.1e-05",
                               "ENC; 2025; 6.7e-06",
                               "ESC; 2005; 0.00024",
                               "ESC; 2010; 0.00023",
                               "ESC; 2015; 0.00022",
                               "ESC; 2020; 0.00018",
                               "ESC; 2025; 0.00013",
                               # "ESW; 2005; 0.00091",
                               "ESW; 2010; 0.00025",
                               "ESW; 2015; 6.2e-05",
                               "ESW; 2020; 8.9e-06",
                               "ESW; 2025; 5.4e-07",
                               "EWN; 2005; 0.00023",
                               "EWN; 2010; 0.00025",
                               "EWN; 2015; 0.00025",
                               "EWN; 2020; 0.00024",
                               "EWN; 2025; 0.00021",
                               "FRA; 2005; 2e-04",
                               "FRA; 2010; 2e-04",
                               "FRA; 2015; 0.00019",
                               "FRA; 2020; 0.00012",
                               "FRA; 2025; 6.7e-05",
                               "IND; 2010; 0",
                               "IND; 2015; 0.00025",
                               "IND; 2020; 0.00044",
                               "IND; 2025; 0.00045",
                               "JPN; 2005; 0.00017",
                               "JPN; 2010; 2e-04",
                               "JPN; 2015; 0.00022",
                               "JPN; 2020; 0.00026",
                               "JPN; 2025; 0.00029",
                               "LAM; 2010; 0",
                               "LAM; 2015; 1.4e-05",
                               "LAM; 2020; 9.3e-05",
                               "LAM; 2025; 0.00024",
                               "MEA; 2010; 0",
                               "MEA; 2015; 5.5e-06",
                               "MEA; 2020; 0.00025",
                               "MEA; 2025; 4e-04",
                               "NEN; 2005; 6e-05",
                               "NEN; 2010; 9.4e-05",
                               "NEN; 2015; 0.00014",
                               "NEN; 2020; 0.00015",
                               "NEN; 2025; 0.00014",
                               "NES; 2005; 9.7e-05",
                               "NES; 2010; 0.00011",
                               "NES; 2015; 0.00012",
                               "NES; 2020; 0.00013",
                               "NES; 2025; 0.00014",
                               "OAS; 2005; 3e-04",
                               "OAS; 2010; 3e-04",
                               "OAS; 2015; 3e-04",
                               "OAS; 2020; 0.00032",
                               "OAS; 2025; 0.00028",
                               "REF; 2005; 0.00033",
                               "REF; 2010; 0.00033",
                               "REF; 2015; 0.00032",
                               "REF; 2020; 0.00031",
                               "REF; 2025; 0.00029",
                               "SSA; 2010; 0",
                               "SSA; 2015; 0",
                               "SSA; 2020; 5e-05",
                               "SSA; 2025; 0.00015",
                               "UKI; 2005; 4e-04",
                               "UKI; 2010; 0.00034",
                               "UKI; 2015; 0.00027",
                               "UKI; 2020; 0.00021",
                               "UKI; 2025; 0.00014",
                               "USA; 2005; 0.00037",
                               "USA; 2010; 0.00029",
                               "USA; 2015; 0.00021",
                               "USA; 2020; 0.00017",
                               "USA; 2025; 0.00015") %>%
    interpolate_missing_periods(periodsPast, expand.values = TRUE) %>%
    filter(.data$period %in% periodsPast) %>%
    right_join(toolGetMapping("regionmapping_21_EU11.csv", "regional", "mappingfolder"),
               by = c(region = "RegionCode"),
               relationship = "many-to-many") %>%
    select(region = "CountryCode", "period", "value") %>%
    mutate(unit = "t_CO2/kWh",
           carrier = "heat")

  # historic emission intensity from EEA
  emiElec <- readSource("EEA_EuropeanEnvironmentAgency",
                        "ghgEmissionIntensityElec") %>%
    toolCountryFillAvg() %>%
    as.quitte(na.rm = TRUE) %>%
    interpolate_missing_periods(periodsPast, expand.values = TRUE) %>%
    filter(.data$period %in% periodsPast) %>%
    select("region", "period", "value") %>%
    mutate(value = .data$value * 1E-6,  # g/kWh to t/kWh
           unit = "t_CO2/kWh",
           carrier = "elec")

  # emission intensity of other carriers
  emiOther <- readSource("ECEMF", "emissionFactor") %>%
    as.quitte(na.rm = TRUE) %>%
    left_join(carrierMap, by = c(carrier = "carrierECEMF")) %>%
    select(carrier = "carrier.y", "value") %>%
    rbind(data.frame(carrier = "biomod", value = 0)) %>% # set biomod to zero
    mutate(value = .data$value * 1E-3,  # t/MWh to t/kWh
           unit = "t_CO2/kWh")

  # all emission factors
  emi <- Reduce(x = list(emiHeat, emiElec, emiOther), f = function(x, y) {
    cols <- setdiff(intersect(colnames(x), colnames(y)), c("carrier", "value"))
    fun <- if (length(cols) > 0) full_join else cross_join
    args <- lapply(list(x, y), pivot_wider, names_from = "carrier")
    if (identical(fun, full_join)) args <- c(args, list(by = cols))
    do.call(fun, args) %>%
      pivot_longer(cols = -any_of(union(colnames(x), colnames(y))),
                   names_to = "carrier")
  })


  ## future ====

  # assume long-term residual emission intensity
  emi <- expand.grid(carrier = c("elec", "heat"),
                     level = lvs) %>%
    mutate(period = case_when(.data$level == "low" & .data$carrier == "elec" ~ 2040,
                              .data$level == "low" & .data$carrier == "heat" ~ 2045,
                              .default = 2050),
           factor = case_when(.data$level == "low"                               ~ 0,
                              .data$level == "central" & .data$carrier == "elec" ~ 0.1,
                              .data$level == "central" & .data$carrier == "heat" ~ 0.2,
                              .default = 1)) %>%
    complete(carrier = unique(emi$carrier),
             level = lvs,
             fill = list(factor = 1, period = max(periodsFuture))) %>%
    full_join(emi %>%
                filter(.data$period == max(periodsPast)) %>%
                select(-"period"),
              by = "carrier",
              relationship = "many-to-many") %>%
    mutate(value = .data$value * .data$factor) %>%
    select(-"factor") %>%
    rbind(cross_join(emi, data.frame(level = lvs))) %>%
    interpolate_missing_periods(periods, expand.values = TRUE) %>%
    filter(.data$period %in% periods) %>%
    mutate(variable = "emi",
           unit = "t_CO2/kWh")



  # Combine data ---------------------------------------------------------------
  data <- rbind(prices, emi)

  # all carriers included?
  carrier <- toolGetMapping("dim_hs.csv",
                            type = "sectoral", where = "brick") %>%
    select("carrier") %>%
    unique()
  data <- data %>%
    right_join(carrier, by = "carrier")
  if (any(is.na(data))) {
    stop("Incomplete mapping of energy carriers.")
  }

  data <- data  %>%
    as.quitte() %>%
    as.magpie()

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    dimSums("typ") %>%
    time_interpolate(periods, extrapolation_type = "constant") %>%
    toolCountryFillAvg()

  return(list(x = data,
              unit = "2017Int$PPP/kWh or t_CO2/kWh",
              weight = feBuildings,
              min = 0,
              description = "Components of FE carrier prices"))
}
