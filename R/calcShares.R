#' Historic carrier-Enduse Shares w.r.t Final Energy
#'
#' Merges and transforms the calculated shares from the Datasets:
#' Odyssee
#' IEA_ETP
#' TCEP
#' WEO
#'
#' Enduse shares are extrapolated using a linear regression where the ETP datapoint
#' as well as selected regions from TCEP serves as origin and the entire TCEP dataset
#' provides the growth factor.
#'
#' Carrier shares are exported w.r.t. to all carriers per region per period. In the case of
#' "carrierCorrection = TRUE", carrier shares are exported w.r.t. their contribution
#' to each individual enduse. The reason for the latter is that the implemantation
#' here is closer to EDGE-B, but didn't work well with further data processing
#' steps (e.g. toolDisaggregate) where EC shares are not necessarily used.
#' Hence, this case is by default disabled.
#'
#' In the thermal case, the enduse "appliances" is transformed to "refrigerators"
#' using the region-specific refrigerator share used in EDGE-B. Higher resolved
#' data was not available.
#'
#' @param subtype specifies share
#' @param carrierCorrection allows additional corrections
#' @param feOnly specifies if shares or quantities are returned
#'
#' @note The parameter "feOnly" is only applicable to IEA_ETP and TCEP data,
#' since this is the necessary data to do a full disaggregation of EU and EC
#' data, whereas Odyssee already gives disaggregated data for countries of the
#' European Union.
#'
#' @note As done in EDGE-B, enduse-disaggregated FE data from WEO for the regions
#' MIE, AFR and JAP were used instead for values from IEA ETP.
#'
#' @returns data.frame with historic energy demands
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom dplyr mutate as_tibble filter select rename group_by across lead
#'   all_of ungroup %>% .data left_join reframe cross_join .data
#' @importFrom tidyr replace_na unite complete
#' @importFrom madrat toolGetMapping calcOutput readSource toolCountryFill
#' @importFrom magclass time_interpolate as.magpie dimSums getItems
#' @importFrom quitte  as.quitte interpolate_missing_periods
#' @export

calcShares <- function(subtype = c("carrier_nonthermal",
                                   "carrier_thermal",
                                   "enduse_nonthermal",
                                   "enduse_thermal"),
                       carrierCorrection = FALSE,
                       feOnly = FALSE) {



  # PARAMETERS -----------------------------------------------------------------

  subtype <- match.arg(subtype)

  shareOf  <- strsplit(subtype, "_")[[1]][1]
  thermVar <- strsplit(subtype, "_")[[1]][2]


  # Regions taken into account from WEO
  regWEO <- c("Africa", "Middle East", "Japan")



  # READ-IN DATA ---------------------------------------------------------------

  ## Main Datasets ====

  # Shares

  # Odyssee
  sharesOdyssee <-
    calcOutput("ShareOdyssee", subtype = shareOf, aggregate = FALSE) %>%
    as.quitte()

  # IEA ETP
  sharesETP <-
    calcOutput("ShareETP", subtype = shareOf, feOnly = FALSE, aggregate = FALSE) %>%
    as.quitte()

  # WEO
  sharesWEO <- calcOutput("ShareWEO", aggregate = FALSE) %>%
    as.quitte()

  if (shareOf == "enduse") {
    # TCEP
    sharesTCEP <- calcOutput("ShareTCEP", aggregate = FALSE) %>%
      as.quitte()

    sharesChina <- calcOutput("ShareChina", aggregate = FALSE) %>%
      as_tibble(na.rm = TRUE)
  }


  # FE

  # IEA ETP
  feETP <-
    calcOutput("ShareETP", subtype = shareOf, feOnly = TRUE, aggregate = FALSE) %>%
    as.quitte()

  if (shareOf == "enduse") {
    # TCEP
    feTCEP <- readSource("TCEP") %>%
      as.quitte()

    feChina <- readSource("IEA_China") %>%
      as_tibble(na.rm = TRUE) %>%
      filter(.data$enduse != "other")
  }


  #--- Mappings

  # EDGE mapping
  regmappingEDGE <- toolGetMapping(name  = "regionmappingEDGE.csv",
                                   type  = "regional",
                                   where = "mredgebuildings")

  # ETP mapping
  regmappingETP <- toolGetMapping(name  = "regionmappingIEA_ETP.csv",
                                  type  = "regional",
                                  where = "mredgebuildings")

  # WEO mapping
  regmappingWEO <- toolGetMapping(name  = "regionmappingWEO.csv",
                                  type  = "regional",
                                  where = "mredgebuildings")

  # Carrier share corrections
  shareCorrections <- toolGetMapping(name  = "correction_carrierShares.csv",
                                     type  = "sectoral",
                                     where = "mredgebuildings")

  # Percentage of Appliances for Refrigerators
  fridgeShare <- toolGetMapping(name  = "fridgeShare.csv",
                                type  = "sectoral",
                                where = "mredgebuildings")

  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- toolGetMapping(name  = "excludeEnduseCarrier.csv",
                            type  = "sectoral",
                            where = "mredgebuildings")




  # FUNCTIONS ------------------------------------------------------------------

  normalize <- function(df, shareOf) {
    df <- df %>%
      group_by(across(-all_of(c(shareOf, "value")))) %>%
      mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE)) %>%
      ungroup()
  }

  getGrowth <- function(df, regmap, regionAgg = "EEAReg") {
    regmap <- regmap %>%
      select(region = "CountryCode", regionAgg = !!regionAgg)
    df %>%
      left_join(regmap, by = "region") %>%
      select("region", "period", "enduse", "regionAgg", "value") %>%
      group_by(across(all_of(c("regionAgg", "enduse", "period")))) %>%
      reframe(value = sum(.data[["value"]], na.rm = TRUE)) %>%
      group_by(across(all_of(c("regionAgg", "enduse")))) %>%
      reframe(growth = (lead(.data[["value"]]) /
                          .data[["value"]])^(1 / c(diff(.data[["period"]]), NA))) %>%
      filter(!is.na(.data[["growth"]])) %>%
      left_join(regmap, by = "regionAgg", relationship = "many-to-many") %>%
      select(-"regionAgg")
  }

  extrapolateGrowth <- function(df, growth, periods = 1990:2022) {
    df %>%
      left_join(growth, by = c("region", "enduse")) %>%
      group_by(across(all_of(c("region", "enduse")))) %>%
      reframe(value = .data[["value"]] * .data[["growth"]]^(periods - .data[["period"]]),
              period = periods)
  }

  replaceData <- function(data, replace, afterRef) {
    after <- afterRef %>%
      getElement("period") %>%
      max()

    replace <- replace %>%
      filter(!is.na(.data$value))

    dropDataPoints <- data %>%
      filter(.data$period > after) %>%
      semi_join(replace, by = setdiff(colnames(replace), c("value", "period")))

    periods <- unique(data[["period"]])
    periods <- periods[periods > after]

    data %>%
      anti_join(dropDataPoints, by = setdiff(colnames(replace), c("value"))) %>%
      rbind(replace) %>%
      interpolate_missing_periods(periods)
  }



  # PROCESS DATA ---------------------------------------------------------------

  # Adjust ETP Mapping
  regmappingETP <- regmappingETP %>%
    mutate(EEAReg = ifelse(.data[["EEAReg"]] == "rest",
                           .data[["OECD"]],
                           .data[["EEAReg"]]))

  if (shareOf == "enduse") {
    # Analogous to EDGE-B, the TCEP Dataset is used to determine a growth / evolution
    # factor of a given Enduse Share. This factor will be used to extrapolate a
    # second Datapoint for the ETP Dataset (highest priority), enabling a linear
    # regression of the time evolution. WEO datapoints were used for selected regions.

    # Define TCEP regions for enduse shares
    regWEO <- regmappingWEO %>%
      filter(.data[["RegionCode"]] %in% regWEO) %>%
      pull("CountryCode")

    # combine ETP and WEO enduse shares
    shares <- sharesETP %>%
      select("region", "period", "enduse", "value") %>%
      left_join(sharesWEO %>%
                  filter(.data[["region"]] %in% regWEO,
                         .data[["period"]] %in% unique(sharesETP$period)) %>%
                  select("region", "period", "enduse", "value"),
                by = c("region", "period", "enduse")) %>%
      mutate(value = ifelse(is.na(.data[["value.y"]]),
                            .data[["value.x"]],
                            .data[["value.y"]])) %>%
      select(-"value.x", -"value.y")



    if (isFALSE(feOnly)) {
      # Extrapolate ETP FE Data
      evolutionFactor <- getGrowth(sharesTCEP, regmappingETP)
      sharesFull <- extrapolateGrowth(shares, evolutionFactor) %>%
        replaceData(sharesChina, afterRef = shares) %>%
        normalize(shareOf)

      # Merge Data
      data <- sharesOdyssee %>%
        select("region", "period", "enduse", "value") %>%
        full_join(sharesFull, by = c("region", "period", shareOf)) %>%
        mutate(value = ifelse(is.na(.data[["value.x"]]),
                              .data[["value.y"]],
                              .data[["value.x"]])) %>%
        select(-"value.x", -"value.y")
    }


    # Calculate FE Data

    # Extrapolate ETP FE Data
    evolutionFactor <- getGrowth(feTCEP, regmappingETP)
    feETPfull <- extrapolateGrowth(feETP, evolutionFactor) %>%
      replaceData(feChina, afterRef = feETP)

    if (feOnly) {
      data  <- feETPfull
    } else {
      regFE <- feETPfull
    }
  }


  #---Carrier Resolution needs special Attention
  if (shareOf == "carrier") {
    # Merge Data
    data <- sharesOdyssee %>%
      select(-"unit", -"variable", -"model", -"scenario") %>%
      left_join(sharesETP, by = c("region", "period", shareOf)) %>%
      mutate(value = ifelse(is.na(.data[["value.x"]]),
                            .data[["value.y"]],
                            .data[["value.x"]])) %>%
      select(-"value.x", -"value.y", -"unit", -"variable", -"model", -"scenario")

    # Interpolate Data
    data <- data %>%
      interpolate_missing_periods(period = seq(1990, 2022), expand.values = TRUE) %>%
      filter(.data$period %in% 1990:2022)


    if (isTRUE(carrierCorrection)) {
      # The Carrier resolution will be extended by a further added resolution for
      # Enduses, which are however not disaggregated. This only serves the purpose
      # of facilitating specific corrections.
      # However, this made the handling in toolDisaggregate more error-prone.

      # NOTE: I haven't followed this approach any further since toolDisaggregate
      # was changed to only take enduse data from an external dataset for its
      # calculation. Hence, "carrierCorrection = FALSE" by default.

      # Add EDGE Mapping
      data <- left_join(data,
                        regmappingEDGE %>%
                          select(-"RegionCodeEUR", -"RegionCodeEUR_ETP", -"X") %>%
                          rename(region = "CountryCode"),
                        by = "region")


      # Load Enduses
      enduses <- calcOutput("ShareOdyssee", subtype = "enduse", aggregate = FALSE) %>%
        as.quitte(na.rm = TRUE) %>%
        select("enduse") %>%
        unique()


      # Cross-join Enduses to Carriers to give further level of resolution
      data <- cross_join(data, enduses)

      # Re-normalize to give Enduse-specific Carrier Share
      data <- normalize(data, shareOf)



      #---Correct some specific EC-EU combinations

      # Specific Exclusion
      data <- data %>%
        unite(col = "EUEC", .data[["enduse"]], .data[["carrier"]], sep = "-", remove = FALSE) %>%
        # anti_join(data.frame("EUEC" = c(exclude, excludeNEW)), by = "EUEC") %>%
        mutate(value = ifelse(.data[["EUEC"]] %in% exclude,
                              0,
                              .data[["value"]])) %>%
        select(-"EUEC")


      # (1) Make "space_cooling" and "appliances" fully electric
      # (2) If only one of "water_heating", "space_heating" is non-empty, use  one for the other
      data <- data %>%
        spread("enduse", "value") %>%
        mutate(space_cooling = ifelse(.data[["carrier"]] == "elec", 1, 0),
               appliances    = ifelse(.data[["carrier"]] == "elec", 1, 0)) %>%
        mutate(space_heating = ifelse(is.na(.data[["space_heating"]]) & !is.na(.data[["water_heating"]]),
                                      .data[["water_heating"]],
                                      .data[["space_heating"]]),
               water_heating = ifelse(is.na(.data[["water_heating"]]) & !is.na(.data[["space_heating"]]),
                                      .data[["space_heating"]],
                                      .data[["water_heating"]])) %>%
        gather("enduse", "value", -any_of(colnames(data))) %>%
        mutate(value = replace_na(.data[["value"]], 0))


      # Make more specific corrections
      data <- data %>%
        left_join(shareCorrections, by = c("RegionCode" = "region", "enduse", "carrier")) %>%
        mutate(value = ifelse(is.na(.data[["correction"]]),
                              .data[["value"]],
                              .data[["correction"]])) %>%
        select(-"correction", -"RegionCode") %>%
        normalize(shareOf)
    }
  } else {
    data <- data %>%
      interpolate_missing_periods(period = seq(1990, 2022), expand.values = TRUE) %>%
      filter(.data$period %in% 1990:2022)
  }

  if (isFALSE(feOnly)) {
    # replace NA's
    data <- data %>%
      mutate(value = replace_na(.data[["value"]], 0))
  }


  #---Thermal Part
  if (thermVar == "thermal") {
    if (shareOf == "enduse") {
      data <- data %>%
        toolAddThermal(regmappingEDGE, fridgeShare, feOnly = FALSE, shareOf)

      if (isFALSE(feOnly)) {
        regFE <- regFE %>%
          toolAddThermal(regmappingEDGE, fridgeShare)
      }
    }
  }




  # OUTPUT ---------------------------------------------------------------------


  data <- data %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(1, verbosity = 2)


  if (isTRUE(feOnly)) {
    weight <- NULL
    unit   <- "EJ"
    max    <- NULL

    description <- "Final energy demand of carrier or end use in buildings"
  } else {
    weight <- regFE %>%
      as.quitte() %>%
      as.magpie() %>%
      dimSums() %>%
      time_interpolate(getItems(data, 2)) %>%
      toolCountryFill(1, verbosity = 2)
    unit   <- "1"
    max    <- 1

    description <- "Share of carrier or end use in buildings final energy demand"
  }

  return(list(x = data,
              weight = weight,
              unit = unit,
              min = 0,
              max = max,
              description = description))
}
