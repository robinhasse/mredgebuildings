#' Share of energy carriers or end uses in historic buildings demand
#'
#' Shares are calculated based on energy demands in households and services from
#' the Odyssee data base (EU member states). In case of multi-level shares, the
#' function gives the share w.r.t. to the last categories but for all
#' categories. E.g. 'enduse_carrier' gives the share of each carrier in the
#' demand from each end use.
#' Missing shares are handled as follows: If all data for a region is missing
#' (no demand data at all), the shares remain NA. If a specific category (e.g.,
#' a specific carrier or end use) has no data across all periods for a region,
#' its shares are filled with 0, assuming zero demand for that category.
#' Biomass is split according to GDP per Capita (see toolSplitBiomass).
#'
#' @author Robin Hasse, Antoine Levesque, Hagen Tockhorn
#'
#' @param subtype Character, dimension names, level of shares.
#' @param feOnly if TRUE, output is absolute FE values
#'
#' @returns MAgPIE object with historic shares
#'
#' @importFrom magclass mbind as.magpie
#' @importFrom madrat readSource toolCountryFill toolGetMapping
#' @importFrom quitte as.quitte revalue.levels
#' @importFrom dplyr filter %>% mutate group_by across all_of left_join
#'   summarise .data syms bind_rows pull
#' @importFrom tidyr separate replace_na complete
#' @importFrom utils tail
#' @importFrom mrcommonsenergy toolSplitBiomass
#' @export

calcShareOdyssee <- function(subtype = c("enduse", "carrier", "enduse_carrier"),
                             feOnly = FALSE) {

  # READ-IN DATA ---------------------------------------------------------------

  # Read Buildings Data
  odysseeData <- rbind(readSource("Odyssee", subtype = "260305") %>%
                         as.quitte() %>%
                         mutate(version = "new"),
                       readSource("Odyssee", subtype = "220405") %>%
                         as.quitte() %>%
                         mutate(version = "old"))

  odysseeRegions <- odysseeData %>%
    filter(!is.na(.data$value)) %>%
    pull("region") %>%
    as.character() %>%
    unique()


  # Get GDP per Cap
  gdppop <- calcOutput("GDPpc",
                       scenario = "SSP2",
                       average2020 = FALSE,
                       aggregate = FALSE)

  # carrier mapping
  carrierMap <- toolGetMapping(name = "carrierMap_Odyssee.csv",
                               type = "sectoral",
                               where = "mredgebuildings") %>%
    pull("EDGE", "Odyssee")

  # enduse mapping
  enduseMap <- toolGetMapping(name = "enduseMap_Odyssee.csv",
                              type = "sectoral",
                              where = "mredgebuildings") %>%
    pull("EDGE", "Odyssee")

  # sector mapping
  sectorMap <- toolGetMapping(name = "sectorMap_Odyssee.csv",
                              type = "sectoral",
                              where = "mredgebuildings") %>%
    pull("EDGE", "Odyssee")



  # PARAMETERS -----------------------------------------------------------------

  subtype <- match.arg(subtype)
  shareOf <- strsplit(subtype, "_")[[1]]

  vars <- expand.grid(names(carrierMap),
                      names(sectorMap),
                      names(enduseMap)) %>%
    apply(1, "paste", collapse = "")


  # PROCESS DATA ---------------------------------------------------------------

  # Map Variables
  odyssee <- odysseeData %>%
    filter(.data$region %in% odysseeRegions, .data$variable %in% vars) %>%
    separate("variable", c("carrier", "sector", "enduse"), c(3, 8)) %>%
    revalue.levels(carrier = carrierMap,
                   sector  = sectorMap,
                   enduse  = enduseMap)

  # For service sector: Appliance demand given by difference between reported totals and sum of all other end uses
  # We assume that appliance demand is electric
  # For residential sector: If all demands are given,
  # add the difference between reported totals and sum of all other end uses to appliance demand
  applianceByDifference <- odyssee %>%
    filter(.data$carrier == "elec", .data$enduse != "totals") %>%
    group_by(across(-all_of(c("enduse", "value")))) %>%
    summarise(value = sum(.data$value),
              enduse = "appliances") %>%
    left_join(odyssee %>%
                filter(.data$enduse == "totals"),
              by = c("model", "scenario", "region", "carrier", "sector", "unit", "period", "version"),
              suffix = c("Sep", "Tot")) %>%
    mutate(value = .data$valueTot - .data$valueSep) %>%
    select("model", "scenario", "region", "carrier", "sector", enduse = "enduseSep",
           "unit", "period", "value", "version")

  odyssee <- odyssee %>%
    filter(.data$enduse != "totals") %>%
    rbind(applianceByDifference %>%
            filter(.data$sector == "services")) %>%
    left_join(applianceByDifference %>%
                filter(.data$sector == "residential"),
              by = c("model", "scenario", "region", "carrier", "sector", "enduse", "unit", "period", "version"),
              suffix = c("Data", "Diff")) %>%
    mutate(value = case_when(
      !is.na(.data$valueData) & !is.na(.data$valueDiff) ~ .data$valueData + .data$valueDiff,
      !is.na(.data$valueData) & is.na(.data$valueDiff) ~ .data$valueData,
      .default = NA
    ), .keep = "unused") %>%
    filter(!is.na(.data$value)) %>%
    interpolate_missing_periods(expand.values = TRUE)


  # Use old data where new demand is zero
  odyssee <- odyssee %>%
    select(-"model", -"scenario", -"unit") %>%
    pivot_wider(names_from = "version", values_from = "value") %>%
    group_by(across(-all_of(c("new", "old")))) %>%
    mutate(value = ifelse(
      all(.data$new == 0) | all(.data$region == "GBR"), # GBR is not in new data so use old data
      .data$old,
      .data$new
    ), .keep = "unused") %>%
    ungroup() %>%
    as.quitte()


  # Split Biomass
  if (subtype != "enduse") {
    odyssee <- odyssee %>%
      as.quitte(na.rm = TRUE) %>%
      as.magpie() %>%
      toolSplitBiomass(gdppop, split = "biomod", dim = "carrier") %>%
      as.quitte(na.rm = TRUE) %>%
      select(-"variable")
  }

  # Fill missing "appliances"/"lighting" entries if "appliances_light" has non-NA entries.
  if (subtype %in% c("enduse", "enduse_carrier")) {
    enduseMap["els"] <- "appliances_light"
    vars <- expand.grid(names(carrierMap),
                        names(sectorMap),
                        names(enduseMap)) %>%
      apply(1, "paste", collapse = "")

    # mean distribution of FE between "appliances" and "lighting"
    meanApplightShares <- odyssee %>%
      filter(.data[["enduse"]] %in% c("lighting", "appliances"),
             !is.na(.data[["value"]])) %>%
      group_by(across(all_of(c("period", "sector", "carrier", "enduse")))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
      group_by(across(all_of(c("period", "sector", "carrier")))) %>%
      mutate(share = proportions(.data[["value"]])) %>%
      ungroup() %>%
      select(-"value")

    # split existing aggregated data into "appliances" and "lighting"
    applightData <- odysseeData %>%
      filter(.data[["variable"]] %in% vars) %>%
      separate("variable", c("carrier", "sector", "enduse"), c(3, 8)) %>%
      revalue.levels(carrier = carrierMap,
                     sector  = sectorMap,
                     enduse  = enduseMap)


    # For service sector if lighting demand is missing in addition:
    # Appliance and lighting  demand given by difference between reported totals and sum of all other end uses
    # We assume that appliance and lighting demand is electric
    applightService <- applightData %>%
      filter(.data$sector == "services", .data$carrier == "elec", .data$enduse != "totals") %>%
      group_by(across(-all_of(c("enduse", "value")))) %>%
      filter(is.na(.data$value[.data$enduse == "lighting"])) %>%
      summarise(value = sum(.data$value[.data$enduse != "lighting"]),
                enduse = "appliances_light") %>%
      left_join(applightData %>%
                  filter(.data$enduse == "totals"),
                by = c("model", "scenario", "region", "carrier", "sector", "unit", "period", "version"),
                suffix = c("Sep", "Tot")) %>%
      mutate(value = .data$valueTot - .data$valueSep) %>%
      select("model", "scenario", "region", "carrier", "sector", enduse = "enduseSep",
             "unit", "period", "value", "version")

    applightData <- applightData %>%
      filter(.data$enduse != "totals") %>%
      rbind(applightService) %>%
      filter(!is.na(.data$value)) %>%
      interpolate_missing_periods(expand.values = TRUE) %>%
      filter(.data[["enduse"]] == "appliances_light") %>%
      select(-"enduse") %>%
      inner_join(meanApplightShares,
                 by = c("period", "carrier", "sector"),
                 relationship = "many-to-many") %>%
      mutate(value = .data[["value"]] * .data[["share"]]) %>%
      select("region", "period", "carrier", "enduse", "sector", "value", "version")

    # Use old data where new demand is zero
    applightData <- applightData %>%
      pivot_wider(names_from = "version", values_from = "value") %>%
      group_by(across(-all_of(c("new", "old")))) %>%
      mutate(value = ifelse(all(.data$new == 0), .data$old, .data$new)) %>%
      ungroup() %>%
      filter(!is.na(.data$value)) %>%
      select(-"new", -"old")

    # replace missing values
    odyssee <- applightData %>%
      anti_join(odyssee,
                by = c("region", "period", "carrier", "enduse", "sector")) %>%
      bind_rows(odyssee)

  }

  if (feOnly) {
    # aggregate residential and services sector
    odyssee <- odyssee %>%
      group_by(across(all_of(c("region", "period", "carrier", "enduse")))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
      as.magpie() %>%
      toolCountryFill(verbosity = 2)

    return(list(x = odyssee,
                unit = "FE",
                description = "Aggregated fe values for Odyssee regions"))
  } else {
    #---Calculate Shares
    share <- odyssee %>%
      group_by(across(all_of(c("region", "period", shareOf)))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      toolCalcShares(if (subtype == "enduse_carrier") shareOf else tail(shareOf, 1)) %>%
      complete(region = odysseeRegions, !!!syms(c("period", shareOf))) %>%

      group_by(across(all_of(c("region", shareOf)))) %>%
      mutate(hasPartialNAs = all(is.na(.data$value))) %>%
      group_by(across(all_of(c("region")))) %>%
      mutate(hasAllNAs = all(is.na(.data$value))) %>%
      ungroup() %>%

      mutate(value = ifelse(is.na(.data$value) & .data$hasPartialNAs & !.data$hasAllNAs, 0, .data$value)) %>%
      select(-"hasPartialNAs", -"hasAllNAs")


    # Weights: regional share of final energy
    regShare <- odyssee %>%
      complete(region = odysseeRegions, !!!syms(c("period", "sector", "carrier", "enduse"))) %>%
      interpolate_missing_periods(expand.values = TRUE) %>%
      group_by(across(all_of(c("region", "period", shareOf)))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
      group_by(across(all_of(c("period", shareOf)))) %>%
      mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE)) %>%
      ungroup()



    # OUTPUT -------------------------------------------------------------------

    # Convert to magpie object
    share <- share %>%
      as.magpie() %>%
      toolCountryFill(verbosity = 2)

    regShare <- regShare %>%
      as.magpie() %>%
      collapseDim() %>%
      toolCountryFill(1, verbosity = 2)

    # Check if non-Odyssee regions have been filled with non-NA values
    if (any(!is.na(share[odysseeRegions, , , invert = TRUE])))
      warning("Non-Odyssee regions have been filled with non-NA values.",
              "Please check the region handling in 'calcShareOdyssee'")


    return(list(x = share,
                weight = regShare,
                unit = "1",
                min = 0,
                max = 1,
                description = "Share of carrier or end use in buildings demand"))
  }
}
