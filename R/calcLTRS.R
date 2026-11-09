#' Calculate data based on Long Term Renovation Strategies
#'
#' Extrapolate the residential heating system distribution in each vintage from
#' DEU to other countries and to commercial buildings.
#'
#' We use iterative proportional fitting to extrapolate the German residential
#' pattern to other countries and subsectors. This data is extremely rough and
#' should be taken with care. It is helpful in the matching nonetheless to
#' represent interactions between vintage and heating systems while other more
#' reliable references assure overall consistency.
#'
#' @param refGranularity character, for \code{"typ"}, you get the most granular
#'   data with residential buildings split by SFH and MFH. However, this data is
#'   less reliable and incomplete. \code{"sec"} will aggregate residential
#'   buildings to Res. It should be complete for EU27.
#' @returns MagPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource calcOutput toolGetMapping toolCountryFill
#' @importFrom quitte removeColNa as.quitte
#' @importFrom utils read.csv
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom dplyr %>% .data filter left_join select group_by summarise all_of
#'   across group_modify mutate ungroup pull rename matches semi_join right_join
#'   reframe

calcLTRS <- function(refGranularity = c("typ", "sec")) {

  refGranularity <- match.arg(refGranularity)

  # year to extrapolate to
  periodCensusHub <- 2021
  periodHotmaps <- 2016



  # FUNCTIONS ------------------------------------------------------------------


  # convert to compatible data frame
  .asDf <- function(x, year = NULL) {
    df <- as_tibble(x) %>%
      filter(!is.na(.data$value))
    if (!is.null(year)) {
      df <- df[df$period == year, ]
    }
    removeColNa(df)
  }


  # map vintages to BRICK resolution
  .mapVin <- function(df, map) {
    right_join(df, map, by = c("vintage"), relationship = "many-to-many") %>%
      group_by(across(all_of(c("region", "year", "vin", "buildingType", "technology")))) %>%
      summarise(value = sum(.data$weight * .data$value),
                .groups = "drop") %>%
      group_by(across(-all_of(c("technology", "value")))) %>%
      mutate(value = proportions(.data$value)) %>%
      ungroup()
  }


  # map dimensions to fit reference data
  .remap <- function(df, map, value = NULL, by = "variable", normaliseBy = NULL) {
    df <- left_join(map, df, by = by, relationship = "many-to-many") %>%
      select(-all_of(by)) %>%
      group_by(across(-all_of("value"))) %>%
      summarise(value = sum(.data$value), .groups = "drop")
    if (!is.null(normaliseBy)) {
      df <- df %>%
        group_by(across(all_of(normaliseBy))) %>%
        mutate(value = proportions(.data$value)) %>%
        ungroup()
    }
    if (!is.null(value)) {
      names(df)[names(df) == "value"] <- value
    }
    df
  }


  # rescale reference data to match regional aggregated shares
  .rescale <- function(df, key) {

    # auxiliary column to name each granular share
    df$name <- paste(df$vin, df$technology, sep = "_")

    # reference shares
    sharesDisagg <- pull(df, "value", "name")

    # regional aggregated shares
    sharesAgg <- list(vin = list(name = "vin",        value = "vinShare"),
                      hs =  list(name = "technology", value = "hsShare")) %>%
      lapply(function(x) {
        df %>%
          select(name = x$name, value = x$value) %>%
          unique()
      }) %>%
      do.call(what = rbind) %>%
      pull("value", "name")

    # matrix that sums granular shares to aggregated shares
    aggMat <- do.call(rbind, lapply(c("vin", "technology"), function(col) {
      df %>%
        select(all_of(c("name", col))) %>%
        mutate(value = 1) %>%
        pivot_wider(names_from = "name", values_fill = 0) %>%
        tibble::column_to_rownames(col)
    })) %>%
      as.matrix()
    aggMat <- aggMat[names(sharesAgg), names(sharesDisagg)] # ensure matrix-vector consistency

    # proportional iterative fitting
    df$value <- ipfp::ipfp(y = sharesAgg, A = aggMat, x0 = sharesDisagg)
    df$name <- NULL

    df
  }



  # READ AND MAP ---------------------------------------------------------------


  ## mappings ====

  mapLTRS <- toolGetMapping("buildingTypeMapping_LTRS.csv",
                            type = "sectoral", where = "mredgebuildings") %>%
    select(-matches("^\\."))
  mapVinLTRS <- toolGetMapping("vintageMapping_LTRS.csv",
                               type = "sectoral", where = "mredgebuildings")
  mapCensusHub <- toolGetMapping("CensusHub_typ_vin2LTRS.csv",
                                 type = "sectoral", where = "mredgebuildings")
  mapOdysseeIDEESheating <- toolGetMapping("OdysseeIDEES_heating2LTRS.csv",
                                           type = "sectoral", where = "mredgebuildings",
                                           returnPathOnly = TRUE) %>%
    read.csv(encoding = "UTF-8")
  mapOdysseeIDEEStyp <- toolGetMapping("OdysseeIDEES_typ2LTRS.csv",
                                       type = "sectoral", where = "mredgebuildings")
  mapHotmaps <- toolGetMapping("Hotmaps_sec_vin2LTRS.csv",
                               type = "sectoral", where = "mredgebuildings")
  mapIDEES <- toolGetMapping("IDEES_heatingShare2LTRS.csv",
                             type = "sectoral", where = "mredgebuildings",
                             returnPathOnly = TRUE) %>%
    read.csv(encoding = "UTF-8")


  ## reference data ====

  # at full granularity (extrapolated to commercial buildings)
  dataDEU <- readSource("LTRS") %>%
    .asDf() %>%
    .mapVin(mapVinLTRS) %>%
    rename(period = "year")
  dataRef <- dataDEU %>%
    .remap(mapLTRS, by = "buildingType")


  ## aggregated data ====

  # for more regions

  vinShares <- switch(refGranularity,
    typ = rbind(
      # residential
      calcOutput("MatchingReference", subtype = "CensusHub_typ_vin", aggregate = FALSE) %>%
        .asDf(year = periodCensusHub) %>%
        .remap(mapCensusHub, value = "vinShare", normaliseBy = c("region", "period", "typ")),
      # commercial
      calcOutput("MatchingReference", subtype = "Hotmaps_sec_vin", aggregate = FALSE) %>%
        .asDf(year = periodHotmaps) %>%
        .remap(filter(mapHotmaps, .data$typ == "Com"), value = "vinShare")
    ),
    sec = calcOutput("MatchingReference", subtype = "Hotmaps_sec_vin", aggregate = FALSE) %>%
      .asDf(year = periodHotmaps) %>%
      .remap(mapHotmaps, value = "vinShare")
  )
  hsShares <- switch(refGranularity,
    typ = rbind(
      # residential
      calcOutput("MatchingReference", subtype = "OdysseeIDEES_heating_typ", aggregate = FALSE) %>%
        .asDf(year = periodCensusHub) %>%
        .remap(mapOdysseeIDEESheating, value = "hsShare", normaliseBy = c("region", "period", "typ")),
      # commercial
      calcOutput("MatchingReference", subtype = "IDEES_heatingShare", aggregate = FALSE) %>%
        .asDf(year = periodHotmaps) %>%
        .remap(filter(mapIDEES, .data$typ == "Com"), value = "hsShare")
    ),
    sec = calcOutput("MatchingReference", subtype = "IDEES_heatingShare", aggregate = FALSE) %>%
      .asDf(year = periodHotmaps) %>%
      .remap(mapIDEES, value = "hsShare")
  )



  # CALCULATE ------------------------------------------------------------------


  # extrapolate ====

  data <- dataRef %>%
    select(-"region", -"period")


  ## aggregate granular data to subsectors ====

  if (refGranularity == "sec") {
    typShares <- calcOutput("MatchingReference", subtype = "OdysseeIDEES_typ", aggregate = FALSE) %>%
      toolCountryFillAvg() %>%
      .asDf(year = periodHotmaps) %>%
      .remap(mapOdysseeIDEEStyp, value = "typShare", normaliseBy = c("region", "period")) %>%
      group_by(across(all_of("region"))) %>%
      reframe(typ = c(.data$typ, "Com"),
              typShare = c(.data$typShare, 1))
    data <- data %>%
      left_join(typShares, by = "typ", relationship = "many-to-many") %>%
      mutate(typ = case_when(.data$typ %in% c("SFH", "MFH") ~ "Res",
                             .data$typ == "Com"             ~ "Com")) %>%
      group_by(across(-all_of(c("value", "typShare")))) %>%
      summarise(value = sum(.data$value * .data$typShare), .groups = "drop")
  }


  ## rescale to regional shares ====

  data <- data %>%
    right_join(vinShares,
               by = switch(refGranularity,
                           typ = c("typ", "vin"),
                           sec = c("typ", "vin", "region")),
               relationship = "many-to-many") %>%
    left_join(hsShares, by = c("region", "period", "typ", "technology"), relationship = "many-to-many") %>%
    semi_join(hsShares, by = c("region", "period", "typ")) %>%
    replace_na(list(hsShare = 0)) %>%
    mutate(value = .data$value * .data$vinShare) %>%
    group_by(across(all_of(c("region", "period", "typ")))) %>%
    group_modify(.rescale) %>%
    ungroup() %>%
    mutate(value = .data$value / .data$vinShare) %>%
    select(-"vinShare", -"hsShare")



  # OUTPUT ---------------------------------------------------------------------

  data <- data %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(verbosity = 2)

  return(list(x = data,
              min = 0,
              max = 1,
              isocountries = FALSE,
              description = "Share of heating systems in each vintage cohort",
              unit = "1"))

}
