#' calculate the European historic building stock
#'
#' Join data from various sources to construct a consistent European building
#' stock with a focus on floor space (million m2).
#'
#' @note if the distribution of building types is missing, it is filled with the
#' Europe-wide distribution. It might be worthwhile to map filling countries
#' instead.
#'
#' @author Robin Hasse
#'
#' @param subtype Character with subsector
#' @param granularity character, name of BRICK granularity
#'
#' @returns MAgPIE object with historic building stock
#'
#' @importFrom madrat readSource toolGetMapping
#' @importFrom dplyr %>% .data filter mutate group_by across across summarise n
#'   anti_join bind_rows group_modify select full_join ungroup arrange semi_join
#'   right_join inner_join last_col relocate
#' @importFrom quitte as.quitte revalue.levels interpolate_missing_periods
#'   calc_addVariable getPeriods
#' @importFrom tidyr separate
#' @importFrom zoo rollmean
#' @importFrom stats pweibull
#' @export

calcBuildingStock <- function(subtype = c("residential", "commercial"),
                              granularity = NULL) {

  subtype <- match.arg(subtype)

  # temporal scope
  periods <- 2000:2020

  # assumptions to extrapolate vintages
  lifetimeAssumptions <- list(
    shape = c(SFH = 1.97, MFH = 1.97),
    scale = c(SFH = 100, MFH = 80) # lifetime in yr
  )



  # FUNCTIONS ------------------------------------------------------------------

  extrapolateMissingPeriods <- function(chunk, key, slopeOfLast = 5) {
    # remove NAs
    outChunk <- chunk
    chunk <- chunk[!is.na(chunk$value), ]
    upperPeriod <- max(chunk$period)
    lowerPeriod <- min(chunk$period)

    # linear regression at upper and lower end
    mUpper <- lm(value ~ period, tail(arrange(chunk, "period"), slopeOfLast))
    mLower <- lm(value ~ period, head(arrange(chunk, "period"), slopeOfLast))

    # extrapolate both ends
    outChunk[["valueUpper"]] <- predict(mUpper, newdata = outChunk["period"])
    outChunk[["valueLower"]] <- predict(mLower, newdata = outChunk["period"])

    # shift extrapolation to match last data points
    outChunk[["valueUpper"]] <- outChunk[["valueUpper"]] *
      as.numeric(outChunk[outChunk$period == upperPeriod, "value"] /
                   outChunk[outChunk$period == upperPeriod, "valueUpper"])
    outChunk[["valueLower"]] <- outChunk[["valueLower"]] *
      as.numeric(outChunk[outChunk$period == lowerPeriod, "value"] /
                   outChunk[outChunk$period == lowerPeriod, "valueLower"])
    # fill missing lower/upper ends
    outChunk[["value"]] <- ifelse(outChunk[["period"]] > max(chunk$period),
                                  outChunk[["valueUpper"]],
                                  outChunk[["value"]])
    outChunk[["value"]] <- ifelse(outChunk[["period"]] < min(chunk$period),
                                  outChunk[["valueLower"]],
                                  outChunk[["value"]])
    outChunk[["valueUpper"]] <- NULL
    outChunk[["valueLower"]] <- NULL

    return(outChunk)
  }

  # inter- and extrapolate quantities of Total, SFH and MFH
  completeQuantities <- function(df, var, periods) {
    varTotal <- paste0(var, "_Total")
    varSFH <- paste0(var, "_SFH")
    varMFH <- paste0(var, "_MFH")
    varSfhMfh <- c(varSFH, varMFH)
    vars <- c(varTotal, varSfhMfh)
    outDf <- df %>%
      filter(.data[["variable"]] == varTotal) %>%
      interpolate_missing_periods(union(periods, df$period)) %>%
      group_by(.data[["region"]]) %>%
      group_modify(extrapolateMissingPeriods) %>%
      mutate(value = pmax(0, .data[["value"]])) %>%
      anti_join(df, by = c("region", "variable", "period")) %>%
      rbind(df)

    # calculate missing total
    outDf <- outDf %>%
      group_by(across(all_of(c("region", "period")))) %>%
      filter(setequal(varSfhMfh, .data$variable)) %>%
      summarise(value = sum(.data[["value"]]),
                variable = varTotal,
                .groups = "drop") %>%
      bind_rows(outDf)

    # calculate missing variable from total and other type
    # e.g. MFH = total - SFH
    outDf <- outDf %>%
      group_by(across(all_of(c("region", "period")))) %>%
      filter(.data[["variable"]] %in% vars) %>%
      filter(n() == 2) %>%
      summarise(value =
                  .data[["value"]][.data[["variable"]] == varTotal] -
                  .data[["value"]][.data[["variable"]] != varTotal],
                variable = setdiff(vars, .data[["variable"]]),
                .groups = "drop") %>%
      bind_rows(outDf)

    dwellingTypeRatio <- outDf %>%
      select("period", "region", "variable", "value") %>%
      filter(.data[["variable"]] %in% varSfhMfh) %>%
      interpolate_missing_periods(union(periods, outDf$period))
    dwellingTypeRatio <- dwellingTypeRatio %>%
      group_by(across(all_of(c("region", "variable")))) %>%
      group_modify(extrapolateMissingPeriods) %>%
      ungroup() %>%
      full_join(interpolate_missing_periods(dwellingTypeRatio,
                                            union(periods, outDf$period),
                                            expand.values = TRUE),
                by = c("period", "region", "variable")) %>%
      group_by(across(all_of(c("region", "variable")))) %>%
      mutate(value = if (any(.data[["value.x"]] < 0)) .data[["value.y"]] else .data[["value.x"]]) %>%
      select(-"value.x", -"value.y")
    dwellingTypeRatio <- dwellingTypeRatio %>%
      group_by(across(all_of(c("period", "variable")))) %>%
      summarise(value = sum(.data[["value"]]),
                region = setdiff(outDf$region, dwellingTypeRatio$region),
                .groups = "drop") %>%
      rbind(dwellingTypeRatio) %>%
      group_by(across(all_of(c("period", "region")))) %>%
      mutate(value = proportions(.data[["value"]]),
             variable = gsub(var, "dwellingRatio", .data[["variable"]]))
    outDf <- outDf %>%
      filter(.data[["variable"]] == varTotal) %>%
      select("period", "region", "variable", "value") %>%
      rbind(dwellingTypeRatio) %>%
      calc_addVariable(
        .variable_SFH = paste(varTotal, "*", "dwellingRatio_SFH"),
        .variable_MFH = paste(varTotal, "*", "dwellingRatio_MFH")
      ) %>%
      mutate(variable = gsub("\\.variable", var, .data[["variable"]])) %>%
      filter(!grepl("^dwellingRatio_(SFH|MFH)$", .data[["variable"]]))

    return(outDf)
  }


  # residual vintage quantity based on Weibull demolition curve
  residualVintage <- function(nRef, tRef, t, tCon, type) {
    shape <- lifetimeAssumptions$shape[[type]]
    scale <- lifetimeAssumptions$scale[[type]]
    nRef * (1 - pweibull(t - tCon, shape, scale)) / (1 - pweibull(tRef - tCon, shape, scale))
  }

  # left join total stock by region, period and buildingType
  addStockColumn <- function(df, stockDf) {
    df %>% left_join(
      stockDf %>%
        filter(grepl("^(SFH|MFH)$", .data[["buildingType"]])) %>%
        group_by(across(all_of(c("region", "period", "buildingType")))) %>%
        summarise(stock = sum(.data[["value"]]), .groups = "drop"),
      by = c("region", "period", "buildingType")
    )
  }

  # super position of two values x and y
  supPos <- function(x, y, w) {
    ifelse(w <= 0, y, ifelse(w >= 1, x, w * x + (1 - w) * y))
  }

  # rollmean with filled ends
  completeRollmean <- function(data, k) {
    data %>%
      arrange(.data[["period"]]) %>%
      mutate(value = unlist(rollmean(
        .data[["value"]], k = k, fill = c(head(.data[["value"]], 1),
                                          mean(.data[["value"]]),
                                          tail(.data[["value"]], 1))
      )))
  }


  # READ DATA ------------------------------------------------------------------

  # Odyssee database (EU+)
  odyssee <- readSource("Odyssee") %>%
    as.quitte() %>%
    filter(!is.na(.data[["value"]])) %>%
    mutate(region = droplevels(.data[["region"]]))

  # EU Buildings Database
  eubdb <- rbind(as.quitte(readSource("EUBuildingsDB", "BuildingStockCharacteristics")),
                 as.quitte(readSource("EUBuildingsDB", "TechnicalBuildingSystems"))) %>%
    filter(!is.na(.data[["value"]])) %>%
    mutate(region = droplevels(.data[["region"]]))

  # JRC's IDEES data base
  idees <- readSource("JRC_IDEES", switch(subtype,
                                          residential = "Residential",
                                          commercial = "Tertiary")) %>%
    as.quitte() %>%
    filter(!is.na(.data[["value"]])) %>%
    mutate(region = droplevels(.data[["region"]]))

  # Eurostat energy balance
  eurostat <- readSource("EurostatBuildings", "nrg_d_hhq") %>%
    as.quitte() %>%
    filter(!is.na(.data[["value"]])) %>%
    mutate(region = droplevels(.data[["region"]])) %>%
    rename(carrier = "siec")

  # Eurostat heat pump data
  hpEurostat <- readSource("EurostatBuildings", "nrg_inf_hptc") %>%
    as.quitte() %>%
    filter(!is.na(.data[["value"]])) %>%
    mutate(region = droplevels(.data[["region"]])) %>%
    select(-"variable") %>%
    rename(variable = "plant_tec",
           hpTech   = "hp_tech")

  # EurObservER
  hpBarometer <- readSource("EurObservER", "heatPumpsBarometer.stock") %>%
    as.quitte() %>%
    filter(!is.na(.data[["value"]])) %>%
    mutate(region = droplevels(.data[["region"]]))

  switch(subtype,
    residential = {

      # EU census data on building type an vintage of dwellings (not used)
      if (FALSE) {
        typeVintage <- readSource("CensusHub", "typeVintage") %>% # nolint: object_usage_linter.
          collapseDim(keepdim = "period") %>%
          as.quitte() %>%
          filter(!is.na(.data[["value"]]),
                 !.data[["buildingType"]] %in% c("residential", "nonresidential",
                                                 "total", "unknown"),
                 !.data[["constructionPeriod"]] %in% c("total", "unknown")) %>%
          revalue.levels(buildingType = c(residential_1dwelling   = "SFH",
                                          residential_2dwelling   = "SFH",
                                          residential_ge3dwelling = "MFH")) %>%
          mutate(constructionPeriod = factor(
            .data[["constructionPeriod"]],
            levels = c("before1919", "1919-1945", "1946-1960", "1961-1970", "1971-1980",
                       "1981-1990", "1991-2000", "2001-2005", "after2005")
          )) %>%
          group_by(across(all_of(c("period", "region", "constructionPeriod", "buildingType")))) %>%
          summarise(value = sum(.data[["value"]]), .groups = "drop")
      }



      # EXTRACT DATA -----------------------------------------------------------

      # relevant variables
      varsOdyssee <- c(
        surlog = "floorPerDwelling_Total",
        surmpr = "floorPerDwelling_SFH",
        suripr = "floorPerDwelling_MFH",
        nbrlpr = "dwellings_Total",
        nbrmpr = "dwellings_SFH",
        nbripr = "dwellings_MFH"
      )
      varsStockEubdb <- c(
        `Average floor area of permanently occupied dwellings` = "floorPerDwelling_Total",
        `Total floor area of single family dwellings` = "floor_SFH",
        `Total floor area of multi family dwellings`  = "floor_MFH",
        `Number of permanently occupied dwelling`               = "dwellings_Total",
        `Number of single family dwelling permanently occupied` = "dwellings_SFH",
        `Number of multi family dwelling permanently occupied`  = "dwellings_MFH"
      )
      varsVintagesEubdb <- c(
        "Share of dwellings built before 1945",
        "Share of dwellings built between 1945 and 1969",
        "Share of dwellings built between 1970 and 1979",
        "Share of dwellings built between 1980 and 1989",
        "Share of dwellings built between 1990 and 1999",
        "Share of dwellings built between 2000 and 2010",
        "Share of dwellings built after 2010"
      )
      varsConstructionOdyssee <- c(
        nbrlpn = "construction_Total",
        nbrmpn = "construction_SFH",
        nbripn = "construction_MFH"
      )
      varsLocationEubdb <- c(
        `Share of dwellings in densely-populated area (urban centre)`       = "dwellings_urban",
        `Share of dwellings in intermediate urbanised area (urban cluster)` = "dwellings_urban",
        `Share of dwellings in thinly-populated area (rural)`               = "dwellings_rural"
      )
      varsHeatingOdyssee <- c(
        nbrlprpet = "dwellings_oil",
        nbrlprgaz = "dwellings_gas",
        nbrlprcms = "dwellings_coal",
        nbrlprvap = "dwellings_heat",
        nbrlprboi = "dwellings_biomod",
        nbrlprele = "dwellings_elec"
      )
      varsHeatingIdees <- c(
        `Residential|Stock of households|Space heating|Solids`                              = "dwellings_coal.1",
        `Residential|Stock of households|Space heating|Liquified petroleum gas (LPG)`       = "dwellings_gas.1",
        `Residential|Stock of households|Space heating|Gas/Diesel oil incl_ biofuels (GDO)` = "dwellings_oil.1",
        `Residential|Stock of households|Space heating|Gases incl_ biogas`                  = "dwellings_gas.1",
        `Residential|Stock of households|Space heating|Biomass and wastes`                  = "dwellings_biomod.1",
        `Residential|Stock of households|Space heating|Geothermal energy`                   = "dwellings_geothermal.1",
        `Residential|Stock of households|Space heating|Derived heat`                        = "dwellings_heat.1",
        `Residential|Stock of households|Space heating|Advanced electric heating`           = "dwellings_heatpump.1",
        `Residential|Stock of households|Space heating|Conventional electric heating`       = "dwellings_resistElec.1"
      )


      # filter variables
      stockOdyssee <- odyssee %>%
        filter(.data[["variable"]] %in% names(varsOdyssee)) %>%
        revalue.levels(variable = varsOdyssee)
      stockEubdb <- eubdb %>%
        filter(.data[["variable"]] %in% names(varsStockEubdb)) %>%
        revalue.levels(variable = varsStockEubdb)
      vintagesEubdb <- eubdb %>%
        filter(.data[["variable"]] %in% varsVintagesEubdb) %>%
        mutate(variable = gsub("Share of dwellings built (between |)| |_1$", "",
                               sub(" and ", "-", .data[["variable"]])))
      constructionOdyssee <- odyssee %>%
        filter(.data[["variable"]] %in% names(varsConstructionOdyssee)) %>%
        revalue.levels(variable = varsConstructionOdyssee)
      locationEubdb <- eubdb %>%
        filter(.data[["variable"]] %in% names(varsLocationEubdb)) %>%
        revalue.levels(variable = varsLocationEubdb) %>%
        group_by(across(all_of(c("region", "period", "variable", "unit")))) %>%
        summarise(value = sum(.data[["value"]]), .groups = "drop")
      heatingOdyssee <- odyssee %>%
        filter(.data[["variable"]] %in% names(varsHeatingOdyssee)) %>%
        revalue.levels(variable = varsHeatingOdyssee) %>%
        separate("variable", c("variable", "carrier"), sep = "_")
      heatingIdees <- idees %>%
        filter(.data[["variable"]] %in% names(varsHeatingIdees)) %>%
        revalue.levels(variable = varsHeatingIdees) %>%
        separate("variable", c("variable", "unit"), sep = "\\.") %>%
        separate("variable", c("variable", "carrier"), sep = "_")
      carrier <- NULL # remove this and find SE formulation
      heatingEurostat <- eurostat %>%
        filter(.data[["nrg_bal"]] == "FC_OTH_HH_E_SH") %>%
        select(-"model", -"scenario", -"nrg_bal", -"variable") %>%
        group_by(across(all_of(c("region", "period", "carrier")))) %>%
        filter(all(.data[["unit"]] != "TJ") | .data[["unit"]] == "TJ") %>%
        ungroup() %>%
        mutate(unit = as.character(.data[["unit"]]),
               value = .data[["value"]] * ifelse(.data[["unit"]] == "THS_T" &
                                                   .data[["carrier"]] == "O4000",
                                                 41.868, 1),
               unit = ifelse(.data[["unit"]] == "THS_T" &
                               .data[["carrier"]] == "O4000",
                             "TJ", .data[["unit"]])) %>%
        filter(.data[["unit"]] == "TJ") %>%
        calc_addVariable(
          coal         = "SFF_P1000_S2000",
          oil          = "O4000",
          gas          = "G3000 + R5300",
          biomod       = "`R5110-5150_W6000RI`",
          solarThermal = "RA410",
          ambientHeat  = "RA600",
          otherRE      = "RA000 - RA410 - `R5110-5150_W6000RI` - R5300 - RA600",
          heat         = "H8000",
          elec         = "E7000",
          variable = carrier, only.new = TRUE, units = "TJ", completeMissing = TRUE
        )



      # COMBINE DATA -----------------------------------------------------------


      ## Tidy EU Buildings DB ====

      varsDwellings <- paste0("dwellings_", c("Total", "SFH", "MFH"))
      stockEubdb <- stockEubdb %>%
        group_by(across(all_of(c("region", "period")))) %>%
        filter(.data[["variable"]] %in% varsDwellings) %>%
        filter(n() == 2) %>%
        summarise(value =
                    .data[["value"]][.data[["variable"]] == "dwellings_Total"] -
                    .data[["value"]][.data[["variable"]] != "dwellings_Total"],
                  unit = .data[["unit"]][.data[["variable"]] == "dwellings_Total"],
                  variable = setdiff(varsDwellings, .data[["variable"]]),
                  .groups = "drop") %>%
        bind_rows(stockEubdb)


      ## Dwelling quantities ====

      stock <- stockEubdb %>%
        filter(grepl("^dwellings_(SFH|MFH)$", .data[["variable"]])) %>%
        anti_join(stockOdyssee, by = c("period", "region", "variable")) %>%
        rbind(stockOdyssee) %>%
        filter(grepl("^dwellings_(Total|SFH|MFH)$", .data[["variable"]])) %>%
        completeQuantities("dwellings", periods) %>%
        mutate(unit = "1") %>%
        separate("variable", c("variable", "buildingType"))



      ## Location ====

      # constant extrapolation of EUBDB location data (last time step 2018)
      # moving average to smooth jumps
      locationEubdb <- locationEubdb %>%
        interpolate_missing_periods(periods, expand.values = TRUE) %>%
        group_by(across(all_of(c("region", "variable")))) %>%
        completeRollmean(5)

      # fill missing regions with dwelling stock-weighted average
      locationEubdb <- stock %>%
        group_by(across(all_of(c("region", "period")))) %>%
        summarise(dwellings = sum(.data[["value"]]), .groups = "drop") %>%
        right_join(locationEubdb, by = c("region", "period")) %>%
        group_by(across(all_of(c("period", "variable")))) %>%
        summarise(value = sum(.data[["value"]] * proportions(.data[["dwellings"]])),
                  region = setdiff(stock$region, locationEubdb$region),
                  unit = "1",
                  .groups = "drop") %>%
        rbind(locationEubdb) %>%
        mutate(location = gsub("dwellings_", "", .data[["variable"]])) %>%
        select(-"variable")

      # apply location shares to stock
      stock <- stock %>%
        left_join(locationEubdb, by = c("period", "region")) %>%
        mutate(value = .data[["value.x"]] * .data[["value.y"]],
               unit = .data[["unit.x"]]) %>%
        select(-"value.x", -"value.y", -"unit.x", -"unit.y") %>%
        filter(!is.na(.data[["value"]]))



      ## Vintages ====

      construction <- constructionOdyssee %>%
        completeQuantities("construction", periods) %>%
        filter(grepl("^construction_(SFH|MFH)$", .data[["variable"]])) %>%
        mutate(buildingType = gsub("^construction_", "", .data[["variable"]]))
      dwellingVintages <- vintagesEubdb %>%
        select(-"model", -"scenario", -"unit") %>%
        left_join(stock %>%
                    filter(.data[["buildingType"]] %in% c("SFH", "MFH")) %>%
                    group_by(across(all_of(c("period", "region", "buildingType",
                                             "unit")))) %>%
                    summarise(value = sum(.data[["value"]]), .groups = "drop"),
                  by = c("period", "region")) %>%
        mutate(value = .data[["value.x"]] * .data[["value.y"]]) %>%
        select("region", "period", "buildingType", "variable", "value")

      ### forward extrapolation ####

      # accumulate construction to extrapolate 'after2010' cohort
      dwellingVintages <- dwellingVintages %>%
        group_by(across(all_of(c("region", "variable", "buildingType")))) %>%
        filter(.data[["period"]] == max(.data[["period"]]),
               .data[["variable"]] == "after2010") %>%
        ungroup() %>%
        full_join(select(construction, -"variable"),
                  by = c("region", "period", "buildingType")) %>%
        group_by(across(all_of(c("region", "buildingType")))) %>%
        semi_join(dwellingVintages, by = c("region", "buildingType")) %>%
        filter(.data[["period"]] >= .data[["period"]][!is.na(.data[["value.x"]])]) %>%
        arrange(.data[["period"]]) %>%
        mutate(value = head(.data[["value.x"]], 1) +
                 cumsum(c(0, head(.data[["value.y"]], -1))),
               variable = "after2010") %>%
        anti_join(dwellingVintages,
                  by = c("region", "period", "variable", "buildingType")) %>%
        select(-"value.x", -"value.y") %>%
        rbind(dwellingVintages)

      # extrapolate closed cohorts with Weibull demolition approach
      dwellingVintages <- dwellingVintages %>%
        group_by(across(all_of(c("region", "variable", "buildingType")))) %>%
        filter(.data[["period"]] == max(.data[["period"]]),
               .data[["variable"]] != "after2010") %>%
        complete(period = max(.data[["period"]]):max(periods)) %>%
        arrange(.data[["period"]]) %>%
        mutate(
          constructionPeriod = unlist(lapply(
            gsub("before", "1900-", .data[["variable"]]),
            function(x) mean(as.numeric(unlist(strsplit(x, "-"))))
          )),
          value = residualVintage(head(.data[["value"]], 1),
                                  head(.data[["period"]], 1),
                                  .data[["period"]],
                                  .data[["constructionPeriod"]],
                                  unique(.data[["buildingType"]]))
        ) %>%
        ungroup() %>%
        select(-"constructionPeriod") %>%
        anti_join(dwellingVintages,
                  by = c("period", "region", "buildingType", "variable")) %>%
        bind_rows(dwellingVintages)

      # fill missing shares with closed cohorts then current cohort
      dwellingVintages <- vintagesEubdb %>%
        group_by(across(all_of(c("region", "variable")))) %>%
        summarise(lastPeriod = max(.data[["period"]]), .groups = "drop") %>%
        select("region", "variable", "lastPeriod") %>%
        right_join(dwellingVintages, by = c("region", "variable")) %>%
        filter(.data[["period"]] >= .data[["lastPeriod"]]) %>%
        addStockColumn(stock) %>%
        group_by(across(all_of(c("region", "period", "buildingType")))) %>%
        mutate(relGap = ifelse(.data[["period"]] > .data[["lastPeriod"]],
                               (.data[["stock"]] - sum(.data[["value"]])) /
                                 sum(.data[["value"]][.data[["variable"]] != "after2010"]),
                               0)) %>%
        group_by(across(all_of(c("region", "variable", "buildingType")))) %>%
        arrange(.data[["period"]]) %>%
        mutate(value = ifelse(
          .data[["period"]] > .data[["lastPeriod"]] &
            .data[["variable"]] != "after2010",
          cummin((1 + .data[["relGap"]]) * .data[["value"]]),
          .data[["value"]]
        )) %>%
        group_by(across(all_of(c("region", "period", "buildingType")))) %>%
        mutate(value = ifelse(.data[["variable"]] == "after2010",
                              .data[["value"]] + .data[["stock"]] -
                                sum(.data[["value"]]),
                              .data[["value"]])) %>%
        group_by(across(all_of(c("region", "variable", "buildingType")))) %>%
        arrange(.data[["period"]]) %>%
        mutate(value = ifelse(
          .data[["period"]] > .data[["lastPeriod"]] &
            .data[["variable"]] == "after2010",
          cummax(.data[["value"]]),
          .data[["value"]]
        )) %>%
        select(-"lastPeriod", -"stock", -"relGap") %>%
        anti_join(dwellingVintages, by = c("region", "period", "variable", "buildingType")) %>%
        rbind(dwellingVintages)

      ### backwards extrapolation ####

      dwellingVintagesEarly <- dwellingVintages %>%
        group_by(across(all_of(c("region")))) %>%
        filter(.data[["period"]] == min(.data[["period"]]))

      # extrapolate with Weibull demolition approach
      dwellingVintagesEarly <- dwellingVintagesEarly %>%
        group_by(across(all_of(c("region", "variable", "buildingType")))) %>%
        complete(period = min(periods):max(.data[["period"]])) %>%
        arrange(.data[["period"]]) %>%
        mutate(
          constructionPeriod = unlist(lapply(
            gsub("before", "1900-",
                 gsub("after2010", "2010-2020", .data[["variable"]])),
            function(x) mean(as.numeric(unlist(strsplit(x, "-"))))
          )),
          value = residualVintage(tail(.data[["value"]], 1),
                                  tail(.data[["period"]], 1),
                                  .data[["period"]],
                                  .data[["constructionPeriod"]],
                                  unique(.data[["buildingType"]]))
        ) %>%
        ungroup() %>%
        select(-"constructionPeriod")

      # assume linear increase for open cohorts
      dwellingVintagesEarly <- dwellingVintagesEarly %>%
        group_by(across(all_of(c("region", "buildingType", "variable")))) %>%
        mutate(
          firstPeriod = max(.data[["period"]]),
          value = ifelse(
            .data[["variable"]] == "after2010",
            pmax(0, .data[["value"]][.data[["period"]] == .data[["firstPeriod"]]] *
                   (.data[["period"]] - 2009) / (.data[["firstPeriod"]] - 2009)),
            .data[["value"]]
          ),
          value = ifelse(
            .data[["variable"]] == "2000-2010" & .data[["period"]] < 2010,
            pmax(0, .data[["value"]][.data[["period"]] == min(2010, .data[["firstPeriod"]], na.rm = TRUE)] *
                   (.data[["period"]] - 1999) / (min(2010, .data[["firstPeriod"]], na.rm = TRUE) - 1999)),
            .data[["value"]]
          ),
          value = ifelse(.data[["variable"]] == "after2010" & .data[["period"]] <= 2010,
                         0,
                         .data[["value"]])
        )

      # scale total
      dwellingVintagesEarly <- dwellingVintagesEarly %>%
        addStockColumn(stock) %>%
        group_by(across(all_of(c("region", "period", "buildingType")))) %>%
        mutate(relGap = (.data[["stock"]] - sum(.data[["value"]])) /
                 sum(.data[["value"]])) %>%
        group_by(across(all_of(c("region", "variable", "buildingType")))) %>%
        arrange(-.data[["period"]]) %>%
        mutate(value = ifelse((.data[["variable"]] == "after2010" & .data[["period"]] > 2010) |
                                (.data[["variable"]] == "2000-2010" & .data[["period"]] %in% 2000:2010),
                              cummin((1 + .data[["relGap"]]) * .data[["value"]]),
                              cummax((1 + .data[["relGap"]]) * .data[["value"]])),
               value = ifelse(.data[["variable"]] == "after2010" & .data[["period"]] <= 2010,
                              0,
                              .data[["value"]])) %>%
        select(-"stock", -"relGap", -"firstPeriod")

      ### apply shares ####

      # join backwards extrapolation
      dwellingVintages <- dwellingVintagesEarly %>%
        anti_join(dwellingVintages, by = c("region", "period", "variable", "buildingType")) %>%
        rbind(dwellingVintages)

      # calculate shares (assume global average for missing regions)
      vintageShares <- dwellingVintages %>%
        group_by(across(all_of(c("period", "buildingType", "variable")))) %>%
        summarise(value = sum(.data[["value"]]),
                  region = setdiff(stock$region, dwellingVintages$region),
                  .groups = "drop") %>%
        rbind(dwellingVintages) %>%
        group_by(across(all_of(c("region", "period", "buildingType")))) %>%
        mutate(value = proportions(.data[["value"]]),
               unit = "1")

      # apply vintage shares to stock
      stock <- stock %>%
        filter(.data[["buildingType"]] != "Total") %>%
        left_join(vintageShares, by = c("region", "period", "buildingType")) %>%
        rename(unit = "unit.y",
               variable = "variable.x",
               vintage = "variable.y") %>%
        mutate(value = .data[["value.x"]] * .data[["value.y"]]) %>%
        select(-"value.x", -"value.y", -"unit.x") %>%
        filter(!is.na(.data[["value"]]))



      ## Heating System ====

      ### clean data source ####

      # Eurostat
      feDemHeating <- heatingEurostat %>%
        filter(!.data[["carrier"]] %in% c("ambientHeat", "solarThermal")) %>%
        mutate(value = pmax(0, .data[["value"]]),
               carrier = gsub("otherRE", "biomod", .data[["carrier"]])) %>%
        group_by(across(all_of(c("region", "period", "carrier", "unit")))) %>%
        summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
        group_by(across(all_of(c("region", "period")))) %>%
        filter(any(.data[["value"]] > 0)) %>%
        ungroup()

      # Odyssee
      dwellingHeating <- heatingOdyssee %>%
        select(-"model", -"scenario", -"unit") %>%
        group_by(across(all_of(c("region", "period")))) %>%
        complete(carrier = unique(heatingOdyssee$carrier),
                 fill = list(variable = "dwellings")) %>%
        ungroup() %>%
        interpolate_missing_periods(union(periods, heatingOdyssee$period))


      ### number of dwellings ####

      # Remove all region-period combinations with missing carriers
      dwellingHeating <- dwellingHeating %>%
        group_by(across(all_of(c("region", "period")))) %>%
        filter(!any(is.na(.data[["value"]])))

      # fill with IDEES numbers until 2015
      smoothPeriods <- 3
      dwellingHeating <- heatingIdees %>%
        filter(.data[["carrier"]] != "geothermal") %>%
        mutate(carrier = gsub("heatpump|resistElec", "elec", .data[["carrier"]])) %>%
        group_by(across(all_of(c("region", "period", "carrier")))) %>%
        summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
        full_join(dwellingHeating, by = c("region", "period", "carrier")) %>%
        group_by(across(all_of(c("region", "carrier")))) %>%
        mutate(
          lastPeriod  = suppressWarnings(max(.data[["period"]][!is.na(.data[["value.y"]])])),
          firstPeriod = suppressWarnings(min(.data[["period"]][!is.na(.data[["value.y"]])])),
          value = ifelse(
            .data[["lastPeriod"]] >= 2015,  # no filling with IDEES
            .data[["value.y"]],
            ifelse(
              .data[["lastPeriod"]] > 2010,  # extrapolate last period a.r.t. rel. changes in IDEES
              .data[["value.y"]] *
                (.data[["value.x"]] / .data[["value.y"]])[.data[["period"]] == .data[["lastPeriod"]]],
              ifelse(
                .data[["lastPeriod"]] >= 2000,  # smooth transition to IDEES
                supPos(.data[["value.x"]], .data[["value.y"]],
                       (.data[["period"]] - pmax(.data[["firstPeriod"]] - 1,
                                                 .data[["lastPeriod"]] + 1 - smoothPeriods)) /
                         pmin(smoothPeriods, .data[["lastPeriod"]] - .data[["firstPeriod"]] + 2)),
                .data[["value.x"]]
              )
            )
          ),
          value = ifelse(is.na(.data[["value.x"]]), .data[["value.y"]], .data[["value"]])
        ) %>%
        select(-"value.x", -"value.y", -"firstPeriod", -"lastPeriod")

      # extrapolate from last period to 2020 based on rel. changes in Eurostat balance
      # this neglects efficiency changes (most relevant for heat pumps)
      dwellingHeating <- feDemHeating %>%
        interpolate_missing_periods(union(periods, dwellingHeating$period),
                                    expand.values = TRUE) %>%
        mutate(value = .data[["value"]] + 1E-3) %>%
        full_join(dwellingHeating, by = c("region", "period", "carrier")) %>%
        filter(.data[["region"]] %in% dwellingHeating$region) %>%
        group_by(across(all_of(c("region", "carrier")))) %>%
        mutate(
          lastPeriod  = suppressWarnings(max(.data[["period"]][!is.na(.data[["value.y"]])])),
          firstPeriod = suppressWarnings(min(.data[["period"]][!is.na(.data[["value.y"]])])),
          value = ifelse(
            .data[["period"]] > .data[["lastPeriod"]],
            .data[["value.x"]] *
              (.data[["value.y"]] / .data[["value.x"]])[.data[["period"]] == .data[["lastPeriod"]]],
            ifelse(
              .data[["period"]] < .data[["firstPeriod"]],
              .data[["value.x"]] *
                (.data[["value.y"]] / .data[["value.x"]])[.data[["period"]] == .data[["firstPeriod"]]],
              .data[["value.y"]]
            )
          )
        ) %>%
        ungroup() %>%
        select(-"value.x", -"value.y", -"firstPeriod", -"lastPeriod", -"unit",
               -"variable")

      ### apply shares ####

      # assume average for missing regions (currently none)
      dwellingHeating <- dwellingHeating %>%
        interpolate_missing_periods(unique(dwellingHeating$period),
                                    expand.values = TRUE)
      heatingShares <- dwellingHeating %>%
        group_by(across(all_of(c("period", "carrier")))) %>%
        summarise(region = setdiff(stock$region, dwellingHeating$region),
                  value = sum(.data[["value"]]),
                  .groups = "drop") %>%
        rbind(dwellingHeating) %>%
        group_by(across(all_of(c("region", "period")))) %>%
        mutate(value = proportions(.data[["value"]]))
      stock <- stock %>%
        left_join(heatingShares, by = c("region", "period"),
                  relationship = "many-to-many") %>%
        mutate(value = .data[["value.x"]] * .data[["value.y"]]) %>%
        select(-"value.x", -"value.y") %>%
        rename(heating = "carrier")


      ### heat pumps ####

      # dwellings with HPs and resist. elec. heating until 2015
      heatPumps <- heatingIdees %>%
        filter(.data[["carrier"]] %in% c("heatpump", "resistElec"),
               !.data[["region"]] %in% c("EST", "LTU", "LVA")) %>%  # missing HP numbers
        select(-"model", -"scenario", -"unit")

      # extrapolate heat pump barometer (exponential growth with avg rate)
      # and fill missing regions with total sum
      hpNumber <- hpBarometer %>%
        filter(.data[["variable"]] == "Total")  %>%
        group_by(.data[["region"]]) %>%
        arrange(.data[["period"]]) %>%
        filter(c(diff(.data[["value"]]), 0) / .data[["value"]] > -0.1) %>%
        mutate(firstPeriod = min(.data[["period"]])) %>%
        ungroup() %>%
        interpolate_missing_periods(periods) %>%
        group_by(.data[["region"]]) %>%
        arrange(.data[["period"]]) %>%
        mutate(growth = mean(diff(.data[["value"]]) /
                               diff(.data[["period"]]) /
                               head(.data[["value"]], -1),
                             na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(growth = replace_na(.data[["growth"]],
                                   mean(.data[["growth"]], na.rm = TRUE))) %>%
        group_by(.data[["region"]]) %>%
        mutate(value = ifelse(is.na(.data[["value"]]),
                              .data[["value"]][.data[["period"]] == .data[["firstPeriod"]]] *
                                (1 + .data[["growth"]])^(.data[["period"]] - .data[["firstPeriod"]]),
                              .data[["value"]])) %>%
        ungroup() %>%
        select("region", "period", hpBarometer = "value")
      hpNumber <- hpNumber %>%
        group_by(.data[["period"]]) %>%
        summarise(hpBarometer = sum(.data[["hpBarometer"]], na.rm = TRUE),
                  region = setdiff(heatPumps$region, hpNumber$region),
                  .groups = "drop") %>%
        rbind(hpNumber)

      # extrapolate IDEES such that total follows stock and heat pumps follow
      # heat pumps barometer
      heatPumps <- heatPumps %>%
        group_by(.data[["region"]]) %>%
        filter(.data[["period"]] == max(.data[["period"]])) %>%
        ungroup() %>%
        spread("carrier", "value") %>%
        full_join(hpNumber, by = c("region", "period")) %>%
        full_join(stock %>%
                    filter(.data[["heating"]] == "elec") %>%
                    group_by(across(all_of(c("region", "period")))) %>%
                    summarise(elecStock = sum(.data[["value"]]), .groups = "drop"),
                  by = c("region", "period")) %>%
        filter(.data[["region"]] %in% heatPumps$region) %>%
        group_by(.data[["region"]]) %>%
        mutate(
          heatpump = .data[["hpBarometer"]] *
            (.data[["heatpump"]] / .data[["hpBarometer"]])[!is.na(.data[["heatpump"]])],
          resistElec = .data[["elecStock"]] *
            ((.data[["resistElec"]] + .data[["heatpump"]]) / .data[["elecStock"]]
            )[!is.na(.data[["resistElec"]])] -
            .data[["heatpump"]],
          variable = "dwellings"
        ) %>%
        gather("carrier", "value", "resistElec", "heatpump") %>%
        select(-"elecStock", -"hpBarometer") %>%
        mutate(value = pmax(0, .data[["value"]]))

      # average efficiency of HP in EU member states
      scopEurostat <- hpEurostat %>%
        select(-"unit", -"model", -"scenario") %>%
        group_by(across(all_of(c("region", "period", "hpTech")))) %>%
        filter("SPERF_FACT" %in% .data$variable) %>%
        filter(.data$value[.data$variable == "SPERF_FACT"] > 0) %>%
        ungroup()
      scopEurostat <- scopEurostat %>%
        group_by(across(all_of(c("period", "variable", "hpTech")))) %>%
        summarise(value = ifelse(all(.data[["variable"]] == "CAP_HEAT"),
                                 sum(.data[["value"]]),
                                 mean(.data[["value"]][.data[["value"]] > 0])),
                  region = c(setdiff(stock$region, scopEurostat$region), "GLO"),
                  .groups = "drop") %>%
        rbind(scopEurostat)
      scopEurostat <- scopEurostat %>%
        spread("variable", "value") %>%
        group_by(across(all_of(c("region", "period")))) %>%
        summarise(scop = 1 / sum(proportions(.data[["CAP_HEAT"]]) / .data[["SPERF_FACT"]]),
                  .groups = "drop") %>%
        select("period", "region", "scop")
      scopEurostat <- scopEurostat %>%
        filter(.data[["region"]] != "GLO") %>%
        group_by(.data[["region"]]) %>%
        complete(period = unique(scopEurostat$period)) %>%
        full_join(scopEurostat %>%
                    filter(.data[["region"]] == "GLO") %>%
                    select(-"region"),
                  by = c("period")) %>%
        arrange(.data[["period"]]) %>%
        mutate(scop = ifelse(is.na(.data$scop.x),
                             .data$scop.y * head((.data$scop.x / .data$scop.y)[!is.na(.data$scop.x)], 1),
                             .data$scop.x),
               scop = ifelse(is.na(.data$scop) | .data$scop < 2,
                             .data$scop.y,
                             .data$scop)) %>%
        select(-"scop.x", -"scop.y")

      # estimate UE share of HPs in electric space heating
      effResistElec <- 0.85
      hpShareElecHeating <- heatingEurostat %>%
        filter(.data[["carrier"]] %in% c("ambientHeat", "elec"),
               .data[["value"]] > 0) %>%
        select(-"unit") %>%
        interpolate_missing_periods(unique(scopEurostat$period)) %>%
        spread("carrier", "value") %>%
        full_join(scopEurostat, by = c("region", "period")) %>%
        mutate(value = 1 / (.data[["elec"]] / .data[["ambientHeat"]] *
                              (effResistElec - effResistElec / .data[["scop"]]) -
                              effResistElec / .data[["scop"]] + 1),
               value = pmin(1, .data[["value"]])) %>%
        select(-"elec", -"ambientHeat", -"scop")
      hpShareElecHeating <- hpShareElecHeating %>%
        group_by(.data[["period"]]) %>%
        summarise(avgValue = mean(.data[["value"]], na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(.data[["period"]]) %>%
        mutate(growth = mean(diff(.data$avgValue) / diff(.data$period) / head(.data$avgValue, -1), na.rm = TRUE),
               firstPeriod = min(.data[["period"]][!is.na(.data[["avgValue"]])]),
               avgValue = ifelse(
                 is.na(.data[["avgValue"]]),
                 .data[["avgValue"]][.data[["period"]] == .data[["firstPeriod"]]] *
                   (1 + .data[["growth"]])^(.data[["period"]] - .data[["firstPeriod"]]),
                 .data[["avgValue"]]
               )) %>%
        full_join(hpShareElecHeating, by = "period") %>%
        group_by(.data[["region"]]) %>%
        arrange(.data[["period"]]) %>%
        mutate(value = ifelse(
          is.na(.data[["value"]]),
          .data[["avgValue"]] *
            ifelse(all(is.na(.data[["value"]])),
                   1,
                   head((.data[["value"]] / .data[["avgValue"]])[!is.na(.data[["value"]])], 1)),
          .data[["value"]]
        )) %>%
        select(-"growth", -"firstPeriod", -"avgValue") %>%
        completeRollmean(3)

      # share of HPs in electric heating
      # if extrapolated IDEES numbers match stock well enough, we use this
      # otherwise, we fall back to the Eurostat energy balance estimate
      regionsMatchingIDEES <- heatPumps %>%
        group_by(across(all_of(c("region", "period")))) %>%
        summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
        inner_join(stock %>%
                     filter(.data[["heating"]] == "elec") %>%
                     group_by(across(all_of(c("region", "period")))) %>%
                     summarise(value = sum(.data[["value"]]), .groups = "drop"),
                   by = c("region", "period")) %>%
        filter(.data[["period"]] == 2015,
               abs(.data[["value.x"]] - .data[["value.y"]]) / .data[["value.y"]] < 0.25) %>%
        getElement("region")
      hpShare <- heatPumps %>%
        filter(.data[["region"]] %in% regionsMatchingIDEES) %>%
        group_by(across(all_of(c("region", "period")))) %>%
        mutate(value = proportions(.data[["value"]]),
               variable = "share") %>%
        group_by(.data[["region"]]) %>%
        completeRollmean(3)
      hpShare <- hpShareElecHeating %>%
        filter(!.data[["region"]] %in% regionsMatchingIDEES) %>%
        rename(heatpump = "value") %>%
        mutate(resistElec = 1 - .data[["heatpump"]],
               variable = "share") %>%
        gather("carrier", "value", "heatpump", "resistElec") %>%
        rbind(hpShare)

      # split stock of electric heating -> heat pumps + resistive electric
      stock <- stock %>%
        filter(.data[["heating"]] == "elec") %>%
        left_join(select(hpShare, -"variable"), by = c("region", "period")) %>%
        mutate(value = .data[["value.x"]] * .data[["value.y"]],
               heating = .data[["carrier"]]) %>%
        select(-"value.x", -"value.y", -"carrier") %>%
        rbind(filter(stock, .data[["heating"]] != "elec"))


      ## Dwelling size (m2) ====

      # floor space per dwelling
      dwellingSize <- stockOdyssee %>%
        filter(grepl("^floorPerDwelling", .data[["variable"]])) %>%
        select(-"model", -"scenario")
      dwellingSize <- dwellingSize %>%
        filter(.data[["variable"]] == "floorPerDwelling_Total") %>%
        interpolate_missing_periods(unique(stock$period)) %>%
        group_by(.data[["region"]]) %>%
        group_modify(extrapolateMissingPeriods) %>%
        ungroup() %>%
        anti_join(dwellingSize, by = c("region", "period", "variable")) %>%
        rbind(dwellingSize) %>%
        group_by(across(all_of(c("region", "period")))) %>%
        filter(!(n() == 2 & grepl("^floorPerDwelling_(SFH|MFH)", .data[["variable"]]))) %>%
        ungroup()

      # ratio in dwelling size: SFH / MFH
      # simple mean for missing regions could be improved with proper weighting
      sizeRatio <- stockOdyssee %>%
        calc_addVariable(floorRatio = "floorPerDwelling_SFH / floorPerDwelling_MFH",
                         only.new = TRUE) %>%
        interpolate_missing_periods(unique(stock$period)) %>%
        group_by(.data[["region"]]) %>%
        group_modify(extrapolateMissingPeriods) %>%
        ungroup()
      sizeRatio <- sizeRatio %>%
        group_by(across(all_of(c("period", "variable")))) %>%
        summarise(value = mean(.data[["value"]]),
                  region = setdiff(dwellingSize$region, sizeRatio$region),
                  .groups = "drop") %>%
        bind_rows(sizeRatio) %>%
        ungroup()

      dwellingSize <- dwellingSize %>%
        filter(.data[["variable"]] == "floorPerDwelling_Total") %>%
        bind_rows(sizeRatio) %>%
        bind_rows(stock %>%
                    unite("variable", "variable", "buildingType") %>%
                    group_by(across(all_of(c("region", "period", "variable")))) %>%
                    summarise(value = sum(.data[["value"]]), .groups = "drop")) %>%
        select(-"model", -"scenario") %>%
        calc_addVariable(
          floorPerDwelling_MFH = paste("(floorPerDwelling_Total * (dwellings_SFH + dwellings_MFH)) /",
                                       "(floorRatio * dwellings_SFH + dwellings_MFH)"),
          floorPerDwelling_SFH = "floorPerDwelling_MFH * floorRatio",
          units = "m2"
        ) %>%
        filter(grepl("^floorPerDwelling_(SFH|MFH)", .data[["variable"]])) %>%
        separate("variable", c("variable", "buildingType"), sep = "_")

      # assume same dwelling size for all vintages, locations and heating systems
      dwellingSize <- dwellingSize %>%
        data.frame(location = rep(unique(stock$location),
                                  each = nrow(dwellingSize)))
      dwellingSize <- dwellingSize %>%
        data.frame(vintage = rep(unique(stock$vintage),
                                 each = nrow(dwellingSize)))
      dwellingSize <- dwellingSize %>%
        data.frame(heating = rep(unique(stock$heating),
                                 each = nrow(dwellingSize)))

      stock <- stock %>%
        rbind(dwellingSize) %>%
        calc_addVariable(floor = "dwellings * floorPerDwelling",
                         units = "m2", only.new = TRUE) %>%
        rbind(stock)



    },
    commercial = {
      NULL
    }
  )


  ## map to BRICK dimensions ====

  # TODO: This remapping should actually happen upstream where the primary # nolint: todo_comment_linter.
  # sources that define the dimensions are read

  # heating system
  hsMap <- toolGetMapping("dim_hs.csv",
                          type = "sectoral", where = "brick") %>%
    select(heating = "mredgebuildings", "hs") %>%
    unique()

  # building shell
  bsMap <- toolGetMapping("dim_bs.csv",
                          type = "sectoral", where = "brick") %>%
    select("bs", "initShare")

  # remap
  stock <- stock %>%
    left_join(hsMap, by = "heating") %>%
    cross_join(bsMap) %>%
    mutate(value = .data[["value"]] * .data[["initShare"]]) %>%
    select(-"heating", -"initShare") %>%
    rename(typ = "buildingType",
           loc = "location",
           vin = "vintage") %>%
    relocate("value", .after = last_col())

  # convert to magpie object
  stock <- stock %>%
    mutate(value = .data[["value"]] / 1E6) %>% # m2 -> million m2
    select(-"unit") %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value") %>%
    collapseDim() %>%
    toolCountryFill(verbosity = 2)

  # aggregate to BRICK granularity
  agg <- toolAggregateBrick(stock, granularity, NULL)

  return(list(x = agg$x,
              weight = NULL,
              min = 0,
              unit = "million dwellings or million m2",
              description = "Number of dwellings and floor space in the European building stock"))
}
