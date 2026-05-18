#' Calculate Reference data for input matching
#'
#' @author Robin Hasse
#'
#' @param subtype character, matching reference
#'
#' @importFrom magclass mbind as.magpie collapseDim mselect
#' @importFrom madrat readSource toolCountryFill toolGetMapping getISOlist
#' @importFrom quitte as.quitte interpolate_missing_periods removeColNa
#' @importFrom dplyr group_by filter mutate .data across all_of reframe distinct
#'   left_join %>% summarise select ungroup inner_join semi_join left_join
#'   right_join
#' @importFrom tidyr complete expand_grid
#' @importFrom zoo rollmean
#' @importFrom utils read.csv
#'
calcMatchingReference <- function(subtype) {

  periods <- 2000:2020

  weight <- NULL
  minVal <- 0
  maxVal <- NULL
  description <- NULL

  # reference mapping
  refMap <- toolGetMapping(paste0("refMap_", subtype, ".csv"),
                           type = "sectoral", where = "mredgebuildings",
                           returnPathOnly = TRUE) %>%
    read.csv(comment.char = "#", encoding = "UTF-8") %>%
    filter(!is.na(.data[["variable"]])) %>%
    select(-any_of(".color"))



  # FUNCTIONS ------------------------------------------------------------------


  aggStock <- function(stockHist, dim) {
    data <- stockHist %>%
      group_by(across(all_of(c("region", "period", dim)))) %>%
      summarise(value = sum(.data[["value"]]),
                .groups = "drop")
    colnames(data)[3] <- "variable"
    return(as.quitte(data))
  }


  .calcShares <- function(x) {
    x %>%
      group_by(across(all_of(c("region", "period", "refVarGroup")))) %>%
      mutate(value = proportions(.data[["value"]])) %>%
      ungroup() %>%
      select(-"refVarGroup")
  }


  getDataIDEES <- function(vars) {
    c("Residential_2021", "Tertiary_2021") %>%
      lapply(readSource, type = "JRC_IDEES") %>%
      lapply(mselect, code = vars) %>%
      do.call(what = mbind) %>%
      as.quitte(na.rm = TRUE) %>%
      select(-"scenario", -"model", -"unit", -"vintage", -"variable") %>%
      mutate(code = dot2Dash(sub("\\..+$", "", .data[["code"]]), rev = TRUE))
  }



  getIDEESHeatingShare <- function(refMap) {
    codes <- dot2Dash(refMap[[".code"]])
    variables <- unique(dot2Dash(refMap[["variable"]]))

    data <- getDataIDEES(codes)

    data <- refMap %>%
      select("variable", "refVarGroup", ".code", "sec") %>%
      unique() %>%
      inner_join(data, by = c(.code = "code")) %>%
      group_by(across(all_of(c("region", "period", "refVarGroup")))) %>%
      complete(variable = variables, fill = list(value = 0)) %>%
      filter(sub("^(.{3})_.*$", "\\1", .data[["variable"]]) == .data[["refVarGroup"]]) %>%
      group_by(across(all_of(c("region", "period", "variable", "refVarGroup")))) %>%
      summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
      .calcShares() %>%
      as.quitte()

    return(data)
  }


  # READ DATA ------------------------------------------------------------------

  if (grepl("^Odyssee_", subtype)) {
    odyssee <- readSource("Odyssee") %>%
      as.quitte(na.rm = TRUE)
  } else if (grepl("EUBDB", subtype)) {
    eubdbCateories <- c(
      "BuildingStockCharacteristics",
      "TechnicalBuildingSystems"
    )
    eubdb <- do.call(rbind, lapply(eubdbCateories, function(c) {
      readSource("EUBuildingsDB", c) %>%
        as.quitte(na.rm = TRUE) %>%
        select("region", "period", "variable", "unit", "value")
    }))


  } else if (grepl("mredgebuildings", subtype)) {
    stockHist <- calcOutput("BuildingStock", aggregate = FALSE) %>%
      as.quitte(na.rm = TRUE) %>%
      filter(.data[["variable"]] == "floor") %>%
      select(-"model", -"scenario", -"variable", -"unit")
  }





  # PREPARE DATA ---------------------------------------------------------------

  # nolint start: todo_comment_linter.

  switch(subtype,


    ## mredgebuildings_buildingType ====

    mredgebuildings_buildingType = {

      data <- aggStock(stockHist, "typ")

      unit <- "million m2"
      description <- "stock of residential buildings"

    },


    ## mredgebuildings_location ====

    mredgebuildings_location = {

      data <- aggStock(stockHist, "loc")

      unit <- "million m2"
      description <- "stock of residential buildings"

    },


    ## mredgebuildings_vintage ====

    mredgebuildings_vintage = {

      data <- aggStock(stockHist, "vin")

      unit <- "million m2"
      description <- "stock of residential buildings"

    },


    ## mredgebuildings_heating ====

    mredgebuildings_heating = {
      data <- aggStock(stockHist, "hs") %>%
        mutate(variable = as.character(.data[["variable"]])) %>%
        group_by(across(-all_of(c("variable", "value")))) %>%
        reframe(variable = c(.data[["variable"]], "h2bo"),
                value = c(.data[["value"]], 0))

      unit <- "million m2"
      description <- "stock of residential buildings"

    },


    ## IDEES_surface ====

    IDEES_surface = {

      vars <- dot2Dash(refMap[[".code"]])

      data <- getDataIDEES(vars)

      data <- refMap %>%
        select("variable", ".code") %>%
        unique() %>%
        left_join(data, by = c(.code = "code")) %>%
        mutate(value = .data[["value"]] / 1E3,
               unit = "million m2") %>%
        as.quitte() %>%
        select(-"model", -"scenario", -".code")

      minVal <- 0
      unit <- "million m2"
      description <- "Building stock floor space"
    },


    ## IDEES_heating ====

    IDEES_heating = {

      ktoe2kWh <- 1.163E7
      vars <- union(refMap[[".variableAbs"]], refMap[[".variablePerSqm"]]) %>%
        dot2Dash()

      data <- getDataIDEES(vars)

      data <- refMap %>%
        select("variable", ".variableAbs", ".variablePerSqm") %>%
        unique() %>%
        left_join(data, by = c(.variableAbs = "code")) %>%
        left_join(data, by = c(.variablePerSqm = "code", "region", "period"),
                  suffix = c("Abs", "PerSqm")) %>%
        group_by(across(-matches("\\.variable|value"))) %>%
        summarise(across(matches("value"), sum), .groups = "drop") %>%
        mutate(value = .data[["valueAbs"]] * ktoe2kWh / .data[["valuePerSqm"]] / 1E6,
               unit = "million m2") %>%
        select(-"valueAbs", -"valuePerSqm") %>%
        as.quitte()

      minVal <- 0
      unit <- "million m2"
      description <- "Building stock by heating system"
    },



    ## IDEES_heatingShare ====

    IDEES_heatingShare = {

      data <- getIDEESHeatingShare(refMap)

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "share of heating systems in the subsectoral stocks"

    },


    ## IDEES_heatingShareNew ====

    IDEES_heatingShareNew = {

      data <- getIDEESHeatingShare(refMap)

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "share of heating systems in new buildings"

    },


    ## OdysseeIDEES_typ ====

    OdysseeIDEES_typ = {
      data <- calcOutput("OdysseeStock", interpolate = TRUE, aggregate = FALSE) %>%
        as.quitte()
      data <- refMap %>%
        select("variable", ".variable") %>%
        unique() %>%
        left_join(data, by = c(.variable = "variable")) %>%
        select(-".variable") %>%
        mutate(value = .data$value / 1E6) %>% # m2 -> million m2
        as.quitte()

      minVal <- 0
      unit <- "million m2"
      description <- "Building stock floor space from Odyssee extrapolated with IDEES"

    },


    ## OdysseeIDEES_sec ====

    OdysseeIDEES_sec = {
      data <- calcOutput("OdysseeStock", interpolate = TRUE, aggregate = FALSE) %>%
        as.quitte()
      data <- refMap %>%
        select("variable", ".variable") %>%
        unique() %>%
        left_join(data, by = c(.variable = "variable")) %>%
        select(-".variable") %>%
        mutate(value = .data[["value"]] / 1E6) # m2 -> million m2

      minVal <- 0
      unit <- "million m2"
      description <- "Building stock floor space from Odyssee extrapolated with IDEES"

    },


    ## OdysseeIDEES_heating_typ ====

    `OdysseeIDEES_heating_typ` = {
      data <- calcOutput("OdysseeStock", interpolate = TRUE, aggregate = FALSE) %>%
        as.quitte()
      data <- refMap %>%
        select("variable", ".variable") %>%
        unique() %>%
        left_join(data, by = c(.variable = "variable")) %>%
        select(-".variable") %>%
        mutate(value = .data[["value"]] / 1E6) # m2 -> million m2

      minVal <- 0
      unit <- "million m2"
      description <- "Residential building stock floor space from Odyssee extrapolated with IDEES"

    },


    ## OdysseeIDEES_heating_sec ====

    `OdysseeIDEES_heating_sec` = {
      data <- calcOutput("OdysseeStock", interpolate = TRUE, aggregate = FALSE) %>%
        as.quitte()
      data <- refMap %>%
        select("variable", ".variable") %>%
        unique() %>%
        left_join(data, by = c(.variable = "variable")) %>%
        select(-".variable") %>%
        mutate(value = .data[["value"]] / 1E6) # m2 -> million m2

      minVal <- 0
      unit <- "million m2"
      description <- "Residential building stock floor space from Odyssee extrapolated with IDEES"

    },


    ## Odyssee_stock ====

    Odyssee_stock = {
      data <- odyssee %>%
        semi_join(refMap, by = "variable") %>%
        mutate(value = .data[["value"]] / 1E6) %>% # m2 -> million m2
        as.quitte()

      unit <- "million m2 or million dwellings"
      description <- "stock of dwellings and service floor space"

    },


    ## Odyssee_dwelSize ====

    Odyssee_dwelSize = {

      data <- refMap %>%
        select("variable") %>%
        unique() %>%
        left_join(odyssee, by = "variable")

      # TODO: weight should be floor area -> circle dependency

      unit <- "m2/dwel"
      description <- "average dwelling size of the stock and new construction"

    },


    ## Odyssee_construction ====

    Odyssee_construction = {

      data <- refMap %>%
        select("variable") %>%
        unique() %>%
        left_join(odyssee, by = "variable") %>%
        mutate(value = .data[["value"]] / 1E6,    # dwel/yr -> million dwel/yr
               unit = "million dwel/yr") %>%
        as.quitte()

      unit <- "million dwel/yr"
      description <- "flow of newly constructed buildings"

    },


    ## Odyssee_constructionFloor_typ ====

    Odyssee_constructionFloor_typ = {

      data <- refMap %>%
        select("variable", ".dwel", ".size") %>%
        unique() %>%
        left_join(odyssee, by = c(.dwel = "variable")) %>%
        left_join(odyssee, by = c(.size = "variable", "region", "period"),
                  suffix = c("Dwel", "Size")) %>%
        mutate(value = .data$valueDwel * .data$valueSize / 1E6, # m2/yr -> million m2/yr
               unit = "million m2/yr") %>%
        select("region", "period", "variable", "unit", "value") %>%
        as.quitte() %>%
        interpolate_missing_periods(periods, expand.values = TRUE) # TODO: remove extrapolation

      unit <- "million m2/yr"
      description <- "flow of newly constructed buildings"

    },



    ## Odyssee_constructionFloor_sec ====

    Odyssee_constructionFloor_sec = {

      data <- refMap %>%
        select("variable", ".dwel", ".size") %>%
        unique() %>%
        left_join(odyssee, by = c(.dwel = "variable")) %>%
        left_join(odyssee, by = c(.size = "variable", "region", "period"),
                  suffix = c("Dwel", "Size")) %>%
        mutate(value = .data$valueDwel * .data$valueSize / 1E6, # m2/yr -> million m2/yr
               unit = "million m2/yr") %>%
        select("region", "period", "variable", "unit", "value") %>%
        as.quitte() %>%
        interpolate_missing_periods(periods, expand.values = TRUE) # TODO: remove extrapolation

      unit <- "million m2/yr"
      description <- "flow of newly constructed buildings"

    },


    ## Odyssee_heatingShare ====

    Odyssee_heatingShare = {
      odyssee <- odyssee %>%
        select("region", "period", "variable", "value")
      uniqueVarsInGroup <- function(rvg) {
        unique(refMap[refMap$refVarGroup == unique(rvg), "variable"])
      }
      data <- refMap %>%
        left_join(odyssee, by = c(.code = "variable")) %>%
        left_join(odyssee, by = c(.codeTotal = "variable", "region", "period"),
                  suffix = c("", "Total")) %>%
        group_by(across(all_of(c("region", "period", "refVarGroup", "variable")))) %>%
        summarise(across(matches("value"), sum), .groups = "drop") %>%
        ungroup() %>%
        group_by(across(all_of(c("region", "period", "refVarGroup")))) %>%
        mutate(value = ifelse(all(uniqueVarsInGroup(.data$refVarGroup) %in% .data$variable),
                              proportions(.data$value),
                              .data$value / .data$valueTotal)) %>%
        ungroup() %>%
        filter(.data$value > 0) %>%
        select(-"valueTotal") %>%
        as.quitte()

      # TODO: weights

      unit <- "1"
      description <- "share of heating systems in the stock"

    },


    ## Odyssee_heatingShare ====

    Odyssee_heatingShare = {
      odyssee <- odyssee %>%
        select("region", "period", "variable", "value")
      uniqueVarsInGroup <- function(rvg) {
        unique(refMap[refMap$refVarGroup == unique(rvg), "variable"])
      }
      data <- refMap %>%
        left_join(odyssee, by = c(.code = "variable")) %>%
        left_join(odyssee, by = c(.codeTotal = "variable", "region", "period"),
                  suffix = c("", "Total")) %>%
        group_by(across(all_of(c("region", "period", "refVarGroup", "variable")))) %>%
        summarise(across(matches("value"), sum), .groups = "drop") %>%
        ungroup() %>%
        group_by(across(all_of(c("region", "period", "refVarGroup")))) %>%
        mutate(value = ifelse(all(uniqueVarsInGroup(.data$refVarGroup) %in% .data$variable),
                              proportions(.data$value),
                              .data$value / .data$valueTotal)) %>%
        ungroup() %>%
        filter(.data$value > 0) %>%
        select(-"valueTotal") %>%
        as.quitte()

      # TODO: weights

      unit <- "1"
      description <- "share of heating systems in the stock"

    },


    ## EUBDB_vintage ====

    EUBDB_vintage = {

      data <- refMap %>%
        select("variable", ".variable") %>%
        unique() %>%
        left_join(eubdb, by = c(.variable = "variable")) %>%
        as.quitte() %>%
        select(-".variable")

      unit <- "1"
      description <- "share of dwellings in vintage cohort"

    },


    ## EUBDB_stock ====

    EUBDB_stock = {

      data <- refMap %>%
        select("variable", ".variable", "qty") %>%
        unique() %>%
        left_join(eubdb, by = c(.variable = "variable")) %>%
        mutate(value = .data[["value"]] / 1E6,    # qty -> million qty
               unit = c(num = "million dwel", area = "million m2")[.data[["qty"]]]) %>%
        select(-"qty", -".variable") %>%
        as.quitte(na.rm = TRUE)

      unit <- "million dwel or million m2"
      description <- "stock of dwellings and service floor space"
    },


    ## EuropeanCommissionRenovation ====

    EuropeanCommissionRenovation = {

      # The data represents the average between 2012 and 2016. But we assume
      # this value for all historic periods temporarily
      data <- readSource("EuropeanCommissionRenovation") %>%
        as.quitte(na.rm = TRUE) %>%
        unite("variable", "variable", "renovation", "subsector", sep = "|") %>%
        right_join(refMap %>%
                     select("variable") %>%
                     unique(),
                   by = "variable") %>%
        select(-"model", -"scenario") %>%
        group_by(across(all_of(c("region", "variable", "unit", "value")))) %>%
        reframe(period = periods) %>%
        ungroup() %>%
        as.quitte()

      unit <- "1/yr"
      description <- "building shell renovation rate"
    },


    ## Hotmaps_typ_vin ====

    Hotmaps_typ_vin = {
      data <- readSource("Hotmaps") %>%
        as.quitte(na.rm = TRUE) %>%
        select(-"model", -"scenario", -"unit")
      data <- refMap %>%
        left_join(data, by = c(.variable = "variable",
                               .bage = "bage",
                               .building = "building")) %>%
        select(-matches("\\.")) %>%
        group_by(across(all_of(c("region", "period", "variable", "refVarGroup")))) %>%
        summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
        .calcShares()


      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of vintages in building stock by building type"
    },


    ## Hotmaps_sec_vin ====

    Hotmaps_sec_vin = {
      data <- readSource("Hotmaps") %>%
        as.quitte(na.rm = TRUE) %>%
        select(-"model", -"scenario", -"unit")
      data <- refMap %>%
        left_join(data, by = c(.variable = "variable",
                               .bage = "bage",
                               .building = "building")) %>%
        select(-matches("\\.")) %>%
        group_by(across(all_of(c("region", "period", "variable", "refVarGroup")))) %>%
        summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
        .calcShares()


      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of vintages in building stock"
    },


    ## CensusHub_typ_vin ====

    CensusHub_typ_vin = {
      vinMap <- toolGetMapping("vintageMapping_CensusHub.csv",
                               type = "sectoral", where = "mredgebuildings")
      data <- readSource("CensusHub", subtype = "typeVintage") %>%
        as.quitte(na.rm = TRUE) %>%
        right_join(vinMap, by = c("period", "constructionPeriod"),
                   relationship = "many-to-many") %>%
        group_by(across(all_of(c("region", "period", "vin", "buildingType")))) %>%
        summarise(value = sum(.data[["weight"]] * .data[["value"]]),
                  .groups = "drop")
      data <- refMap %>%
        left_join(data,
                  by = c("vin", .buildingType = "buildingType")) %>%
        group_by(across(all_of(c("region", "period", "variable", "refVarGroup",
                                 "vin", "typ")))) %>%
        summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
        .calcShares() %>%
        select("region", "period", "variable", "value")

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of vintages in residential building stock by building type"
    },


    ## CensusHub_vin ====

    CensusHub_vin = {
      vinMap <- toolGetMapping("vintageMapping_CensusHub.csv",
                               type = "sectoral", where = "mredgebuildings")
      data <- readSource("CensusHub", subtype = "typeVintage") %>%
        as.quitte(na.rm = TRUE) %>%
        right_join(vinMap, by = c("period", "constructionPeriod"),
                   relationship = "many-to-many") %>%
        group_by(across(all_of(c("region", "period", "vin", "buildingType")))) %>%
        summarise(value = sum(.data[["weight"]] * .data[["value"]]),
                  .groups = "drop")
      data <- refMap %>%
        left_join(data,
                  by = c("vin", .buildingType = "buildingType")) %>%
        group_by(across(all_of(c("region", "period", "variable", "refVarGroup", "vin")))) %>%
        summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
        .calcShares() %>%
        select("region", "period", "variable", "value")

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of vintages in residential building stock"
    },


    ## HeatingSystemSales ====

    HeatingSystemSales = {
      variables <- unique(refMap[["hs"]])
      data <- calcOutput("HeatingSystemSales", aggregate = FALSE) %>%
        as.quitte(na.rm = TRUE) %>%
        removeColNa()
      data <- refMap %>%
        inner_join(data, by = "hs") %>%
        group_by(across(all_of(c("region", "period", "refVarGroup")))) %>%
        complete(variable = variables, fill = list(value = 0)) %>%
        .calcShares() %>%
        select("region", "period", "variable", "value")

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of heating systems in sales"

    },

    ## VHK ====

    VHK = {
      variables <- unique(refMap[["variable"]])
      data <- readSource("VHK", subtype = "2019_spaceHeating.Task2") %>%
        as.quitte(na.rm = TRUE) %>%
        removeColNa()
      data <- refMap %>%
        inner_join(data, by = c(.variable = "variable")) %>%
        group_by(across(all_of(c("region", "period", "variable", "refVarGroup")))) %>%
        summarise(value = sum(.data$value * .data$.weight),
                  .groups = "drop") %>%
        group_by(across(all_of(c("region", "period", "refVarGroup")))) %>%
        complete(variable = variables, fill = list(value = 0)) %>%
        .calcShares() %>%
        select("region", "period", "variable", "value") %>%
        interpolate_missing_periods(period = 1991:2014)

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of boilers in sales"

    },

    ## StatusQuo ====

    StatusQuo = {
      data <- calcOutput("StatusQuoShare", aggregate = FALSE) %>%
        as.quitte(na.rm = TRUE) %>%
        select(old = "hs", new = "hs", "value")
      data <- refMap %>%
        left_join(data, by = c(hs = "old", hsr = "new")) %>%
        distinct(.data$variable, .data$hs, .data$value, .keep_all = TRUE) %>%
        group_by(.data$hs) %>%
        # calculate probability for non-identical replacement
        #   this is given as a reference target such that relative reference
        #   values add up to one, but only identical replacement is considered
        #   in the matching objective
        mutate(value = ifelse(is.na(.data$value),
                              1 - sum(.data$value, na.rm = TRUE),
                              .data$value)) %>%
        ungroup() %>%
        select("variable", "value") %>%
        expand_grid(region = getISOlist()) %>%
        mutate(period = 2021) %>%
        as.quitte() %>%
        # assume for all time periods
        interpolate_missing_periods(period = 1999:2023, expand.values = TRUE)

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of old heating systems replaced by themselves"

    },

    ## EHI_statusQuo ====

    EHI_statusQuo = {
      data <- calcOutput("Renovation", aggregate = FALSE) %>%
        as.quitte(na.rm = TRUE) %>%
        removeColNa() %>%
        group_by(across(-all_of(c("loc", "typ", "value")))) %>%
        summarise(value = sum(.data$value), .groups = "drop") %>%
        ungroup()
      data <- refMap %>%
        select(-"sec", -".comment") %>%
        left_join(data, by = c("hs", "hsr")) %>%
        select(-"hs", -"hsr") %>%
        .calcShares() %>%
        interpolate_missing_periods(period = 1999:2023, expand.values = TRUE)

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of old heating systems during heating system replacement"

    },

    ## Pulsen ====

    Pulsen = {
      data <- readSource("Pulsen") %>%
        as.quitte(na.rm = TRUE)
      data <- refMap %>%
        select("variable", ".variable") %>%
        left_join(data, by = c(.variable = "variable")) %>%
        group_by(across(all_of(c("region", "period", "variable")))) %>%
        summarise(value = sum(.data$value) / 100, .groups = "drop")

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of previous heating system in heat pump installations"
    },

    ## Destatis ====

    Destatis = {
      data <- calcOutput("Construction", aggregate = FALSE) %>%
        mselect(variable = "nBuildings", collapseNames = TRUE) %>%
        as.quitte(na.rm = TRUE) %>%
        removeColNa()
      data <- refMap %>%
        left_join(data, by = c(.typ = "typ", "hs")) %>%
        group_by(across(all_of(c("region", "period", "variable", "refVarGroup")))) %>%
        summarise(value = sum(.data$value), .groups = "drop") %>%
        .calcShares()

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of heating systems in new construction"

    },


    ## Destatis_typ ====

    Destatis_typ = {
      data <- calcOutput("Construction", aggregate = FALSE) %>%
        mselect(variable = "nBuildings", collapseNames = TRUE) %>%
        as.quitte(na.rm = TRUE) %>%
        removeColNa()
      data <- refMap %>%
        left_join(data, by = c("typ", "hs")) %>%
        select(-"typ", -"hs") %>%
        .calcShares()

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of heating systems in new construction by building type"

    },

    ## dummy_hsReplace ====

    dummy_hsReplace = {
      lifetime <- 20
      data <- mbind(calcOutput("MatchingReference", subtype = "OdysseeIDEES_typ",
                               aggregate = FALSE),
                    calcOutput("MatchingReference", subtype = "OdysseeIDEES_sec",
                               aggregate = FALSE)) %>%
        as.quitte(na.rm = TRUE) %>%
        removeColNa()
      data <- refMap %>%
        left_join(data, by = c(typ = "variable")) %>%
        group_by(across(all_of(c("region", "period", "variable")))) %>%
        summarise(value = sum(.data$value), .groups = "drop") %>%
        interpolate_missing_periods((2000 - lifetime):2022,
                                    expand.values = TRUE) %>%
        group_by(across(-all_of(c("period", "value")))) %>%
        arrange(.data$period) %>%
        mutate(value = 1 / lifetime * rollmean(.data$value, k = lifetime,
                                               fill = NA, align = "right")) %>%
        filter(!is.na(.data$value))

      minVal <- 0
      unit <- "million m2"
      description <- "Total renovated floor space"

    },

    ## Eurostat_loc ====

    Eurostat_loc = {

      # number of households
      hh <- readSource("EurostatBuildings", subtype = "lfst_r_lfsd2hh") %>%
        collapseDim("unit")
      hh <- hh / 1000 # unit conversion: thousand -> million

      # avg. dwelling size
      dwellSize <- mbind(
        readSource("EurostatBuildings", subtype = "ilc_hcmh02") %>%
          mselect(hhtyp = "TOTAL", collapseNames = TRUE),
        readSource("EurostatBuildings", subtype = "ilc_lvho31") %>%
          mselect(hhcomp = "TOTAL", collapseNames = TRUE)
      ) %>%
        as.quitte() %>%
        interpolate_missing_periods(getYears(hh, as.integer = TRUE), expand.values = TRUE) %>%
        as.magpie()

      # harmonise
      items <- getItems(dwellSize, 3.1)
      hh <- hh[, , items]
      i <- setdiff(items, "TOTAL")
      hh[, , i] <- hh[, , i] * hh[, , "TOTAL"] / dimSums(hh[, , i])

      data <- hh * dwellSize
      data <- as_tibble(data)
      data <- refMap %>%
        left_join(data, by = c(.deg_urb = "deg_urb")) %>%
        group_by(across(all_of(c("region", "period", "variable")))) %>%
        summarise(value = sum(.data$value), .groups = "drop")

      minVal <- 0
      unit <- "million m2"
      description <- "Residential building stock by location"

    },

    ## Eurostat_hs_loc ====

    Eurostat_hs_loc = {

      # we apply the shares of persons to number of households neglecting
      # differences in the household size between different heating systems

      # percentage of persons
      shareDH <- readSource("EurostatBuildings", subtype = "ilc_lvhe02") %>%
        mselect(hhcomp = "TOTAL", amenity = "DHEAT", collapseNames = TRUE) / 100
      shareHeatingNoDH <- readSource("EurostatBuildings", subtype = "ilc_lvhe05") %>%
        mselect(hhcomp = "TOTAL", collapseNames = TRUE) / 100

      # drop unknown and other and renormalise
      shareHeatingNoDH <- shareHeatingNoDH[, , c("OTH", "UNK"), invert = TRUE]
      shareHeatingNoDH <- shareHeatingNoDH / dimSums(shareHeatingNoDH, 3.1)

      shareHeating <- mbind(
        add_dimension(shareDH, add = "nrg_src", nm = "DHEAT"),
        shareHeatingNoDH * (1 - shareDH)
      ) %>% dimOrder(c(2, 1))

      # time steps
      t <- getYears(shareHeating)

      # number of households
      hh <- readSource("EurostatBuildings", subtype = "lfst_r_lfsd2hh")[, t, ] %>%
        collapseDim("unit")
      hh <- hh / 1000 # unit conversion: thousand -> million
      items <- getItems(shareHeating, "deg_urb")
      hh <- hh[, , items]
      i <- setdiff(items, "TOTAL")
      hh[, , i] <- hh[, , i] * hh[, , "TOTAL"] / dimSums(hh[, , i])

      # floor space per dwelling
      dwellSize <- readSource("EurostatBuildings", subtype = "ilc_lvho31")[, t, ] %>%
        mselect(hhcomp = "TOTAL", collapseNames = TRUE)

      # finalise
      data <- hh * dwellSize * shareHeating
      data <- as_tibble(data)
      data <- refMap %>%
        select("variable", deg_urb = ".deg_urb", nrg_src = ".nrg_src") %>%
        unique() %>%
        left_join(data, by = c("deg_urb",  "nrg_src")) %>%
        group_by(across(all_of(c("region", "period", "variable")))) %>%
        summarise(value = sum(.data$value), .groups = "drop")

      minVal <- 0
      unit <- "million m2"
      description <- "Residential building stock by heating system and location"

    },

    ## Eurostat_typ_loc ====

    Eurostat_typ_loc = {

      # percentage of persons
      share <- readSource("EurostatBuildings", subtype = "ilc_lvho01") %>%
        mselect(incgrp = "TOTAL", collapseNames = TRUE) / 100
      typ <- c("FLAT", "HOUSE")
      loc <- c("DEG1", "DEG2", "DEG3")
      i <- quitte::cartesian(typ, loc)
      share[, , i] <- share[, , i] * share[, , "TOTAL.TOTAL", drop = TRUE] / dimSums(share[, , i])
      share <- share[, , i]

      # time steps
      t <- getYears(share, as.integer = TRUE)

      # number of rooms per dwelling
      rooms <- readSource("EurostatBuildings", subtype = "ilc_lvho03") %>%
        mselect(tenure = "TOTAL", collapseNames = TRUE) %>%
        as.quitte() %>%
        interpolate_missing_periods(t, expand.values = TRUE) %>%
        as.magpie()

      # room ratio: houses:flats
      roomRatio <- rooms[, , "HOUSE", drop = TRUE] / rooms[, , "FLAT", drop = TRUE]
      roomRatio <- toolCountryFillAvg(roomRatio, verbosity = 2)
      areaRatio <- roomRatio * 1.2 # assumed: rooms in houses are 20% bigger than rooms in flats

      # shift shares from flats to houses to reflect dwelling size differences
      share[, , "FLAT"] <- share[, , "FLAT"] /
        (share[, , "HOUSE", drop = TRUE] * areaRatio + share[, , "FLAT", drop = TRUE])
      share[, , "HOUSE"] <- share[, , "HOUSE"] /
        (share[, , "HOUSE", drop = TRUE] + share[, , "FLAT", drop = TRUE] / areaRatio)

      # number of households
      hh <- readSource("EurostatBuildings", subtype = "lfst_r_lfsd2hh") %>%
        collapseDim("unit") %>%
        as.quitte() %>%
        interpolate_missing_periods(t, expand.values = TRUE) %>%
        as.magpie()
      hh <- hh[, , loc] * hh[, , "TOTAL"] / dimSums(hh[, , loc])
      hh <- hh / 1000 # unit conversion: thousand -> million

      # floor space per dwelling
      dwellSize <- mbind(
        readSource("EurostatBuildings", subtype = "ilc_hcmh02") %>%
          mselect(hhtyp = "TOTAL", collapseNames = TRUE),
        readSource("EurostatBuildings", subtype = "ilc_lvho31") %>%
          mselect(hhcomp = "TOTAL", collapseNames = TRUE)
      ) %>%
        mselect(deg_urb = loc) %>%
        as.quitte() %>%
        interpolate_missing_periods(t, expand.values = TRUE) %>%
        as.magpie()

      # finalise
      data <- hh * dwellSize * share
      data <- as_tibble(share)
      data <- refMap %>%
        select("variable", "refVarGroup", deg_urb = ".deg_urb", building = ".building") %>%
        unique() %>%
        left_join(data, by = c("deg_urb",  "building")) %>%
        group_by(across(all_of(c("region", "period", "variable", "refVarGroup")))) %>%
        summarise(value = sum(.data$value), .groups = "drop") %>%
        .calcShares()

      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Residential building stock by building type and location"

    },

    ## LTRS_typ ====

    LTRS_typ = {
      data <- calcOutput("LTRS", refGranularity = "typ", aggregate = FALSE) %>%
        as.quitte(na.rm = TRUE) %>%
        removeColNa()
      # map to BRICK reference variables
      data <- refMap %>%
        select("variable", "refVarGroup", "typ", ".hs", "vin") %>%
        unique() %>%
        inner_join(data, by = c(.hs = "technology", "typ", "vin")) %>%
        select("region", "period", "variable", "value")


      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of heating systems in each vintage"
    },


    ## LTRS_sec ====

    LTRS_sec = {
      data <- calcOutput("LTRS", refGranularity = "sec", aggregate = FALSE) %>%
        as.quitte(na.rm = TRUE) %>%
        removeColNa()
      # map to BRICK reference variables
      data <- refMap %>%
        select("variable", "refVarGroup", "sec", ".hs", "vin") %>%
        unique() %>%
        inner_join(data, by = c(.hs = "technology", sec = "typ", "vin")) %>%
        select("region", "period", "variable", "value") %>%
        interpolate_missing_periods(2000:2023, expand.values = TRUE)


      minVal <- 0
      maxVal <- 1
      unit <- "1"
      description <- "Share of heating systems in each vintage"
    },

    stop("The subtype '", subtype, "' is an invalid matching reference.")
  )

  # nolint end: todo_comment_linter.



  # OUTPUT ---------------------------------------------------------------------

  data <- data %>%
    as.magpie() %>%
    collapseDim(keepdim = c("region", "period", "variable")) %>%
    toolCountryFill(NA, verbosity = 2)


  return(list(x = data,
              weight = weight,
              unit = unit,
              min = minVal,
              max = maxVal,
              description = description,
              cache = FALSE))
}
