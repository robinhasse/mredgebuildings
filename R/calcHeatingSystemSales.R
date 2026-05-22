#' Calculate yearly sales of heating systems
#'
#' We use national data from industry association for the biggest European
#' countries and European data sets from the EHI and EHPA.
#'
#' @returns MagPIE object with heating system sales
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource calcOutput toolGetMapping
#' @importFrom magclass mselect collapseDim getSets<- complete_magpie
#' @importFrom dplyr %>% .data filter
#' @export

calcHeatingSystemSales <- function() {

  # INDIVIDUAL SOURCES ---------------------------------------------------------

  # all heating systems
  salesTotal <- calcOutput("BRG", aggregate = FALSE, warnNA = FALSE) %>%
    .removeNADims("region")

  # efficient heating systems (i.e. excl. non-condensing)
  mapEHI <- toolGetMapping("technologyMapping_EHI.csv",
                           type = "sectoral", where = "mredgebuildings") %>%
    filter(.data[["considerSales"]])
  incompleteHs <- mapEHI %>%
    filter(!.data[["isComplete"]]) %>%
    getElement("technologyBRICK") %>%
    unique()
  salesEff <- readSource("EHI") %>%
    mselect(variable = mapEHI[["variable"]]) %>%
    .removeNADims() %>%
    .mapToBRICK(mapEHI, from = "variable")

  # heat pumps (used for space heating in buildings)
  mapEHPA <- toolGetMapping("technologyMapping_EHPA.csv",
                            type = "sectoral", where = "mredgebuildings") %>%
    filter(.data[["spaceHeatingHP"]])
  salesHp <- readSource("EHPA", subtype = "hpSales") %>%
    mselect(corrected = TRUE) %>%
    .removeNADims() %>%
    .mapToBRICK(mapEHPA, from = "hpType")

  # capacity
  cap <- readSource("VHK", subtype = "2019_spaceHeating.Task2")

  # DEU sales by new construction and renovation
  mapDena <- toolGetMapping("technologyMapping_Dena.csv",
                            type = "sectoral", where = "mredgebuildings")
  salesSplit <- readSource("DenaGebaeudereport", subtype = "salesHeatingSystems") %>%
    .mapToBRICK(mapDena, from = "tech", collapse = FALSE)



  # COMBINE --------------------------------------------------------------------

  ## many European countries ====

  # common regions
  reg <- Reduce(intersect, lapply(list(salesTotal, salesEff, salesHp),
                                  getItems, dim = 1))

  # all time steps
  t <- Reduce(union, lapply(list(salesTotal, salesEff, salesHp),
                            getItems, dim = 2))

  # extrapolate to all time steps
  salesTotal <- .time_interpolate(salesTotal, t, 0.2)
  salesEff <- .time_interpolate(salesEff, t, 0.8)
  salesHp <- .time_interpolate(salesHp, t, 0.2)

  sales <- mbind(salesEff[reg, , ], salesHp[reg, , ])

  # scale incomplete technologies to match total
  salesMissing <- salesTotal[reg, , ] - dimSums(sales)
  salesMissing[salesMissing < 0] <- 0
  incompleteFactor <- 1 + salesMissing / dimSums(sales[, , incompleteHs])
  sales[, , incompleteHs] <- sales[, , incompleteHs] * incompleteFactor

  # add zeros for missing technologies
  sales <- add_columns(sales, c("reel", "sobo"), dim = 3, fill = 0)
  sales <- add_columns(sales, "dihe", dim = 3, fill = NA)



  ## individual countries ====

  ### AUT ####
  mapBMK <- toolGetMapping("technologyMapping_BMK.csv",
                           type = "sectoral",
                           where = "mredgebuildings",
                           returnPathOnly = TRUE) %>%
    read.csv(encoding = "UTF-8")
  bmk <- readSource("BMK")["AUT", t] %>%
    .mapToBRICK(mapBMK, from = "technology")
  hs <- setdiff(getItems(bmk, "hs"), "sobo_biom")
  sales["AUT", , hs] <- bmk[, , hs]
  sales["AUT", , "sobo"] <- pmax(0, bmk[, , "sobo_biom"] - sales["AUT", , "biom"])


  ### DEU  ####
  mapBDH <- toolGetMapping("technologyMapping_BDH.csv",
                           type = "sectoral",
                           where = "mredgebuildings")
  bdh <- readSource("BDH")["DEU", t] %>%
    .mapToBRICK(mapBDH, from = "technology")
  hs <- getItems(bdh, "hs")
  sales["DEU", , hs] <- bdh[, , hs]


  ### ESP ####
  mapFEGECA <- toolGetMapping("technologyMapping_FEGECA.csv",
                              type = "sectoral",
                              where = "mredgebuildings",
                              returnPathOnly = TRUE) %>%
    read.csv(encoding = "UTF-8") %>%
    filter(.data[["considerSales"]])
  # scale sum of gabo and libo to match total of boilers and add gas heaters
  fegeca <- readSource("FEGECA")["ESP", , ] %>%
    .mapToBRICK(mapFEGECA, from = "variable") %>%
    time_interpolate(t)
  hs <- c("gabo", "libo")
  sales["ESP", , hs] <- fegeca[, , "gabo_libo"] / dimSums(sales["ESP", , hs]) %>%
    collapseDim(3) %>%
    magpie_expand(sales["ESP", , hs]) *
    sales["ESP", , hs]
  hs <- setdiff(getItems(fegeca, "hs"), "gabo_libo")
  sales["ESP", , hs] <- sales["ESP", , hs] + fegeca[, , hs]


  ### FRA ####
  mapUniclima <- toolGetMapping("technologyMapping_Uniclima.csv",
                                type = "sectoral",
                                where = "mredgebuildings") %>%
    filter(.data[["considerSales"]])
  # scale sum of gabo and libo to match total of boilers, add blown air heaters
  uniclima <- readSource("Uniclima")["FRA", , ] %>%
    .mapToBRICK(mapUniclima, from = "variable") %>%
    time_interpolate(t)
  hs <- c("gabo", "libo")
  sales["FRA", , hs] <- uniclima[, , "gabo_libo"] / dimSums(sales["FRA", , hs]) %>%
    collapseDim(3) %>%
    magpie_expand(sales["FRA", , hs]) *
    sales["FRA", , hs] +
    uniclima[, , hs]


  ### ITA ####
  mapAssotermica <- toolGetMapping("technologyMapping_Assotermica.csv",
                                   type = "sectoral",
                                   where = "mredgebuildings") %>%
    filter(.data[["considerSales"]])
  # only consider non-condensing and add it to condensing from EHI
  assotermica <- readSource("Assotermica")["ITA", , ] %>%
    .mapToBRICK(mapAssotermica, from = "variable") %>%
    time_interpolate(t)
  hs <- getItems(assotermica, "hs")
  sales["ITA", , hs] <- assotermica[, , hs] + salesEff["ITA", , hs]


  ### POL ####
  mapSPIUG <- toolGetMapping("technologyMapping_SPIUG.csv",
                             type = "sectoral",
                             where = "mredgebuildings") %>%
    filter(.data[["considerSales"]])
  spiug <- readSource("SPIUG")["POL", t] %>%
    .mapToBRICK(mapSPIUG, from = "variable") %>%
    add_columns("sobo", 3, NA)
  spiug[, , "sobo"] <- pmax(0, spiug[, , "sobo_biom"] - sales["POL", , "biom"])
  hs <- setdiff(getItems(spiug, "hs"), "sobo_biom")
  sales["POL", , hs] <- spiug[, , hs]



  # EXTRAPOLATE ----------------------------------------------------------------


  ## To all regions ====
  stock <- calcOutput("BuildingStock", aggregate = FALSE) %>%
    mselect(variable = "dwellings") %>%
    dimSums(c("variable", "typ", "loc", "vin", "bs")) %>%
    .removeNADims("region") %>%
    .time_interpolate(t, 0.3)

  # based on dwelling numbers for all regions that have stock data
  salesExtrStock <- .predictSales(sales, stock)

  # recover data that got lost when we filtered for common regions
  reg <- intersect(getItems(salesExtrStock, 1), getItems(salesHp, 1))
  salesExtrStock[reg, , "ehp1"] <- salesHp[reg, , ]
  reg <- intersect(getItems(salesExtrStock, 1), getItems(salesEff, 1))
  salesExtrStock[reg, , "biom"] <- salesEff[reg, , "biom"]


  salesExtr <- salesExtrStock
  salesExtr[getItems(sales, 1), , ] <- sales


  ## District heating ====

  # extrapolate dihe sales from AUT to all countries
  salesExtr[, , "dihe"] <- salesExtrStock[, , "dihe"]
  salesExtr["AUT", , "dihe"] <- sales["AUT", , "dihe"]


  ## Split new construction and renovation ====
browser()
  salesTest <- .splitSales(salesExtr, salesSplit)



  # RETURN ---------------------------------------------------------------------

  salesExtr <- salesExtr %>%
    toolCountryFill(verbosity = 2)

  return(list(x = salesExtr,
              min = 0,
              weight = NULL,
              description = "Sales of heating systems for space heating",
              unit = "units/yr"))
}




.mapToBRICK <- function(x, map, from, to = "technologyBRICK", collapse = TRUE) {
  out <- x %>%
    replace_non_finite(0) %>%
    toolAggregate(rel = map, from = from, to = to, dim = 3.1, partrel = TRUE,
                  verbosity = 3)
  if (ndim(out) > 3 && collapse) {
    out <- collapseDim(out, 3.2)
  }
  getSets(out)[3] <- "hs"
  return(out)
}


.removeNADims <- function(x, dims = NULL) {
  if (is.null(dims)) {
    dims <- getSets(x)
  }
  for (dim in dims) {
    dimSelect <- dimSums(!is.na(x), setdiff(getSets(x), dim)) > 0
    dimSelect <- getItems(dimSelect, dim)[dimSelect]
    dimSelect <- stats::setNames(list(dimSelect), dim)
    x <- mselect(x, dimSelect)
  }
  x
}


.predictSales <- function(from, to) {
  commonCols <- intersect(tail(getSets(from), -2), tail(getSets(to), -2))
  data <- left_join(.toDf(from), .toDf(to),
                    by = c("region", "period", commonCols),
                    suffix = c("From", "To"))
  factor <- data %>%
    group_by(across(all_of(commonCols))) %>%
    summarise(factor = lm(.data[["valueFrom"]] ~ .data[["valueTo"]] + 0)$coefficients)

  to %>%
    as.quitte() %>%
    left_join(factor, by = commonCols) %>%
    mutate(value = .data[["value"]] * .data[["factor"]]) %>%
    select(-"factor") %>%
    as.quitte() %>%
    as.magpie()
}


.toDf <- function(x) {
  x %>%
    as.quitte() %>%
    removeColNa()
}


.time_interpolate <- function(dataset, # nolint: object_name_linter.
                              interpolated_year, # nolint: object_name_linter.
                              wLinear = 1,
                              nonNegative = TRUE,
                              integrate_interpolated_years = FALSE) { # nolint: object_name_linter.
  out <- wLinear  * time_interpolate(dataset,
                                     interpolated_year,
                                     integrate_interpolated_years,
                                     extrapolation_type = "linear") +
    (1 - wLinear) * time_interpolate(dataset,
                                     interpolated_year,
                                     integrate_interpolated_years,
                                     extrapolation_type = "constant")
  if (nonNegative) {
    out[] <- pmax(0, out)
  }
  return(out)
}


.splitSales <- function(x, xSplit) {
  browser()
  xSplit <- .toDf(xSplit)
  xSplitSmooth <- xSplit %>%
    group_by(across(-all_of(c("region", "value")))) %>%
    summarise(value = sum(.data$value), .groups = "drop") %>%
    group_by(.data$hs) %>%
    mutate(.fit = list(lm(value ~ period, data = dplyr::cur_data())),
           value = predict(.fit[[1]], newdata = data.frame(period = .data$period))) %>%
    ungroup() %>%
    select(-".fit")
  test <- x %>%
    .toDf() %>%
    left_join(xSplit, by = c("region", "period", "hs"), suffix = c("", "Split")) %>%
    left_join(xSplitSmooth, by = c("period", "hs", "build"), suffix = c("", "SplitSmooth")) %>%
    mutate(ref = ifelse(is.na(.data$valueSplit),
                        .data$valueSplitSmooth,
                        .data$valueSplit),
           .keep = "unused")
}
