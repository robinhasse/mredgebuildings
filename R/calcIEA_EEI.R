#' Process data from IEA End Uses and Efficiency Indicators Database
#'
#' IEA EEI final energy data is processed and mapped w.r.t. carrier and enduse names.
#'
#' As for the buildings sector, data for residential and commercial ("service")
#' buildings is aggregated and the carrier "biomass" is split into traditional
#' and modern biomass w.r.t. to income per capita. Energy data on "space_cooling"
#' is aggregated over all carriers into "elec".
#'
#' @param subtype sector name
#' @param mixData logical indicating whether to mix res/com data points if both are not given
#'
#' @return data.frame containing enduse- and carrier-resoluted energy data
#'
#' @author Hagen Tockhorn
#'
#' @importFrom madrat readSource calcOutput toolGetMapping
#' @importFrom dplyr filter group_by across all_of summarise mutate select pull
#'   rename ungroup %>% .data recode_values right_join
#' @importFrom tidyr replace_na
#' @importFrom quitte revalue.levels as.quitte
#' @importFrom magclass complete_magpie as.magpie getItems<- collapseNames
#'   getItems
#' @importFrom mrcommonsenergy toolSplitBiomass


calcIEA_EEI <- function(subtype = c("buildings", "buildings_reporting"), # nolint: object_name_linter.
                        mixData = FALSE) {

  # PARAMETERS -----------------------------------------------------------------

  subtype <- match.arg(subtype)

  # energy unit conversion PJ -> EJ
  pj2ej <- 1e-3 #nolint object_name_linter

  data <- readSource("IEA_EEI", convert = TRUE)





  # PROCESS DATA ---------------------------------------------------------------

  if (subtype == "buildings") {

    data <- as.quitte(data)

    ## get mappings ====

    # GDP per capita
    gdppop <- calcOutput("GDPpc",
                         scenario = "SSP2",
                         average2020 = FALSE,
                         aggregate = FALSE)

    # enduse and carrier mapping
    enduseMap <- toolGetMapping(name = "enduseMap_IEA-EEI.csv",
                                type = "sectoral",
                                where = "mredgebuildings") %>%
      pull("EDGE", "IEA_EEI")

    carrierMap <- toolGetMapping(name = "carrierMap_IEA-EEI.csv",
                                 type = "sectoral",
                                 where = "mredgebuildings") %>%
      pull("EDGE", "IEA_EEI")


    ## aggregate ====

    dataAgg <- data %>%
      # filter residential and service data and do some pre-processing
      rename("carrier" = "ITEM",
             "enduse"  = "ENDUSE") %>%
      filter(.data[["enduse"]] %in% names(enduseMap),
             .data[["carrier"]] %in% names(carrierMap)) %>%
      # revalue carrier/enduse names
      revalue.levels(carrier = carrierMap,
                     enduse  = enduseMap) %>%
      # sum up service and residential data
      group_by(across(-all_of("value"))) %>%
      summarise(value = sum(.data$value, na.rm = isTRUE(mixData)),
                .groups = "drop") %>%
      # only keep region/periods with data
      group_by(across(all_of(c("region", "period", "enduse")))) %>%
      filter(any(.data$value > 0)) %>%
      ungroup() %>%
      # convert unit to EJ
      mutate(value = replace_na(.data[["value"]], 0) * pj2ej)


    # aggregate all space_cooling energy carriers to electricity
    coolingDemand <- dataAgg %>%
      filter(.data$enduse == "space_cooling") %>%
      mutate(carrier = "elec") %>%
      group_by(across(-all_of(c("value")))) %>%
      summarise(value = sum(.data$value)) %>%
      ungroup()


    # split biomass into traditional + modern biomass and merge aggregated space cooling data
    data <- dataAgg %>%
      filter(.data$enduse != "space_cooling") %>%
      rbind(coolingDemand) %>%
      select("region", "period", "carrier", "enduse", "value") %>%
      as.quitte() %>%
      as.magpie() %>%
      toolSplitBiomass(gdppop) %>%
      toolCountryFill(verbosity = 2)

  } else if (subtype == "buildings_reporting") {

    map <- toolGetMapping("IEA_EEI.csv", type = "reportingVariables",
                          where = "mredgebuildings") %>%
      select(-"comment") %>%
      filter(.data$variable != "")

    # map to reporting variables
    data <- data %>%
      as_tibble() %>%
      right_join(map, by = c("ITEM", "ENDUSE")) %>%
      group_by(across(all_of(c(region = "COUNTRY", period = "TIME", "variable")))) %>%
      summarise(value = sum(.data$value), .groups = "drop") %>%
      toolSumResCom() %>%
      mutate(unit = recode_values(sub("^([^\\|]+)\\|.*$", "\\1", .data$variable),
                                  "Emi"        ~ "Mt CO2/yr",
                                  "FE"         ~ "PJ/yr",
                                  "Stock"      ~ "bn m2",
                                  "StockShare" ~ "bn m2"), # actually percent
             .before = "value") %>%
      as.magpie()

    # compute floor space stock by space heating carrier
    shares <- grep("StockShare", getItems(data, "variable"), value = TRUE)
    data[, , shares] <- data[, , shares] / 100 * collapseNames(data[, , "Stock|Residential"])
    getItems(data, "variable") <- sub("StockShare", "Stock", getItems(data, "variable"))

    # unit conversion
    data <- toolUnitConversion(data,
                               inline.data.frame("from;      to;       factor",
                                                 "Mt CO2/yr; MtCO2/yr; 1",
                                                 "PJ/yr;     EJ/yr;    1e-3",
                                                 "bn m2;     mn m2;    1e3"))
  }



  # OUTPUT ---------------------------------------------------------------------

  return(list(x = data,
              weight = NULL,
              unit = "EJ/yr",
              min = 0,
              description = "IEA End Uses and Efficiency Indicators Database"))

}
