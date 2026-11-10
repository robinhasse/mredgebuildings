#' Floor-area specific heating capacity
#'
#' Rough assumption based on online reference values scaled with U-values from
#' Hotmaps and heating degree days (HDD).
#'
#' This is a very rough estimation that neglects regional differences and many
#' other effects. It should be replace by bottom-up calculation based on climate
#' data.
#'
#' @param swissFormular boolean, apply the simplistic swiss formular?
#' @return MagPIE object with floor-area specific heating capacity
#'
#' @author Robin Hasse
#'
#' @source https://www.heizung.de/ratgeber/diverses/heizleistung-berechnen-gruende-und-ablauf.html
#'
#' @importFrom stats median
#' @importFrom madrat calcOutput
#' @importFrom quitte as.quitte inline.data.frame
#' @importFrom magclass as.magpie getItems
#' @importFrom dplyr %>% .data filter select mutate right_join left_join
#'   full_join group_by across all_of reframe summarise
#' @export

calcHeatingCapacity <- function(swissFormular = FALSE) {

  # period for weight (rather arbitrary)
  t <- 2020

  if (swissFormular) {
    flh <- 2300 # in h/yr
    ueDem <- calcOutput("UEdemand", aggregate = FALSE) %>%
      mselect(enduse = "space_heating", collapseNames = TRUE)
    heatCap <- ueDem / flh
  } else {
    # reference heating capacity from online tool (SFH, after 1994)
    heatCapRef <- 0.100  # in kW/m2


    # heating degree days
    hdd <- calcOutput("HDDCDD", tlimit = "23", aggregate = FALSE) %>%
      as.quitte(na.rm = TRUE) %>%
      filter(.data[["period"]] == t,
             .data[["scenario"]] == "ssp2",
             .data[["rcp"]] == "rcpNoC",
             .data[["variable"]] == "HDD") %>%
      select("region", "period", hdd = "value")

    # avoid zero
    hdd <- hdd %>%
      mutate(hdd = pmax(.data[["hdd"]],
                        0.1 * median(.data[["hdd"]][.data[["hdd"]] > 0])))

    # U-value
    uval <- calcOutput("UValue", aggregate = FALSE) %>%
      as.quitte(na.rm = TRUE) %>%
      select("region", "vin", "typ", uval = "value")

    # scale reference value with U-value and HDD
    heatCap <- uval %>%
      full_join(hdd, by = "region") %>%
      mutate(value = heatCapRef *
               (.data[["uval"]] * .data[["hdd"]]) /
               (.data[["uval"]] * .data[["hdd"]])[.data[["region"]] == "DEU" &
                                                    .data[["vin"]] == "1990-1999" &
                                                    .data[["typ"]] == "SFH"]) %>%
      select("region", "period", "typ", "vin", "value")

    # convert to magpie object
    heatCap <- heatCap %>%
      as.magpie(spatial = "region", datacol = "value")
  }


  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE)[, t, ]

  return(list(x = heatCap,
              unit = "kW/m2",
              weight = feBuildings,
              min = 0,
              description = "Floor-space specific heating system capacity"))
}
