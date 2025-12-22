#' Exchange rate between USD and EUR
#'
#' @param year integer, reference year
#' @returns MER EUR in USD in given year
#'
#' @author Robin Hasse
#'
usd2eur <- function(year = 2020) {
  # A quick way to return the MER [EUR per USD] in a given year.
  GDPuc::toolConvertSingle(1, "DEU", year,
                           unit_in = "current US$MER",
                           unit_out = "current LCU")
}



#' Exchange rate between EUR and USD
#'
#' Convert EUR in given year to internal unit
#'
#' @param year integer, reference year
#' @returns MER EUR in USD in given year
#'
#' @author Robin Hasse
#'
eur2usd <- function(year = 2020) {
  GDPuc::toolConvertSingle(1, "DEU", year,
                           unit_in = "current LCU",
                           unit_out = "constant 2017 US$MER")
}
