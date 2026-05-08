#' Convert data from the Danish Energy Agency
#'
#' @param x MagPIE object
#' @param subtype character, report
#' @returns MagPIE object for all ISO countries (all NA but DNK)
#'
#' @author Robin Hasse
#'
#' @importFrom madrat toolCountryFill

convertDEA_TechnologyCatalogues <- function(x, subtype) {  # nolint: object_name_linter.
  toolCountryFill(x, verbosity = 2)
}
