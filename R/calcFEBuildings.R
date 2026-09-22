#' Historical FE deman of uildings
#'
#' @author Robin Hasse
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass mbind setNames

calcFEBuildings <- function() {
  fe <- calcOutput("FE", aggregate = FALSE)
  vars <- grep("Buildings", getItems(fe, 3), value = TRUE)
  fe <- fe[, , vars]
  fe <- mbind(fe,
              setNames(fe[, , "FE|Buildings|Solids|Biomass (EJ/yr)"],
                       "FE|Buildings|Biomass (EJ/yr)"),
              setNames(fe[, , "FE|Buildings|Solids|Fossil (EJ/yr)"],
                       "FE|Buildings|Coal (EJ/yr)"))
  return(list(x = fe,
              unit = "EJ/yr",
              min = 0,
              description = "Final energy demand of buildings"))
}
