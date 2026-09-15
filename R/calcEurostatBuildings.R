calcEurostatBuildings <- function() {
  nrg <- readSource("EurostatBuildings", subtype = "nrg_d_hhq")

  map <- toolGetMapping("EurostatBuildings.csv",
                        type = "reportingVariables",
                        where = "mredgebuildings")
  cols <- c("nrg_bal", "siec", "unit")

  x <- nrg %>%
    as_tibble() %>%
    right_join(map, by = cols) %>%
    group_by(across(-all_of(c(cols, "value")))) %>%
    summarise(value = sum(.data$value * .data$factor),
              .groups = "drop") %>%
    select("region", "period", "variable", "value") %>%
    as.magpie()

  return(list(x = x,
              weight = NULL,
              min = 0,
              unit = "EJ/yr",
              description = "Disaggregated final energy consumption in households"))
}
