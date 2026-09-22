toolSumResCom <- function(x) {

  subsec <- "(Residential|Commercial)"

  xBuild <- x %>%
    mutate(subsector = case_when(grepl("Residential", .data$variable) ~ "Residential",
                                 grepl("Commercial", .data$variable)  ~ "Commercial",
                                 .default = NA),
           variableBase = sub(subsec, "Buildings", .data$variable)) %>%
    filter(!is.na(.data$subsector)) %>%
    group_by(across(-all_of(c("value", "subsector", "variable")))) %>%
    filter(dplyr::n_distinct(.data$subsector) == 2,
           all(!is.na(.data$value))) %>%
    summarise(
      value = sum(.data$value),
      variable = .data$variableBase[[1]],
      .groups = "drop"
    ) %>%
    select(-"variableBase")

  rbind(x, xBuild)
}
