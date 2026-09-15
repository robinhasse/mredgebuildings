#' Sum Residential and Commercial reporting variables to total Buildings
#'
#' If variables differ only by the subsector tag *Residential* and *Commercial*
#' and are identical otherwise, this function sums them under the identical
#' variable name with the subsector tag replaced by *Buildings*.
#'
#' @param x data.frame with `variable` and `value`columns
#' @returns data.frame with additional rows that hold summed variables
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr %>% .data mutate case_when filter group_by across summarise
#'   all_of n_distinct select

toolSumResCom <- function(x) {

  subsec <- "(Residential|Commercial)"

  xBuild <- x %>%
    mutate(subsector = case_when(grepl("Residential", .data$variable) ~ "Residential",
                                 grepl("Commercial", .data$variable)  ~ "Commercial",
                                 .default = NA),
           variableBase = sub(subsec, "Buildings", .data$variable)) %>%
    filter(!is.na(.data$subsector)) %>%
    group_by(across(-all_of(c("value", "subsector", "variable")))) %>%
    filter(n_distinct(.data$subsector) == 2,
           all(!is.na(.data$value))) %>%
    summarise(
      value = sum(.data$value),
      variable = .data$variableBase[[1]],
      .groups = "drop"
    ) %>%
    select(-"variableBase")

  rbind(x, xBuild)
}
