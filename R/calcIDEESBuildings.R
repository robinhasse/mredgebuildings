#' Calculate Reporting variables from JRC IDEES
#'
#' @importFRom madrat readSource toolGetMapping toolCountryFill
#' @importFrom magclass mbind as.magpie mselect
#' @importFrom dplyr %>% .data mutate filter all_of left_join group_by summarise
#'   across right_join  select
#' @importFrom tidyr separate_wider_delim
#' @importFrom quitte inline.data.frame

calcIDEESBuildings <- function() {
  idees <- mbind(readSource("JRC_IDEES", subtype = "Residential_2021"),
                 readSource("JRC_IDEES", subtype = "Tertiary_2021"))
  map <- toolGetMapping("IDEESBuildings.csv",
                        type = "reportingVariables",
                        where = "mredgebuildings") %>%
    filter(.data$variable != "") %>%
    select(-"vintage")

  x <- idees %>%
    mselect(code = unique(map$code)) %>%
    toolUnitConversion(inline.data.frame(
      "from;        to;        factor",
      "ksqm;        mn m2;     1e-3",
      "ktoe;        EJ/yr;     4.1868E-5",
      "kWh;         EJ/yr;     3.6E-12",
      "ktCO2;       Mt CO2/yr; 1e-3"
    )) %>%
    as_tibble() %>%
    right_join(map, by = c("code")) %>%
    group_by(across(-all_of(c("code", "value")))) %>%
    summarise(value = ifelse(all(grepl("perSqm", .data$variable)),
                             mean(.data$value), # negligible error from unweighted average of UE intensity
                             sum(.data$value)),
              .groups = "drop")

  # recover floor space by space heating
  .extract <- function(x, patterns) {
    for (pattern in patterns) {
      x <- x[grepl(pattern, x$variable), ]
    }
    categories <- x$variable
    for (pattern in patterns) {
      categories <- sub(pattern, "", categories)
    }
    x$category <- categories
    x
  }
  xFloor <- x %>%
    .extract(c("^UEperSqm\\|", "Space heating\\|")) %>%
    left_join(.extract(x, c("^UE\\|", "Space heating\\|")),
              by = c("region", "period", "category"),
              suffix = c("UEperSqm", "UE")) %>%
    mutate(value = .data$valueUE / .data$valueUEperSqm * 1e-6,
           unit = "mn m2",
           variable = paste0("Stock|", .data$category)) %>%
    select(names(x))

  x <- x %>%
    filter(!grepl("^UEperSqm\\|", .data$variable)) %>%
    rbind(xFloor) %>%
    toolSumResCom() %>%
    as.magpie() %>%
    toolCountryFill(verbosity = 2)

  return(list(x = x,
              min = 0,
              description = "Energy, emission and floorspace variables from JRC IDEES 2021"))
}
