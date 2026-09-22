#' Calculate Reporting variables from Odyssee
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource toolGetMapping
#' @importFrom magclass as.magpie
#' @importFrom dplyr %>% .data mutate filter right_join select left_join matches
#' @importFrom tidyr separate_wider_delim
#' @importFrom utils read.csv2

calcOdyssee <- function() {
  odyssee <- readSource("Odyssee")
  map <- toolGetMapping("Odyssee.csv",
                        type = "reportingVariables",
                        where = "mredgebuildings",
                        returnPathOnly = TRUE) %>%
    read.csv2() %>%
    select("variable", "code") %>%
    filter(.data$variable != "")

  x <- odyssee %>%
    as_tibble() %>%
    right_join(map, by = c(variable = "code"), suffix = c("", "")) %>%
    mutate(unit = as.character(.data$unit),
           value = .data$value * ifelse(.data$unit == "m2", 1e-6, 1),
           unit = ifelse(.data$unit == "m2", "mn m2", .data$unit))

  # calculate variables that need to be multiplied
  xMult <- x %>%
    filter(grepl("\\*", .data$variable)) %>%
    separate_wider_delim(cols = "variable", delim = "*", names = c("variable", "fac")) %>%
    left_join(x, by = c("region", "period", fac = "variable"), suffix = c("", "Fac")) %>%
    mutate(value = .data$value * .data$valueFac) %>%
    select(-matches("Fac$"), -"fac", -"unit", unit = "unitFac")

  x <- x %>%
    filter(!grepl("\\*", .data$variable),
           !grepl("^fac", .data$variable)) %>%
    rbind(xMult) %>%
    toolSumResCom() %>%
    as.magpie()

  return(list(x = x,
              min = 0,
              description = "Floor space and final energy variables from Odyssee"))
}
