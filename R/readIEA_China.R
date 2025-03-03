readIEA_China <- function() {
  read.csv("figure3-25.csv") %>%
    select("region", "period", "enduse", "value") %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value",
              tidy = FALSE)
}
