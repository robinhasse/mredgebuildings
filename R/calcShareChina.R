calcShareChina <- function() {
  x <- readSource("IEA_China")
  x <- x[, , "other", invert = TRUE]
  shares <- x / dimSums(x, "enduse")
  shares <- toolCountryFill(shares, verbosity = 2)
  w <- toolCountryFill(x, 1, verbosity = 2)
  list(x = shares,
       weight = w,
       min = 0,
       max = 1,
       unit = "1",
       description = "Share of end uses in Chinese buildings final energy demand")
}
