#' Weibull lifetime distribution parameters
#'
#' Parameters for the lifetime of heating systems are taken from a very
#' detailed EIA publication for building sector appliances and equipment. The
#' range of the building shell lifetime is taken from Skarning et al. 2017.
#'
#' @source https://www.eia.gov/analysis/studies/buildings/equipcosts/pdf/full.pdf
#' @source http://dx.doi.org/10.1016/j.enbuild.2017.01.080
#'
#' @param subtype character, type of asset (either 'building', 'heatingSystem'
#'   or 'buildingShell')
#' @param granularity character, name of BRICK granularity
#' @returns MagPIE object with Weibull lifetime distribution parameters
#'
#' @author Robin Hasse
#'
#' @importFrom madrat readSource calcOutput toolGetMapping
#' @importFrom magclass add_dimension as.magpie mselect getSets mbind
#' @importFrom quitte inline.data.frame
#' @importFrom dplyr .data %>% mutate filter everything pull
#'   right_join
#' @importFrom tidyr pivot_longer
#' @export

calcLifetimeParams <- function(subtype, granularity = NULL) {

  # FUNCTIONS ------------------------------------------------------------------

  # find Weibull parameters to given mean and standard deviation
  approxWeibull <- function(m, s, scale = 20, shape = 3, eps = 1E-5, iMax = 100) {
    speed <- c(scale = 1.2, shape = 1)
    calS <- function(scale, shape) {
      scale * sqrt(gamma(1 + 2 / shape) - gamma(1 + 1 / shape)^2)
    }
    calM <- function(scale, shape) {
      scale * gamma(1 + 1 / shape)
    }

    for (i in seq_len(iMax)) {

      sApprox <- calS(scale, shape)
      mApprox <- calM(scale, shape)

      scale <- scale * (m / mApprox)^speed[["scale"]]
      shape <- shape / (s / sApprox)^speed[["shape"]]

      if (all(abs(c(m - mApprox, s - sApprox)) < eps)) break
    }

    if (i == iMax) {
      warning("Approximation stopped after the maximum of ", iMax, " iterations. ",
              "The tolerance might not be fulfilled.")
    }

    return(list(scale = scale, shape = shape))
  }

  # find Weibull parameters that result in given percentiles
  weibullFromRange <- function(x, probFrom, probTo) {
    x$shape <- log(log(1 - probTo) / log(1 - probFrom), x$to / x$from)
    x$scale <- x$from / (-log(1 - probFrom))^(1 / x$shape)
    x
  }



  # READ & CALCULATE -----------------------------------------------------------


  switch(subtype,

    ## Buildings ====

    building = {

      ### Deetman et al. ####

      res <- readSource("Deetman2020", "residential")
      com <- readSource("Deetman2020", "commercial")

      params <- do.call(mbind, lapply(c("SFH", "MFH", "Com"), function(typ) {
        switch(typ, SFH = res, MFH = res, Com = com) %>%
          add_dimension(add = "typ", nm = typ)
      }))

      params <- mselect(params, variable = c("scale", "shape"))


      ### Sandberg et al. ####

      resEUR <- readSource("Sandberg") %>%
        as_tibble() %>%
        select(-"unit") %>%
        group_by(across(-all_of(c("constructionPeriod", "value")))) %>%
        summarise(value = mean(.data$value, na.rm = TRUE), .groups = "drop") %>%
        group_by(across(-all_of(c("region", "value")))) %>%
        mutate(value = ifelse(is.na(.data$value),
                              mean(.data$value, na.rm = TRUE),
                              .data$value)) %>%
        ungroup() %>%
        pivot_wider(names_from = "variable") %>%
        mutate(params = Map(approxWeibull,
                            m = .data$averageLifetime,
                            s = .data$averageLifetime / 2, # roughly matched Deetman distributions
                            scale = 1.1 * .data$averageLifetime)) %>%
        tidyr::unnest_longer("params", indices_to = "variable", values_to = "value") %>%
        select("region", "variable", "value") %>%
        as.magpie(tidy = TRUE)


      ### combine ####
      eur <- toolGetMapping("regionmappingH12.csv") %>%
        filter(.data$RegionCode == "EUR") %>%
        getElement("CountryCode")

      mselect(params, region = eur, typ = c("SFH", "MFH")) <- resEUR[eur, , ]


      description <- "Weibull lifetime distribution parameters for buildings"

    },


    ## Heating system ====

    heatingSystem = {


      ### DEA technology catalogue ####

      dea <- readSource("DEA_TechnologyCatalogues",
                        subtype = "individualHeatingPlants") %>%
        mselect(period = "y2025",
                region = "DNK",
                variable = "Technical economic lifetime") %>%
        as_tibble() %>%
        select(-"variable", -"unit")

      ### map to technologies ####
      techMap <- toolGetMapping("technologyMapping_DEA.csv",
                                type = "sectoral",
                                where = "mredgebuildings")
      colsMapped <- c("tech", "buildingType", "buildingVin")
      deaMapped <- dea %>%
        right_join(techMap,
                   by = colsMapped,
                   relationship = "many-to-many") %>%
        select(-all_of(c("region", "period", colsMapped)))


      ### derive Weibull parameters ####

      # find Weibull distributions with the central value as mean
      # assume that lower and upper estimates are two standard deviations apart
      params <- deaMapped %>%
        pivot_wider(names_from = "estimate") %>%
        mutate(params = Map(approxWeibull, m = .data$ctrl, s = (.data$upper - .data$lower) / 2),
               .keep = "unused") %>%
        tidyr::unnest_longer("params", indices_to = "variable", values_to = "value")



      # all technologies included?
      hs <- toolGetMapping("dim_hs.csv",
                           type = "sectoral", where = "brick")
      params <- params %>%
        right_join(hs["hs"], by = "hs")
      if (any(is.na(params))) {
        stop("Incomplete mapping of heating technologies.")
      }



      params <- params %>%
        select("typ", "hs", "variable", "value") %>%
        as.magpie()

      description <- "Weibull lifetime distribution parameters for heating systems"
    },




    ## Building shell ====

    buildingShell = {

      # taken from Skarning et al. 2017
      params <- inline.data.frame(
        "from; to",
        "  40; 60"
      )

      # we assume probabilities of the lower and upper value respectively
      params <- weibullFromRange(params, probFrom = 0.1, probTo = 0.9) %>%
        select(-"from", -"to") %>%
        pivot_longer(everything(), names_to = "variable") %>%
        as.magpie(datacol = "value")

      description <- "Weibull lifetime distribution parameters for the building shell"
    },


    stop("Invalid subtype: ", subtype)
  )



  # RETURN ---------------------------------------------------------------------

  # fill missing regions
  params <- toolCountryFillAvg(params, verbosity = 2, no_remove_warning = "GLO")

  # weight: FE demand
  feBuildings <- calcOutput("WeightFeBuildings", aggregate = FALSE) %>%
    mselect(period = "y2020", collapseNames = TRUE)
  if ("typ" %in% getSets(params)) {
    feBuildings <- feBuildings %>%
      mselect(typ = getItems(params, "typ"))
  } else {
    feBuildings <- dimSums(feBuildings)
  }

  # aggregate to BRICK granularity
  agg <- toolAggregateBrick(params, granularity, feBuildings)



  return(list(x = agg$x,
              weight = agg$weight,
              min = 0,
              unit = "[scale] = yr; [shape] = 1",
              description = description))
}
