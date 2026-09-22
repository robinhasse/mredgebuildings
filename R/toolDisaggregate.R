#' Disaggregate energy demand by carriers into end uses by each carrier
#'
#' This function takes energy demand data that differentiates only by carrier
#' and disaggregates it further such that all relevant combinations of carrier
#' and end use are differentiated. It ensures that given end use shares (across
#' all carriers) are met.
#'
#' The function essentially performs a quadratic optimisation. The constraints
#' make sure that both the carrier quantities and the end use shares are met.
#' This generally leaves infinite solutions (in problematic cases none). To get
#' to one disaggregation, the deviation from a given distribution across all
#' relevant combinations of carriers and end uses is minimised.
#'
#' @param data data.frame with energy demand data differentiated by carriers
#'   that is to be disaggregated
#' @param enduseShares data.frame with end use shares that have to be met. These
#'   shares might be given at aggregated regional resolution if
#'   \code{regionmapping} is provided.
#' @param exclude data.frame with the columns \code{carrier} and
#'   \code{enduse} that should list all  combinations of the two that are
#'   excluded. All remaining combinations of the carriers in \code{data} and the
#'   end uses in \code{enduseShares} are considered.
#' @param dataDisagg data.frame similar to \code{data} but already disaggregated
#'   by carriers and end uses. The average distribution of its disaggregation
#'   will be used as the target distribution for the minisation.
#' @param regionMapping data.frame with the columns \code{region} and
#'   \code{regionAgg} that maps the regions between \code{data} and
#'   \code{enduseShares}.
#' @param outliers list of regions where naive disaggregation estimate shall
#'   be used.
#' @param forceShares data frame with shares to enforce for carrier-enduse combinations
#' with single year data points per region.
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom quitte interpolate_missing_periods
#' @importFrom dplyr %>% .data mutate group_by ungroup across all_of left_join
#'   semi_join group_modify select summarise filter anti_join
#' @importFrom tidyr pivot_wider replace_na
#' @export

toolDisaggregate <- function(data,
                             enduseShares,
                             outliers = NULL,
                             exclude = NULL,
                             dataDisagg = NULL,
                             regionMapping = NULL,
                             forceShares = NULL) {


  # CHECK AND PREPARE INPUT ----------------------------------------------------

  ## columns ====

  # check that all required columns are present
  checkCols <- function(df, dfName, cols) {
    if (!is.null(df)) {
      missingCols <- setdiff(cols, colnames(df))
      if (length(missingCols) > 0) {
        stop("The input '", dfName, "' is missing the following columns: ",
             paste(missingCols, collapse = ", "))
      }
    }
  }

  checkCols(data, "data", c("region", "carrier", "value"))
  checkCols(enduseShares, "enduseShares", c("region", "enduse", "value"))
  checkCols(exclude, "exclude", c("carrier", "enduse"))
  checkCols(dataDisagg, "dataDisagg", c("region", "enduse", "value", "enduse"))
  checkCols(regionMapping, "regionMapping", c("region", "regionAgg"))

  if (is.null(outliers)) outliers <- c()


  ## region mapping ====

  # no regionMapping needed if data and enduseShares share regional resolution
  if (is.null(regionMapping)) {
    missingRegions <- setdiff(data[["region"]], enduseShares[["region"]])
    if (length(missingRegions) > 0) {
      stop("If no regionMapping is provided, data and enduseShares need to ",
           "have the same regional resolution. But the follwing regions in ",
           "data are missing in enduseShares: ",
           paste(missingRegions, collapse = ", "))
    }
    regionMapping <- data.frame(region    = unique(data[["region"]]),
                                regionAgg = unique(data[["region"]]))
  } else {
    missingRegions <- setdiff(data[["region"]], regionMapping[["region"]])
    if (length(missingRegions) > 0) {
      stop("The follwing regions in data are missing in regionMapping: ",
           paste(missingRegions, collapse = ", "))
    }
  }


  ## end use shares ====

  # normalise to make sure that shares add up to 1
  enduseShares <- enduseShares %>%
    group_by(across(-all_of(c("enduse", "value")))) %>%
    mutate(value = proportions(.data[["value"]])) %>%
    ungroup()


  ## carrier end use mapping

  # all combinations of carriers and end uses except those excluded
  carrierEnduseMapping <- expand.grid(carrier = unique(data[["carrier"]]),
                                      enduse = unique(enduseShares[["enduse"]]))
  if (!is.null(exclude)) {
    carrierEnduseMapping <- carrierEnduseMapping %>%
      anti_join(exclude, by = c("carrier", "enduse"))
  }


  # GENERATE ESTIMATE ----------------------------------------------------------

  if (is.null(dataDisagg)) {
    # naive estimate: overall carrier distribution applies to all end uses
    estimate <- data %>%
      left_join(regionMapping, by = "region") %>%
      left_join(carrierEnduseMapping,
                by = "carrier",
                relationship = "many-to-many") %>%
      join_all(enduseShares, by = c(regionAgg = "region")) %>%
      mutate(estimate = .data[["value.x"]] * .data[["value.y"]]) %>%
      select(-"value.x", -"value.y")

  } else {
    # use carrier-end use distribution from given disaggregated data
    # missing periods get the average distribution across all given regions
    estimateRegional <- dataDisagg %>%
      semi_join(carrierEnduseMapping, by = c("carrier", "enduse"))

    # calculate global shares
    estimateGlobal <- estimateRegional %>%
      group_by(across(-all_of(c("region", "value")))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE),
                .groups = "drop") %>%
      group_by(across(-all_of(c("enduse", "value")))) %>%
      mutate(share = proportions(.data[["value"]])) %>%
      select(-"value")

    # calculate regional shares
    estimateRegional <- estimateRegional %>%
      group_by(across(-all_of(c("enduse", "value")))) %>%
      mutate(share = proportions(.data[["value"]])) %>%
      select(-"value")

    # replace outlier regions with EU shares rather than global shares
    estimateRegional <- estimateRegional %>%
      left_join(regionMapping, by = "region") %>%
      left_join(enduseShares %>%
                  select("region", "period", "enduse", "value"),
                by = c("regionAgg" = "region", "period", "enduse")) %>%
      mutate(share = ifelse(!is.na(.data[["share"]]),
                            .data[["share"]],
                            ifelse(.data[["regionAgg"]] %in% outliers,
                                   .data[["value"]],
                                   .data[["share"]]))) %>%
      select(-"value", -"regionAgg")

    # calculate estimations
    estimate <- data %>%
      join_all(estimateRegional) %>%
      join_all(estimateGlobal, exclude = "share",
               suffix = c("Regional", "Global")) %>%
      mutate(estimate = .data$value *
               ifelse(is.na(.data$shareRegional),
                      .data$shareGlobal,
                      .data$shareRegional)) %>%
      select(-"value", -"shareRegional", -"shareGlobal")
  }


  # enforce existing end-use specific carrier shares if given
  if (!is.null(forceShares)) {
    # exclude unwanted enduse-carrier combinations and re-normalize
    forceShares <- forceShares %>%
      anti_join(exclude, by = c("enduse", "carrier")) %>%
      filter(!is.na(.data$value)) %>%
      group_by(across(all_of(c("region", "period", "enduse")))) %>%
      mutate(value = proportions(.data$value)) %>%
      ungroup()

    estimate <- .enforceShares(estimate, forceShares)
  }


  # DISAGGREGATE  --------------------------------------------------------------

  # Disaggregate within each group of aggregated regions and periods
  dataOut <- data %>%

    # map regions from data to agg. regions from enduseShares
    left_join(regionMapping, by = "region") %>%

    # total demand in each agg. region
    group_by(across(-all_of(c("region", "carrier", "value")))) %>%
    mutate(total = sum(.data$value)) %>%

    # map carriers to relevant end uses
    left_join(carrierEnduseMapping, by = "carrier",
              relationship = "many-to-many") %>%

    # share of end uses in total demand within each agg. region
    join_all(enduseShares %>%
               rename(enduseShare = "value"),
             by = c(regionAgg = "region"),
             relationship = "many-to-many") %>%

    # total demand per end use in each agg. region
    mutate(enduseTotal = .data$enduseShare * .data$total) %>%

    # estimated disaggregation that should be met as closely as possible
    join_all(estimate) %>%
    mutate(estimate = replace_na(.data$estimate, 0))


  dataOut <- dataOut %>%

    # remove region-carrier combinations with zero demand to reduce problem size
    filter(.data[["value"]] > 0) %>%

    # disaggregate demand  within each agg. region
    group_by(across(-all_of(c("region", "carrier", "enduse", "estimate", "value",
                              "enduseTotal", "enduseShare", "total")))) %>%
    group_modify(.disaggregate) %>%
    ungroup() %>%

    # calculate carrier-specific shares
    group_by(across(all_of(c("region", "period", "carrier")))) %>%
    mutate(share = proportions(.data[["pred"]])) %>%
    ungroup() %>%

    # apply shares on original fe data
    mutate(value = .data[["value"]] * .data[["share"]]) %>%
    select(-"share", -"pred") %>%

    # recover region-carrier combinations with zero demand
    join_all(dataOut %>% select(-"estimate", -"value", -"enduseTotal",
                                -"enduseShare", -"total"),
             .direction = "right") %>%
    mutate(value = replace_na(.data[["value"]], 0)) %>%

    # remove information on precision (can be kept for debugging)
    select(-"precision")




  return(dataOut)
}



#' Disaggregate energy demand within an aggregated region
#'
#' Disaggregate regional energy demand per carrier by end use while meeting the
#' end use shares in the aggregated region.
#'
#' The function first tries to find a solution that satisfies both the regional
#' carrier and the overall end use constraints. If there is no solution, another
#' optimisation is run that tries to also minimise deviations from the end use
#' quantities but removes them from the constraints.
#'
#' @param subset data frame for one period and aggregated region
#' @param key named vector with specification of the subset group (not used)
#'
#' @importFrom dplyr %>%  .data mutate select all_of full_join
#' @importFrom tidyr unite pivot_wider replace_na
#' @importFrom quadprog solve.QP
#' @importFrom purrr reduce

.disaggregate <- function(subset, key) {

  # types of constraints:
  #   - total demand per carrier in each region has to be met
  #   - total demand per end use in each agg. region has to be met
  #   - all disaggregated quantities have to be larger or equal zero
  constraints <- list(carrier    = c("region", "carrier"),
                      enduse     = "enduse",
                      lowerBound = c("region", "carrier", "enduse"))

  # weight that increases the importance of minimising the deviation from given
  # end use quantities over minimising the deviations from the estimate
  # the estimate is often times rather arbitrary -> high weight
  weight <- 100

  # percentage of naive estimate used as lower bound to avoid zero allocations for
  # regions/periods where there is non-vanishing carrier and end-use demand
  lowerBoundShare <- 0.05


  # PREPARE DATA ---------------------------------------------------------------


  # target values to get to as closely as possible
  variables <- subset %>%
    select("region", "carrier", "enduse", "estimate") %>%
    unique()

  # rescale weight with the number of individual regions to have comparable
  # weighting independent of the size of the aggregated region
  weight <- weight / length(unique(variables[["region"]]))

  # right-hand side of the constraints
  constraintRHS <- lapply(names(constraints), function(c) {
    subset %>%
      mutate(value = switch(c,
                            carrier    = .data[["value"]],
                            enduse     = .data[["enduseTotal"]],
                            lowerBound = lowerBoundShare * .data$estimate)) %>%
      unite("rhs", all_of(constraints[[c]]), sep = "-", remove = FALSE) %>%
      select("rhs", "value") %>%
      unique()
  })
  names(constraintRHS) <- names(constraints)

  # number of constraints
  nConstraints <- as.numeric(lapply(constraintRHS, nrow))

  # constraint matrix: maps disaggregated to aggregated quantities
  constraintMatrix <- lapply(names(constraints), function(c) {
    subset %>%
      select("region", "carrier", "enduse") %>%
      unique() %>%
      mutate(value = 1) %>%
      unite("cols", all_of(constraints[[c]]), sep = "-", remove = FALSE) %>%
      pivot_wider(names_from = "cols", values_fill = 0)
  })
  names(constraintMatrix) <- names(constraints)

  # identity matrix
  identityMatrix <- diag(nrow(variables))

  # first look for exact solution
  # If there is none, find one that matches end use quantities closely
  for (precision in c("exact", "close")) {
  # for (precision in c("close")) { #nolint

    # BUILD MATRICES -----------------------------------------------------------

    if (precision == "exact") {

      dMat <- identityMatrix

      dvec <- variables %>%
        getElement("estimate")

      aMat <- constraintMatrix %>%
        reduce(full_join, by = c("region", "carrier", "enduse")) %>%
        select(-"region", -"carrier", -"enduse") %>%
        as.matrix()

      bvec <- constraintRHS %>%
        do.call(what = rbind) %>%
        getElement("value")

      meq  <- sum(nConstraints[1:2])

    } else if (precision == "close") {

      enduseMatrix <- constraintMatrix[["enduse"]] %>%
        select(-"region", -"carrier", -"enduse") %>%
        as.matrix()
      objectiveMatrix <- rbind(identityMatrix, weight * t(enduseMatrix))

      dMat <- t(objectiveMatrix) %*% objectiveMatrix

      objectiveRHS <- constraintRHS[["enduse"]] %>%
        getElement("value")
      objectiveRHS <- variables %>%
        getElement("estimate") %>%
        c(weight * objectiveRHS)

      dvec <- t(objectiveMatrix) %*% objectiveRHS

      aMat <- constraintMatrix[c("carrier", "lowerBound")] %>%
        reduce(full_join, by = c("region", "carrier", "enduse")) %>%
        select(-"region", -"carrier", -"enduse") %>%
        as.matrix()

      bvec <- constraintRHS[c("carrier", "lowerBound")] %>%
        do.call(what = rbind) %>%
        getElement("value")

      meq  <- nConstraints[1]
    }



    # SOLVE --------------------------------------------------------------------

    # solve quadratic problem (QP)
    # - exact:
    #   minimise quadratic deviation from estimate
    #   subject to matching regional carrier totals and overall end use
    #   quantities exactly with non-negative disaggregated quantities
    # - close:
    #   minimise quadratic deviation from end use shares (and to lesser extend
    #   deviations from estimate)
    #   subject to matching regional carrier totals with non-negative
    #   disaggregated quantities
    r <- tryCatch(solve.QP(dMat, dvec, aMat, bvec, meq),
                  error = function(e) NULL)

    # no need to lower the ambition if a solution is found
    if (!is.null(r)) {
      break
    }

  }



  # RETURN ---------------------------------------------------------------------

  subsetOut <- subset %>%
    select("region", "carrier", "enduse", "value")

  if (is.null(r)) {
    subsetOut[["pred"]] <- as.numeric(NA)
    subsetOut[["precision"]] <- as.character(NA)
  } else {
    subsetOut[["pred"]] <- r[["solution"]]
    subsetOut[["precision"]] <- precision
  }

  subsetOut[replace_na(subsetOut[["pred"]], 0) < 1E-6 &
              !is.na(subsetOut[["pred"]]),
            "pred"] <- 0

  return(subsetOut)
}



#' Enforce Shares into Energy Demand Estimates
#'
#' Adjusts energy demand estimates to meet specified carrier share targets
#' within end-uses while preserving total carrier demands. This function applies
#' proportional adjustments to align existing shares with target shares while
#' maintaining energy balance constraints. It handles missing combinations, zero
#' values, and special cases where target shares exist for carriers with no
#' historical demand. The adjustments are applied consistently across all time
#' periods with careful rescaling to ensure carrier totals remain unchanged.
#'
#' Note: This function requires \code{forceShares} data to contain only one
#' period per region-enduse-carrier combination to work correctly.
#'
#' @param estimate data frame with energy carrier demand estimates
#' @param forceShares data frame with shares to enforce for carrier-enduse combinations
#'
#' @returns data frame with adjusted estimates preserving carrier totals
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr left_join right_join semi_join group_by summarise filter
#'   mutate select bind_rows across all_of ungroup anti_join distinct
#' @importFrom tidyr crossing

.enforceShares <- function(estimate, forceShares) {
  # add any region-carrier-enduse combinations from forceShares that are missing in estimate
  missingCombinations <- forceShares %>%
    select("region", "carrier", "enduse") %>%
    distinct() %>%
    anti_join(estimate %>% select("region", "carrier", "enduse") %>% distinct(),
              by = c("region", "carrier", "enduse"))

  if (nrow(missingCombinations) > 0) {
    # create zero-value rows for all missing combinations across all periods
    estimate <- bind_rows(
      estimate,
      missingCombinations %>%
        left_join(estimate %>%
                    select("region", "enduse", "unit") %>%
                    distinct(),
                  by = c("region", "enduse")) %>%
        crossing(estimate %>%
                   select("period") %>%
                   distinct()) %>%
        mutate(estimate = 0) %>%
        select("region", "period", "unit", "carrier", "enduse", "estimate")
    )
  }

  # calculate existing carrier shares within each end-use for reference periods
  existingShares <- estimate %>%
    semi_join(forceShares, by = c("region", "period", "enduse")) %>%
    group_by(across(all_of(c("region", "enduse")))) %>%
    mutate(existingShare = .data$estimate / sum(.data$estimate, na.rm = TRUE)) %>%
    ungroup()

  # combine with target shares, ensuring all forceShares combinations are included
  combinedShares <- existingShares %>%
    right_join(forceShares %>%
                 rename("forceShare" = "value"),
               by = c("region", "period", "carrier", "enduse")) %>%
    filter(!is.na(.data$forceShare))

  # handle normal cases where existing share > 0 or both shares are zero
  normalCases <- combinedShares %>%
    filter(.data$existingShare > 0 | (.data$existingShare == 0 & .data$forceShare == 0)) %>%
    mutate(adjustmentFactor = .data$forceShare / .data$existingShare,
           adjustmentFactor = replace_na(.data$adjustmentFactor, 0)) %>%
    select("region", "carrier", "enduse", "adjustmentFactor")

  # handle special cases where existing share = 0 but forced share > 0
  specialCases <- combinedShares %>%
    filter(.data$existingShare == 0 & .data$forceShare > 0) %>%
    select("region", "carrier", "enduse", "forceShare")

  # apply normal adjustment factors to all periods
  if (nrow(normalCases) > 0) {
    estimateWithFactors <- estimate %>%
      left_join(normalCases, by = c("region", "carrier", "enduse")) %>%
      mutate(adjustedEstimate = ifelse(!is.na(.data$adjustmentFactor),
                                       .data$estimate * .data$adjustmentFactor,
                                       .data$estimate)) %>%
      select(-"adjustmentFactor")
  } else {
    estimateWithFactors <- estimate %>%
      mutate(adjustedEstimate = .data$estimate)
  }

  # handle special cases by applying forced shares directly to total enduse demand
  if (nrow(specialCases) > 0) {
    # calculate total energy demand per enduse to apply forced shares
    enduseTotals <- estimateWithFactors %>%
      group_by(across(all_of(c("region", "period", "enduse", "unit")))) %>%
      summarise(totalEnduse = sum(.data$adjustedEstimate, na.rm = TRUE), .groups = "drop")

    # calculate estimates based on forced share of total enduse demand
    estimateWithFactors <- estimateWithFactors %>%
      left_join(specialCases, by = c("region", "carrier", "enduse")) %>%
      left_join(enduseTotals, by = c("region", "period", "enduse", "unit")) %>%
      mutate(adjustedEstimate = ifelse(!is.na(.data$forceShare),
                                       .data$totalEnduse * .data$forceShare,
                                       .data$adjustedEstimate)) %>%
      select(-"forceShare", -"totalEnduse")
  }

  # rescale to preserve original carrier totals with exact preservation
  finalEstimate <- estimateWithFactors %>%
    # flag values that were adjusted by forced shares
    left_join(bind_rows(normalCases %>%
                          mutate(isAdjusted = TRUE) %>%
                          select("region", "carrier", "enduse", "isAdjusted"),
                        specialCases %>%
                          mutate(isAdjusted = TRUE) %>%
                          select("region", "carrier", "enduse", "isAdjusted")),
              by = c("region", "carrier", "enduse")) %>%
    mutate(isAdjusted = !is.na(.data$isAdjusted)) %>%
    # group by carrier to preserve carrier totals
    group_by(across(all_of(c("region", "period", "carrier", "unit")))) %>%
    # calculate totals and determine how much to rescale non-adjusted values
    mutate(
      originalTotal  = sum(.data$estimate, na.rm = TRUE),
      adjustedSum    = sum(.data$adjustedEstimate[.data$isAdjusted], na.rm = TRUE),
      remainingTotal = .data$originalTotal - .data$adjustedSum,
      nonAdjustedSum = sum(.data$adjustedEstimate[!.data$isAdjusted], na.rm = TRUE)
    ) %>%
    # rescale non-adjusted values to exactly match remaining total
    mutate(
      scaleFactor   = ifelse(.data$nonAdjustedSum > 0, .data$remainingTotal / .data$nonAdjustedSum, 0),
      finalEstimate = ifelse(.data$isAdjusted,
                             .data$adjustedEstimate,
                             ifelse(.data$nonAdjustedSum > 0,
                                    .data$adjustedEstimate * .data$scaleFactor,
                                    0))

    ) %>%
    # handle edge cases where all values are adjusted or scaling would produce negative values
    mutate(
      needsSpecialHandling = (.data$nonAdjustedSum == 0 & .data$remainingTotal != 0) | (.data$scaleFactor < 0),
      # if all values adjusted but totals don't match, rescale all proportionally
      totalScaleFactor = ifelse(.data$needsSpecialHandling & sum(.data$adjustedEstimate) > 0,
                                .data$originalTotal / sum(.data$adjustedEstimate),
                                1),
      finalEstimate = ifelse(.data$needsSpecialHandling,
                             .data$adjustedEstimate * .data$totalScaleFactor,
                             .data$finalEstimate)
    ) %>%
    ungroup() %>%
    select("region", "period", "unit", "carrier", "enduse", "estimate" = "finalEstimate")

  return(finalEstimate)
}
