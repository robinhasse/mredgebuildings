#' Prepare EU-SILC data.
#'
#' Read selected micro data of the EU-SILC survey and aggregate it.
#'
#' The aggregation should serve the needs of later processing but suppress the
#' identification of respondents.
#'
#' @author Robin Hasse
#'
#' @param subtype Character, type of data.
#' @param variable Character vector with variable codes to include.
#'
#' @importFrom utils unzip read.csv
#' @importFrom dplyr rename any_of full_join
#' @importFrom tidyr pivot_longer
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
#' @importFrom purrr reduce
#' @export

prepareSILC <- function(subtype, vars = NULL, dims = NULL, weight = "DB090") {



  # PREPARE --------------------------------------------------------------------


  ## settings ====

  # used if vars is NULL
  defaultVars <- list(
    noHhousehold = list(
      code = NULL,
      agg = "count"),
    householdSize = list(
      code = "HX040",
      agg = "mean"),
    dwellingSize = list(
      code = c("HD035", "HC020"),
      agg = "mean")
  )

  # mapping of dimensions
  dimMappingPath <- toolGetMapping("structuremappingSILC.csv", "sectoral",
                                   returnPathOnly = TRUE)
  dimMapping <- read.csv(dimMappingPath, sep = ";") %>%
    select(-"note", -"source", -matches("Name"))


  ## check arguments ====

  subtype <- match.arg(subtype, list.dirs(recursive = FALSE, full.names = FALSE))

  # replace empty arguments with default
  if (is.null(vars)) {
    vars <- defaultVars
  }
  if (is.null(dims)) {
    dims <- unique(dimMapping[["variableTarget"]])
  } else {
    dimMapping <- dimMapping %>%
      filter(.data[["variableTarget"]] %in% dims)
  }



  ## unpack ZIP ====

  mustUnzip <- setdiff(
    sub(".zip$", "", list.files(subtype, ".zip$")),
    list.dirs(subtype, recursive = FALSE, full.names = FALSE))
  for (i in seq_along(mustUnzip)) {
    unzip(zipfile = file.path(subtype, paste0(mustUnzip[i], ".zip")),
          exdir   = file.path(subtype, mustUnzip[i]))
  }


  ## derive helpers ====

  baseCols <- c("region", "period", "id")
  folders <- list.dirs(subtype, recursive = FALSE)
  varCodes <- as.character(do.call(c, lapply(vars, function(var) var[["code"]])))
  dimCodes <- unique(dimMapping[["variableSILC"]])
  codeRename <- c(
    do.call(c, lapply(names(vars), function(var) {
        codes <- vars[[var]][["code"]]
        setNames(rep(var, length(codes)), codes)
      })),
    unique(dimMapping[c("variableSILC", "variableTarget")]) %>%
      spread("variableSILC", "variableTarget") %>%
      as.list(),
    setNames("weight", weight)
  )



  # PROCESS DATA ---------------------------------------------------------------

  switch(subtype,
    publicMicrodata = {

      data <- do.call("mbind", lapply(folders, function(folder) {


        ## read data ====

        df <- do.call(full_join, c(lapply(c("D", "H"), function(fileFlag) {
          list.files(folder, paste0(tolower(fileFlag), "_EUSILC.csv"),
                     full.names = TRUE) %>%
            read.csv() %>%
            rename(period = paste0(fileFlag, "B010"),
                   region = paste0(fileFlag, "B020"),
                   id = paste0(fileFlag, "B030")) %>%
            select(any_of(c(baseCols, varCodes, dimCodes, weight)))
        }), list(by = baseCols)))



        ## remap dimensions ====

        df2 <- reduce(lapply(intersect(colnames(df), dimCodes), function(d) {

          # look-up table for remapping of this dimension
          lookUp <- dimMapping %>%
            filter(.data[["variableSILC"]] == d,
                   !is.na(.data[["valueTarget"]]),
                   .data[["valueTarget"]] != "")
          if (nrow(lookUp) == 0) {
            stop("There is no mapping of the dimension '", d, "' in ",
                 dimMappingPath)
          }

          # new name of dimension
          dimRename <- unique(lookUp[["variableTarget"]])
          if (length(dimRename) != 1) {
            stop("One unique value in the column 'variableTarget' for the ",
                 "dimension '", d, "' required in ", dimMappingPath)
          }

          # is this a discrete dimension?
          if (all(is.na(lookUp[c("quantileFrom", "quantileTo")]))) {
            discreteDim <- TRUE
          } else if (!any(is.na(lookUp[c("quantileFrom", "quantileTo")]))) {
            discreteDim <- FALSE
          } else {
            stop("Something is wrong in the mapping of '", d, "' in ",
                 dimMappingPath)
          }

          dfOut <- df %>%
            select(all_of(c(baseCols, d)))

          if (discreteDim) {

            ### discrete dim ####
            lookUp <- lookUp %>%
              select("valueSILC", "valueTarget", "weight")
            dfOut <- dfOut %>%
              select(all_of(c(baseCols, d))) %>%
              left_join(lookUp, by = setNames("valueSILC", d)) %>%
              select(-all_of(d))
          } else {

            ### binned continuous dim ####
            lookUp <- lookUp %>%
              select("quantileFrom", "quantileTo", "valueTarget", "weight")
            dfOut <- dfOut %>%
              mutate(`_relPos_` = (rank(.data[[d]]) - 1) / (length(.data[[d]]) - 1),
                     `_quantile_` = NA,
                     `_weight_` = 1)
            for (q in 1:nrow(lookUp)) {
              from <- lookUp[q, "quantileFrom"]
              to   <- lookUp[q, "quantileTo"]
              rowsInQuantile <- which(dfOut[["_relPos_"]] >= from &
                                        if (to == max(lookUp[["quantileTo"]])) {
                                          dfOut[["_relPos_"]] <= to
                                        } else {
                                          dfOut[["_relPos_"]] < to
                                        })
              dfOut[rowsInQuantile, "_quantile_"] <- lookUp[q, "valueTarget"]
              dfOut[rowsInQuantile, "_weight_"]   <- lookUp[q, "weight"]
            }
            dfOut <- dfOut %>%
              select(-all_of(c(d, "_relPos_")))
          }

          colnames(dfOut) <- c(baseCols, dimRename, paste("_weight_", d))
          return(dfOut)

        }), full_join, by = baseCols)
      }))








    })
}
