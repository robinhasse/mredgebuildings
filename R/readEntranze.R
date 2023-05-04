#' @source https://www.entranze.eu/pub/pub-optimality
#' @importFrom openxlsx2
#' @importFrom tidyr fill
#' @importFrom dplyr bind_rows everything across
#' @export

readEntranze <- function() {

  files <- c(
    residential = "ENTRANZE_WP3-D3.2_Energy-cost_matrices_Def_RESIDENTIAL.xlsx",
    tertiary    = "ENTRANZE_WP3-D3.2_Energy-cost_matrices_Def_TERTIARY.xlsx"
  )

  data <- do.call("rbind", lapply(names(files), function(f) {

    file <- files[f]
    sheets <- read_sheet_names (file)
    dataSheets <- grep("^[A-Z]{2}_[A-Z][a-z]*$", sheets, value = TRUE)



    # read codes
    codes <- do.call("bind_rows", lapply(head(sheets, 3), function(sheet) {
      read_xlsx(file, sheet, colNames = FALSE, fillMergedCells = TRUE,
                rows = switch(sheet,
                              `1 INTRO&Legend`  = 9:21,
                              `2 EnvelopeCodes` = 6:39,
                              `3 PlantsCodes`   = 2:41),
                cols = switch(sheet,
                              `1 INTRO&Legend`  = 1:11,
                              `2 EnvelopeCodes` = 1:3,
                              `3 PlantsCodes`   = 1:3)) %>%
        mutate(across(everything(), as.character))
    }))
    codes <- as.data.frame(mapply(c, codes[, 1:3], codes[9:11])) %>%
      `colnames<-`(c("type", "code", "name")) %>%
      filter(!is.na(.data[["name"]]))

    # relevant columns
    cols <- switch(f,
                   residential = list(general    = 1:2,
                                      shell      = 9:12,
                                      technology = 14:22,
                                      cost1      = 24:41,
                                      cost2      = 42:59,
                                      energy1    = 60:66,
                                      energy2    = 67:74))

    # read data
    dataReg <- do.call("rbind", lapply(dataSheets, function(sheet) {
      sheetRaw <- read_xlsx(file, sheet, fillMergedCells = TRUE, colNames = FALSE,
                            startRow = 2)
      colsNamed <- lapply(cols, function(colIndex) colnames(sheetRaw)[colIndex])
      sheetRaw <- sheetRaw[, as.numeric(unlist(cols))]

      # region of this sheet
      region <- sheetRaw["2", "X"]

      # rows with data
      rows <- sheetRaw %>%
        select(period = "A") %>%
        mutate(row = rownames(sheetRaw)) %>%
        filter(.data[["period"]] != "Year") %>%
        mutate(chunk = c(0, cumsum(diff(as.numeric(.data$row)) - 1))) %>%
        group_by(across(all_of(c("period", "chunk")))) %>%
        summarise(from = min(as.numeric(.data[["row"]])),
                  to   = max(as.numeric(.data[["row"]])),
                  .groups = "drop") %>%
        group_by(.data[["period"]]) %>%
        arrange(.data[["chunk"]]) %>%
        mutate(chunk = c("lower", "upper"))
      rows <- lapply(split(rows, rows$period, drop = TRUE), function(x) {
        lapply(split(x, x[["chunk"]], drop = TRUE), function(y) {
          as.character(y$from:y$to)})
        })

      # reorganise data
      sheetOrg <- do.call("rbind", lapply(rows, function(r) {
        cbind(sheetRaw[r$lower, unlist(colsNamed[c("general", "shell", "technology", "cost1", "cost2")])],
              sheetRaw[r$upper, unlist(colsNamed[c("energy1", "energy2")])])
        # continue development here
      }))



    }))


  }))

}
