ExtractEISData <- function(FileList) {

  EISFiles <- FileList[grepl("EIS", basename(FileList), ignore.case = TRUE)]

  if (length(EISFiles) == 0) stop("No EIS files found in FileList")

  cat("Loading", length(EISFiles), "file(s) from data/raw...\n")

  AllDataList <- map(seq_along(EISFiles), function(i) {

    Fname <- basename(EISFiles[i])
    Fpath <- EISFiles[i]

    Meta <- str_match(Fname, "^(.*?)_EIS_(\\d+)_S(\\d+)_(TOC|BOC)")
    Material    <- Meta[, 2]
    CellNum     <- Meta[, 3]
    Stage       <- as.integer(Meta[, 4])
    ChargeState <- Meta[, 5]

    if (is.na(Material) || is.na(CellNum) || is.na(Stage) || is.na(ChargeState)) {
      stop(sprintf("Filename '%s' does not match expected EIS naming pattern.", Fname))
    }

    Raw <- read_tsv(Fpath, skip = 1, show_col_types = FALSE)

    cat(sprintf("  [%d/%d] %s — %d rows\n", i, length(EISFiles), Fname, nrow(Raw)))

    Raw |>
      select(
        ReZ    = `Re(Z)/Ohm`,
        NegImZ = `-Im(Z)/Ohm`
      ) |>
      mutate(
        File        = tools::file_path_sans_ext(Fname),
        Stage       = Stage,
        ChargeState = ChargeState,
        CellNum     = CellNum,
        Material    = Material
      )
  })

  AllData <- bind_rows(AllDataList)

  cat(sprintf("Done — %d total rows\n\n", nrow(AllData)))
  AllData
}