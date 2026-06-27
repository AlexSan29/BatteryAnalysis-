SelectFiles <- function(PerCycleStats) {

  AvailableFiles <- unique(PerCycleStats$File)
  Choices        <- vapply(AvailableFiles, GetPlotTitle, character(1))

  Picked <- utils::select.list(
    choices  = Choices,
    multiple = TRUE,
    title    = "Select files to plot (none = all)"
  )

  if (length(Picked) == 0) {
    cat("No selection made — using all", length(AvailableFiles), "file(s)\n\n")
    return(PerCycleStats)
  }

  SelectedFiles <- AvailableFiles[Choices %in% Picked]

  cat("Selected", length(SelectedFiles), "of", length(AvailableFiles), "file(s):\n")
  cat("", paste(SelectedFiles, collapse = "\n "), "\n\n")

  PerCycleStats |> filter(File %in% SelectedFiles)
}