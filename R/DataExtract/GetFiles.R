GetFiles <- function(FolderPath = "data/raw") {
  
  FileList <- list.files(FolderPath,
                         pattern = "(?i)\\.xlsx$|\\.xls$|\\.csv$|\\.txt$",
                         full.names = TRUE,
                         recursive = FALSE)
  
  if (length(FileList) == 0) stop("No files found in: ", FolderPath)
  
  cat("=== GetFiles ===\n")
  cat("Found", length(FileList), "file(s) in:", FolderPath, "\n")
  cat("", paste(basename(FileList), collapse = "\n "), "\n\n")
  
  return(FileList)
}