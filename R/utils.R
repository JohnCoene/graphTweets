# startUpMessage -------------------------------
.onAttach <- function(libname, pkgname = "fbAdsInsightsR") {
  packageStartupMessage("See ?getEdges for documentation")
}

# clean handles
cleanHandles <- function(handles) {
  # remove unwanted punctuation
  handles <- gsub(":", "",handles)
  handles <- gsub(",", "",handles)
  handles <- gsub(";", "",handles)
  handles <- gsub(">", "",handles)
  handles <- gsub("<", "",handles)
  handles <- gsub("\\...", "",handles)
  handles <- substring(handles, 2)
  
  return(handles)
}