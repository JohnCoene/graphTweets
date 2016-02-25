# startUpMessage -------------------------------
.onAttach <- function(libname, pkgname = "fbAdsInsightsR") {
  packageStartupMessage("See ?getEdges and ?getNodes for examples")
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
  
  # remove @@
  if(length(grep("@", handles))){
    handles <- substring(handles, 2)
  }
  
  return(handles)
}