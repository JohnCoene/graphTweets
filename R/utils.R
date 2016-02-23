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