#' Make a list of files to download
#' @description Creates a list of files to download for desired years of CWT recovery data.
#' @param startYear First year of desired CWT recovery data
#' @param endYear  Last year of desired CWT recovery data
#' @param RMIS_url URL to RMIS ftp server
#' @param temp_dir File to hold downloaded csv's
#' @importFrom  purrr map_lgl 
#' @importFrom XML htmlParse  xpathSApply
#' @importFrom stringr str_detect str_extract
#' @return A list with download urls for each file and destination paths for each file
#'
make_file_list <- function(startYear, 
                              endYear,
                              dir=NULL) {

# Read the html of the public data url www.rmpc.org/pub/data
doc <- XML::htmlParse(readLines(RMIS.globals$url), asText=TRUE)

# Find links in the lines
links <-XML::xpathSApply(doc, "//a/@href", simplify = T, unname)

# Keep links to files starting with RC0 and ending with .csv
csvs <- links[purrr::map_lgl(links, ~stringr::str_detect(.x,"^RC.*\\.csv$"))]

# Extract 4-digit years for each recovery filename used to filter
  yrs <- as.numeric(stringr::str_extract(csvs,"(?<=_)\\d{4}(?=\\.)"))
  
# Subset the csv filenames for only the years we want, drop the "\r" from filenames (for when we go to download) 
  files_to_download <- csvs[which(yrs >= startYear & yrs<=endYear)] #%>% substr(1, nchar(.)-1)

# Return the list with a vector of file urls and the local path to save the downloaded files
  list(download_urls=paste0(RMIS.globals$url, files_to_download), files=file.path(dir,files_to_download))
}

