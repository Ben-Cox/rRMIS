#' Make a list of files to download
#' @description Creates a list of files to download for desired years of CWT recovery data.
#' @param startYear First year of desired CWT recovery data
#' @param endYear  Last year of desired CWT recovery data
#' @param RMIS_url URL to RMIS ftp server
#' @param temp_dir File to hold downloaded csv's
#'
#' @return A list with download urls for each file and destination paths for each file
#' @export
make_file_list <- function(startYear, 
                              endYear,
                              RMIS_url="ftp://ftp.rmpc.org/pub/data/", 
                              dir=NULL) {
# # Error message
#   if(is.null(startYear) | is.null(endYear)) {
# 
#     return(message("ERROR: Please supply start and/or end year"))
#   }
  
  # load pkgs required for this function
    #using("RCurl", "tidyverse")
  
# Get filenames in the RMIS ftp directory - Takes a minute
ftp_names <- RCurl::getURL(RMIS_url, dirlistonly = TRUE)

# Make a vector of filenames, separating by line breaks ("\n")
  filenames <- strsplit(ftp_names, "\n") [[1]]

# Only want the csv files, use the regex below to grab only files beginning with RC041_ and ending with .csv
    # Changed to .zip to help with download speed. readr can read from local zips but not url .zips
  csvs <- filenames[grep("^RC041_.*\\.zip", filenames)]

  #  Only grab ODFW/WDFW files, replace line 22 with:
  #   csvs <-  filenames[grep("^RC041_ODFW.*\\.csv | ^RC041_WDFW.*\\.csv", filenames,fixed=F)]

# Extract the years for each recovery filename,convert to number. used to filter out years we don't need
  yrs <- substr(csvs, start=nchar(csvs)-8, nchar(csvs)-5) %>% as.integer

# Subset the csv filenames for only the years we want, drop the "\r" from filenames (for when we go to download) 
  files <- csvs[which(yrs >= startYear & yrs<=endYear)] %>% substr(1, nchar(.)-1)

# Return the list with a vector of file urls and the local path to save the downloaded files
  list(download_urls=paste0(RMIS_url, files), local_paths=paste0(dir, files))
}

