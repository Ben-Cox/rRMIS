#' Download release data from RMIS
#'
#' @param url the url to the full CWT release data set on the RMIS ftp server
#' @param dir Directory to save downloaded file. If `NULL` (the default), saves in Data in working directory.
#'
#' @return `NULL` 
#' @return Saves file locally.
#'
#' @export
#'
#' @examples download_releases()
download_releases <- function(file="RL041_ALL_FULLSET.zip", dir="RMIS"){
  #if(is.null(dir)) dir <- "RMIS"
  url <- file.path(get("url",env=RMIS.globals), get("rel_file",env=RMIS.globals))
  #dir <- file.path(url, file)
  if(!dir.exists(dir)) dir.create(dir,recursive=TRUE)
  message("Downloading Release data from RMIS to ~./", dir,".")
  download.file(url=url, 
                destfile=file.path(dir,get("rel_file",env=RMIS.globals)), 
                quiet=TRUE)
}

RMIS.globals <- new.env()
RMIS.globals$url <- "https://www.rmpc.org/pub/data/"
RMIS.globals$rel_file <- "RL042_ALL_FULLSET.zip"

