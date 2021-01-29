#' Download release data from RMIS
#'
#' @param url the url to the full CWT release data set on the RMIS ftp server
#' @param dir Directory to save downloaded file. If `NULL` (the default), saves to working directory.
#'
#' @return `NULL` 
#' @return Saves file locally.
#'
#' @export
#'
#' @examples download_releases()
download_releases <- function(url="ftp://ftp.rmpc.org/pub/data/RL041_ALL_FULLSET.zip", dir=NULL){
  if(is.null(dir)) dir <- getwd()
  download.file(url=url, 
                destfile=file.path(dir,"RL041_ALL_FULLSET.zip"), 
                quiet=TRUE)
}

