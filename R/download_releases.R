#' Download release data from RMIS
#'
#' @param url the url to the full CWT release data set on the RMIS ftp server
#' @param dir Directory to save downloaded file.  Defaults to `RMIS/Releases` in working directory.
#'
#' @return `NULL` 
#' @return Saves file locally.
#'
#' @export
#'
#' @examples download_releases()
download_releases <- function(url=RMIS.globals$url, dir=RMIS.globals$rel_dir){
  
  url <- file.path(url, rel_file)

  if(!dir.exists(dir)) dir.create(dir,recursive=TRUE)
  message("Downloading Release data from RMIS to ~./", dir,".")
  download.file(url=url, 
                destfile=file.path(dir,RMIS.globals$rel_file), 
                quiet=TRUE)
}
