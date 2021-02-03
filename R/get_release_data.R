#' Download dataframe of the RMIS release data
#'
#' @param first_by first brood year of releases 
#' @param last_by  last brood year of releases
#' @param dir directory where releases are held if `NULL` (the default) creates a Data folder in working directory.
#'
#' @return A dataframe of the RMIS release data with foreign keys decoded.
#' @export
#'
get_release_data <- function(first_by=NULL, last_by=NULL, dir=NULL){
  download_releases(dir=dir)
  read_releases(first_by=first_by, 
                last_by=last_by,dir) %>% 
    decode_release_data()
}
  
