#' Download dataframe of the RMIS release data
#'
#' @param first_by first brood year of releases 
#' @param last_by  last brood year of releases
#' @param dir directory where releases are held if `NULL` (the default) creates a Data folder in working directory.
#' @param lut_dir directory where releases are held
#' @param dl_now  Logical determines if release file should be downloaded now (if already present in `rel_dir`). Defaults to FALSE.
#' @return A dataframe of the RMIS release data with foreign keys decoded.
#' @export
#'
get_release_data <- function(first_by=NULL, last_by=NULL, rel_dir="RMIS",lut_dir="RMIS/LUTs", dl_now=FALSE){
  #if(is.null(dir))dir <- "RMIS"
  rel_file <- file.path(rel_dir,"RL041_ALL_FULLSET.zip")
 
  if((file.exists(rel_file) & dl_now==TRUE) | !file.exists(rel_file)) {
    download_releases(dir=rel_dir)
    download_luts(lut_dir=lut_dir)
    }
  
  read_releases(first_by=first_by, 
                last_by=last_by,
                dir=rel_dir) %>% 
    decode_release_data(lut_dir=lut_dir)
}
