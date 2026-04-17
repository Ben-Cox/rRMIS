#' Set the RMIS location file name
#' only necessary to use if RMIS changes this filename
#' @param filename 
#'
#' @return
#' @export
#'
set_location_filename <- function(filename){
  RMIS.globals$location_file <- filename
}

