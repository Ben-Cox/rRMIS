#' Set the RMIS release file name
#' only necessary to use if RMIS changes this filename
#' @param filename 
#'
#' @return
#' @export
#'
set_release_filename <- function(filename){
  RMIS.globals$rel_file <- filename
}

