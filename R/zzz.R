#' zzz
#'
#' @param libname 
#' @param pkgname 
#'
#' @return
#'
.onLoad <- function(libname=find.package("rRMIS"), pkgname="rRMIS"){
  download_releases()
  download_luts()
  message("Downloading release data and LUTs from RMIS")
}

