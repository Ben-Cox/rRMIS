#' Download RMIS LUTs
#' @description Downloads lookup tables from RMIS ftp server.
#' @param url url to RMIS ftp serer 
#' @param lut_dir Directory to save RMIS lookup tables. If `NULL` (the default) puts luts in a RMIS_LUTs folder in working directory.
#' @return `NULL`
#' @return Downloads lookup tables from RMIS into chosen dir
#' @export
download_luts <- function(lut_dir=RMIS.globals$lut_dir, lut_filenames=RMIS.globals$lut_filenames){

url <- get("url",RMIS.globals)

    if(!dir.exists(lut_dir)) {dir.create(lut_dir,recursive=TRUE) }
  
      lut_filenames <- RMIS.globals$lut_filenames

    ftp_paths <- file.path(url, lut_filenames)
    dest_paths <- file.path(lut_dir, lut_filenames)                

     
message("Downloading LUTs from RMIS to ~./",lut_dir,".")

for (i in 1:length(ftp_paths)){
    download.file(url=ftp_paths[i], destfile=dest_paths[i], quiet=TRUE)
} 

}


