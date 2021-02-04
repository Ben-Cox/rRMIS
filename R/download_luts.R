#' Download RMIS LUTs
#' @description Downloads lookup tables from RMIS ftp server.
#' @param url url to RMIS ftp serer 
#' @param lut_dir Directory to save RMIS lookup tables. If `NULL` (the default) puts luts in a RMIS_LUTs folder in working directory.
#' @return `NULL`
#' @return Downloads lookup tables from RMIS into chosen dir
#' @export
download_luts <- function(lut_dir="RMIS/LUTs"){
  #require(parallel)
  #require(doParallel)
  #require(foreach)
url <- get("url",RMIS.globals)
 # if(is.null(dir)) {dir <- "RMIS/LUTs"}
    if(!dir.exists(lut_dir)) {dir.create(lut_dir,recursive=TRUE) }

  #lut_dir <- dir
  
  lut_filenames <- c("LC041_ALL_FULLSET.zip",
                    "run.zip",
                    "species.zip",
                    "study_type.zip",
                    "marks.zip",
                    "location_type.zip",
                    "gear.zip", 
                    "fishery.zip",
                    "period.zip",
                    "adclip_selective_fishery.csv"
                    )

    ftp_paths <- file.path(url, lut_filenames)
    dest_paths <- file.path(lut_dir, lut_filenames)                
   
       cl <- parallel::makeCluster(parallel::detectCores())
      
       doParallel::registerDoParallel(cl=cl)
     
message("Downloading LUTs from RMIS to ~./",lut_dir,".")

  foreach::foreach(i=seq_along(ftp_paths)) %dopar% {
    download.file(url=ftp_paths[i], destfile=dest_paths[i], quiet=TRUE)
    } %>% 
      invisible()
    stopCluster(cl)
} 


