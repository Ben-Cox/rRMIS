#' Download RMIS LUTs
#' @description Downloads lookup tables from RMIS ftp server.
#' @param url url to RMIS ftp serer 
#' @param dir Directory to save RMIS lookup tables. If `NULL` (the default) puts luts in a RMIS_LUTs folder in working directory.
#' @return NULL
#' @return Downloads lookup tables from RMIS into chosen dir
#' @export
download_luts <- function(url="ftp://ftp.rmpc.org/pub/data/",dir=NULL){
  #require(parallel)
  #require(doParallel)
  #require(foreach)

  if(is.null(dir)) {dir <- "Data/RMIS_LUTs"}
    if(!dir.exists(dir)) {
      dir.create(dir,recursive=TRUE) }

  lut_dir <- dir
  
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
    dest_paths <- file.path(dir, lut_filenames)                
   
       cl <- parallel::makeCluster(parallel::detectCores())
      
       doParallel::registerDoParallel(cl=cl)
     

  foreach::foreach(i=seq_along(ftp_paths)) %dopar% {
    download.file(url=ftp_paths[i], destfile=dest_paths[i], quiet=TRUE)
    } %>% 
      invisible()
    stopCluster(cl)
} 

