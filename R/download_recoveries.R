#' Download CWT recoveries
#'
#' @param start_yr First year of recoveries 
#' @param end_yr  Last year of recoveries
#' @param dir Directory to save downloaded files, if `NULL` creates Data/Recoveries/temp in working directory.
#' @param by_brood  `TRUE` (default) if start and end years are in terms of brood years
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom foreach foreach %dopar%
#' @return `NULL`
#' @return Downloads .zip files of recovery data to local dir
#' @export
#'
download_recoveries <- function(start_yr, end_yr, by_brood=TRUE, dir=RMIS.globals$rec_dir) {

  #if(is.null(dir)) dir <- "RMIS/Recoveries"
  if(!dir.exists(dir)){dir.create(dir, recursive=TRUE)}

  if(by_brood){
    
  startYear <- start_yr + 2
  endYear <- end_yr + 7
  
  }else{
    startYear <- start_yr
    endYear <- end_yr
  }
  
  curYear <- lubridate::year(Sys.Date())
  
  if (startYear > (curYear-1) || (endYear > (curYear-1))){
    warning(paste0("Chosen brood year(s) may not be fully reported to RMIS"))
    }
  
  # BUILD TEST FOR COMPLETED BROODs- maybe year(sys.Date()-1)?

   # Create list with the file  paths
      file_list <- make_file_list(startYear, endYear, dir=dir)
      n_files <- length(file_list$files)
    
    # Set up clusters for parallel downloads
      cl <- parallel::makeCluster(parallel::detectCores())

      doParallel::registerDoParallel(cl)
      
      message(paste0("There are ", n_files," files to download."))

  if(n_files>20){message("This could take a second.")}     
      
  # Do download, read and combine the csvs with parallel for loop
foreach::foreach(i=seq_along(file_list$files), .inorder=FALSE) %dopar% {
      # Download the files from download urls, save them to the local paths
       download.file(url=file_list$download_urls[i], destfile=file_list$files[i], quiet=TRUE,mode = "wb")
       
    }
    
parallel::stopCluster(cl)

}

