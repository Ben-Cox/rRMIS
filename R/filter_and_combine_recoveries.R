#' Filter and combine recoveries
#'
#' @param first_by 
#' @param last_by 
#' @param rec_dir Directory where recovery data were downloaded. Defaults to RMIS.globals$rec_dir
#' @param lut_dir Directory where RMIS LUTs were downloaded. Defaults to RMIS.globals$lut_dir
#' @param ... filter conditions for RMIS fields in release data 
#' @importFrom rlang quos
#' @importFrom rlang !!!
#' @return
#' @export
#'
filter_and_combine_recoveries <- function(first_by, last_by, rec_dir=RMIS.globals$rec_dir, lut_dir=RMIS.globals$lut_dir, ...){
  
  if(!dir.exists(rec_dir) || length(list.files(rec_dir))==0){stop("No recovery data found")}
  
  # Create quosure for filter conditions passed in: need to document examples. 
  # Can pass in any filter conditions using RMIS field names.
  filter_conditions <- rlang::quos(...)

  files <- list.files(rec_dir, full.names=TRUE)

  Releases <- read_releases() %>% decode_release_data()
  
  species_lu <- read_csv(file.path(lut_dir,"species.zip"), 
                         col_types=cols(species=col_integer(), 
                                        species_name=col_character(),
                                        species_name=col_character())) %>% 
                select(species,species_name)
                                                                 
  # Set up clusters for parallel read/filter/combine
      cl <- makeCluster(detectCores()-1)
      
  # Register cluster      
      doParallel::registerDoParallel(cl=cl)  
  message("Reading recovery files, please be patient.")
  
# Read, lookup release info, filter in parallel
df <- foreach(i=seq_along(files), .combine=rbind, .inorder=FALSE, .packages=c("tidyverse")) %dopar% {
    read_csv(files[i], col_types=cols(.default="c", 
                                      recovery_date=col_date(format="%Y%m%d"),
                                      run_year=col_integer(),
                                      species=col_integer(),
                                      estimated_number=col_double())) %>%
    # Look up recovery species
    left_join(species_lu, by="species") %>%
    # Look up release info
    left_join(Releases, by=c("tag_code"="tag_code_or_release_id"), suffix=c("_recovery","")) %>%

    # Filter by conditions passed in as '...'
    filter(!!!filter_conditions,
           # Type 5 recoveries can lead to double counting, check RMIS manual
           sample_type!=5,
           # Only succesfully decoded recoveries
           tag_status==1,
           brood_year>=first_by,
           brood_year<=last_by,
           run_year>=first_by+2,
           run_year<=last_by+7)

  } # End parallel for loop

    # Stop the parallel cluster
        stopCluster(cl)
  
#  Return df      
  df

}

