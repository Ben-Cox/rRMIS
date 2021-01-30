#' Filter and combine recoveries
#'
#' @param first_by 
#' @param last_by 
#' @param ... filter conditions for RMIS fields 
#'
#' @return
#' @export
#'
filter_and_combine_recoveries <- function(first_by, last_by, ...){
  # Create quosure for filter conditions passed in: need to document examples. 
  # Can pass in any filter conditions using RMIS field names.
  filter_conditions <- rlang::quos(...)

  files <- list.files("GatherData/temp_csvs", full.names=TRUE)

  Releases <- read_releases() %>% decode_release_data()
  
  species_lu <- read_csv("RMIS_LUTs/species.zip", 
                         col_types=cols(species=col_integer(), 
                                        species_name=col_character(),
                                        species_name=col_character())) %>% 
    select(species,species_name)
                                                                 
  # Set up clusters for parallel read/filter/combine
      cl <- makeCluster(detectCores())
      
  # Register cluster      
      registerDoParallel(cl=cl)  
      
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
    left_join(Releases, by=c("tag_code"="tag_code_or_release_id"), suffix=c("_recovery","_release")) %>%
    
    # Filter by conditions passed in as '...'
    filter(!!!filter_conditions, 
           # Type 5 recoveries can lead to double counting, check RMIS manual
           sample_type!=5, 
           # Only succesfully decoded recoveries
           tag_status==1,
           run_year>=first_by+2,
           run_year<=last_by+7)
  
  } # End parallel for loop

    # Stop the parallel cluster
        stopCluster(cl)
  
#  Return df      
  df
      
    # Write out the combined dataframe as a csv
    #write.csv(df, paste0("GatherData/", "recoveries.csv"),row.names=FALSE)
    
    # Delete the downloaded csvs in the temp directory
      #file.remove(file_list$local_paths) %>% invisible()
  
}

