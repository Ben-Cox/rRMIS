#' Lookup recovery location/fishery info
#'
#' @param recoveries A dataframe of recovery data from RMIS
#'
#' @return A dataframe with foreign keys decoded from LUTs
#' @export
#'
tidy_recoveries <- function(recoveries,lut_dir="RMIS/LUTs") {
  #using("readxl") 
  
  RMIS_recoveries <- recoveries %>%  mutate(temp_prefix = NA)#overies 

  gear_lu <- read_csv(file.path(lut_dir,"gear.zip"), col_types = cols(fishery = "d"))
  
  fishery_lu2 <- read_csv(file.path(lut_dir,"fishery.zip"), col_types = cols(fishery = "d"))
  
  RMIS_locations <- read_csv(file.path(lut_dir,"LC042_ALL_FULLSET.zip"), col_types = cols(.default = "c"), progress=FALSE)

  # Get unique recovery location/fishery rows from recoveries, used to create a mgt fishery lut for the recoveries
    unique_rec_locs <- RMIS_recoveries %>% select(recovery_location_code,fishery) %>% distinct() %>% mutate(temp_prefix=NA)

  # Matches the fishery prefix to corresponding row(s) in the fishery_lu
    mgt_fishery_lu <-
  
    sapply(1:nrow(unique_rec_locs), function(x)
  which(startsWith(unique_rec_locs$recovery_location_code[x], prefix = fishery_lu$location_code_prefix) &
          unique_rec_locs$fishery[x] >= fishery_lu$starting_psc_fishery & unique_rec_locs$fishery[x] <= fishery_lu$ending_psc_fishery)
      )

  no_codes <-
    sapply(1:length(mgt_fishery_lu), function(x)
      length(mgt_fishery_lu[[x]])) == 0
  
  
  unique_rec_locs$temp_prefix[no_codes] <- "X"
  
  mgt_fishery_lu[which(unique_rec_locs$temp_prefix == "X")] <-
    sapply(which(unique_rec_locs$temp_prefix == "X"), function(x)
      which(
        startsWith(unique_rec_locs$temp_prefix[x], prefix = fishery_lu$location_code_prefix) &
          unique_rec_locs$fishery[x] >= fishery_lu$starting_psc_fishery &
          unique_rec_locs$fishery[x] <= fishery_lu$ending_psc_fishery
      ))
  
  no_codes <-
    sapply(1:length(mgt_fishery_lu), function(x)
      length(mgt_fishery_lu[[x]])) == 0
  
  mgt_fishery_lu[no_codes] <- NA
  
  # Matches with >1 corresponding fishery codes
  to_fix <-sapply(1:length(mgt_fishery_lu), function(x) length(mgt_fishery_lu[[x]])) > 1
  
  if(any(to_fix)){
  # Replace with the longest match
    for (i in 1:length(mgt_fishery_lu[to_fix])) {
      mgt_fishery_lu[to_fix][[i]] <- mgt_fishery_lu[to_fix][[i]][which.max(nchar(fishery_lu$location_code_prefix[mgt_fishery_lu[to_fix][[i]]]))]
    }
  }
  
  mgt_fishery_lu <- unlist(mgt_fishery_lu)
  
  rec_fishery_lu <- bind_cols(unique_rec_locs, mgt_fishery=fishery_lu$mgt_fishery_name[mgt_fishery_lu]) %>% 
                    select(- temp_prefix)

  RMIS_recoveries %>% 
    left_join(rec_fishery_lu, by=c("recovery_location_code","fishery")) %>% 
    #select(tag_code,recovery_date,mgt_fishery) %>% 
    left_join(select(filter(RMIS_locations,location_type==1), location_code, recovery_location=name, description, psc_region), by=c("recovery_location_code"="location_code")) %>% 
    left_join(fishery_lu2 %>% mutate(fishery=as.character(fishery)),by="fishery") %>% 
    left_join(gear_lu %>% mutate(fishery=as.character(fishery)), by = c("reporting_agency_recovery"="reporting_agency", "fishery", "gear")) %>% 
    mutate(age=run_year - brood_year)
      
}

