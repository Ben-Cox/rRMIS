#' Lookup foreign keys in RMIS release data
#'
#' @param RMIS_releases read in from read_releases()
#' @param lut_dir Directory with RMIS lookup tables defaults to `RMIS/LUTs` in working directory.
#' @return Dataframe of RMIS release data with decoded field names.
#' 
#'
decode_release_data <- function(RMIS_releases, lut_dir=RMIS.globals$lut_dir){
 
  if(!dir.exists(lut_dir) | length(list.files(lut_dir))==0){
    download_luts(lut_dir=lut_dir)
    }
  
 # Read lookup tables
  RMIS_locations <- read_csv(file.path(lut_dir,"LC042_ALL_FULLSET.zip"), col_types=cols(.default="c"),progress=FALSE)
  RMIS_runs <- read_csv(file.path(lut_dir,"run.zip"), col_types=cols(.default="c",run="i"),progress=FALSE)
  RMIS_species <- read_csv(file.path(lut_dir,"species.zip"), col_types=cols(.default="c",species="i"),progress=FALSE)
  RMIS_studytype <- read_csv(file.path(lut_dir,"study_type.zip"), col_types=cols(.default="c"),progress=FALSE)
  
  missing_release_locs <- which(is.na(RMIS_releases$release_location_code) | 
                                  RMIS_releases$release_location_code=="" | 
                                  grepl("TEMP",RMIS_releases$release_location_code))

  missing_domains <- RMIS_releases %>% 
    dplyr::filter(grepl("TEMP", .$release_location_code) | is.na(release_location_code) | release_location_code == "") %>% 
  left_join(filter(RMIS_locations, location_type == 3), by=c("hatchery_location_code"="location_code")) %>% 
  select(tag_code_or_release_id, psc_region) %>% 
  left_join(RMIS_domain, by=c("psc_region"="rmis_region")) %>% 
    select(rmis_domain)

CR_rel_data <- 

  # Filter the releases for brood years, grab all years for both release summary and tag recoveries  
  RMIS_releases %>% 
  # Then join to the location table (type 4 = release locations) to lookup the release site name and psc region
    left_join(
    
    #Select just the location code and name from the locations table to join to the releases
      select(filter(RMIS_locations,location_type==4), location_code, release_site=name, psc_region),
  
    #Join the release data to the filtered location table (line above) by release location code and RelCode (named in line above: "RelCode=location_code")
      by=c("release_location_code"="location_code")) %>% 
  
    # Join the psc region looked up above to the rmis_domain table to lookup the RMIS domain,
      left_join(RMIS_domain, by=c("psc_region"="rmis_region")) %>%
  
    # Join to the location table again, this time lookup hatchery name
      left_join(
      select(filter(RMIS_locations, location_type==3), location_code, hatchery=name),
      by=c("hatchery_location_code"="location_code")) %>%
    
    # Join to location table one last time to lookup stock name
      left_join(
      select(filter(RMIS_locations, location_type==5), location_code, stock=name),
      by=c("stock_location_code"="location_code")) %>%   
  
    left_join(select(RMIS_species, species, species_name), by=c("species")) %>% 
    left_join(RMIS_runs, by="run") %>% 
    left_join(RMIS_studytype, by="study_type") %>% 
  
    mutate(release_year=substr(last_release_date, 1, 4)) #%>%
  
    # Finally, select only the columns needed.
    #select(record_code,tag_code_or_release_id,reporting_agency, species=species_name, run=run_name, tag_code_or_release_id, Hatchery, Stock, RelSite,brood_year,first_release_date, last_release_date,release_year,rmis_domain,
    #       cwt_1st_mark, cwt_1st_mark_count,non_cwt_1st_mark,non_cwt_1st_mark_count,cwt_2nd_mark,
    #       cwt_2nd_mark_count,non_cwt_2nd_mark,non_cwt_2nd_mark_count,release_location_code,study_type=study_type_name,avg_weight,rel_id,comp_key)

# Lookup the rmis domain for the missing domains, replace in CR_rel_data
CR_rel_data[which(is.na(CR_rel_data$rmis_domain)), "rmis_domain"] <- 
  
  CR_rel_data[which(is.na(CR_rel_data$rmis_domain)),] %>% 
  left_join(filter(RMIS_locations,location_type==3), by=c("hatchery_location_code"="location_code")) %>%
  left_join(RMIS_domain, by=c("psc_region.y"="rmis_region")) %>% 
  select(rmis_domain.y)

# Check for agencies with releases having undefined domains
agencies_no_domain <- CR_rel_data %>% 
  filter(is.na(rmis_domain)) %>% 
  select(release_agency) %>% 
  distinct()

# # Print warning for missing rmis_domains. Col R are usually reported, have seen WDFW coastal data with no domain
# if(nrow(agencies_no_domain)>0){
#   warning(paste(nrow(agencies_no_domain),"agencies have undefined release domains."), 
#           paste(c(" Agencies include:", pull(agencies_no_domain, release_agency)), collapse=" "))
# }

# Change release date to date format
 CR_rel_data %>% 
   
   #tibble(last_release_date=c("20200507","202004","2020",NA)) %>% 
   mutate(date_len=stringr::str_length(last_release_date)) %>% 
   mutate(last_release_date=case_when(date_len==4 ~ paste0(last_release_date,"0101"), # only year reported to RMIS YYYY
                                      date_len==6 ~ paste0(last_release_date,"14"),   # only month reported to RMIS YYYYmm
                                      date_len==8 ~ last_release_date)) %>%   # full date reported YYYYmmdd
   mutate(last_release_date=lubridate::ymd(last_release_date))

 }

