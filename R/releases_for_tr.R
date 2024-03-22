#' prep release data for tag rate calculations
#'
#' @param release_data 
#'
#' @return Table with a row for each tag_code_or_release_id, with 4 new columns for CWT and clip status: AdClipped_CWT, AdClipped_NoCWT, Unclipped_CWT, Unclipped_NoCWT and a composite key of species,hatchery,run,stock,release location for grouping when doing tagrate calcs
#' @export
#'
releases_for_tr <- function(release_data){
  # function works as follows:
  # 1) strip tag nums/marks from raw release data i.e., columns cwt_1st... cwt_2nd etc....
  # 2) filter original data by mark (1st and 2nd) for Ad and unclipped
  # 3) rename count column with cwt and mark status
  # 4) bind rows with same cwt and mark status (i.e., same mark/cwt from 'cwt_1st' and 'cwt_2nd' columns)
  # 5) join all the bound tables together to the tag info in step 1 by tag code

  d <- release_data

 d %>% 
  mutate(LastRelDate=last_release_date) %>% 
  tidyr::unite(col=comp_key, 
               species,
               hatchery_location_code, 
               run, 
               stock_location_code, 
               release_location_code, 
               brood_year,remove=FALSE) %>% 
   
  # First, drop cwt_1st, 2nd etc. from raw release data (retains tag info but not marks/counts)
select(-c(cwt_1st_mark,
          cwt_1st_mark_count,
          cwt_2nd_mark,
          cwt_2nd_mark_count,
          non_cwt_1st_mark,
          non_cwt_1st_mark_count,
          non_cwt_2nd_mark,
          non_cwt_2nd_mark_count)) %>% 
  
#  Join tag info to AdCWT
  left_join(
    bind_rows(
      # Bind rows Ad-clippped with CWT
      d %>% select(tag_code_or_release_id, cwt_1st_mark, cwt_1st_mark_count) %>% 
            filter(cwt_1st_mark >= 5000) %>% 
            transmute(tag_code_or_release_id,
                      Ad_CWT=cwt_1st_mark_count),
      
      d %>% select(tag_code_or_release_id, cwt_2nd_mark, cwt_2nd_mark_count) %>% 
            filter(cwt_2nd_mark >= 5000) %>% 
            transmute(tag_code_or_release_id,
                      Ad_CWT=cwt_2nd_mark_count))%>% 
      group_by(tag_code_or_release_id) %>% 
      summarise_all(sum), 
    by="tag_code_or_release_id"
  ) %>% 
  
# Join to AdNoCWT
  left_join(
    bind_rows(
      # Bind rows Ad-clipped, no CWTs
      d %>% select(tag_code_or_release_id, non_cwt_1st_mark, non_cwt_1st_mark_count) %>% 
            filter(non_cwt_1st_mark >= 5000) %>% 
            transmute(tag_code_or_release_id,
                      Ad_NoCWT=non_cwt_1st_mark_count),
      
      d %>% select(tag_code_or_release_id, non_cwt_2nd_mark, non_cwt_2nd_mark_count) %>% 
            filter(non_cwt_2nd_mark >= 5000) %>% 
            transmute(tag_code_or_release_id,
                      Ad_NoCWT=non_cwt_2nd_mark_count)) %>% 
      group_by(tag_code_or_release_id) %>% 
      summarise_all(sum),
    by="tag_code_or_release_id"
  ) %>% 
  
# Join to NoClip CWT  
  left_join(
    # Bind unclipped with CWT
    bind_rows(d %>%
              select(tag_code_or_release_id, cwt_1st_mark, cwt_1st_mark_count) %>% 
              filter(cwt_1st_mark < 5000) %>% 
              transmute(tag_code_or_release_id,
                        Unclipped_CWT=cwt_1st_mark_count),
              d %>%
              select(tag_code_or_release_id, cwt_2nd_mark, cwt_2nd_mark_count) %>% 
              filter(cwt_2nd_mark < 5000) %>% 
              transmute(tag_code_or_release_id, 
                        Unclipped_CWT=cwt_2nd_mark_count))%>% 
      group_by(tag_code_or_release_id) %>% 
      summarise_all(sum),
    by="tag_code_or_release_id"
  ) %>% 
  
# Join to Unclipped No CWTs              
  left_join(
    # Bind unclipped with no CWTs
    bind_rows(d %>%
              select(tag_code_or_release_id, non_cwt_1st_mark, non_cwt_1st_mark_count) %>% 
              filter(non_cwt_1st_mark < 5000) %>% 
              transmute(tag_code_or_release_id,
                        Unclipped_NoCWT=non_cwt_1st_mark_count), 
              d %>% 
              select(tag_code_or_release_id, non_cwt_2nd_mark, non_cwt_2nd_mark_count) %>% 
              filter(non_cwt_2nd_mark < 5000) %>% 
              transmute(tag_code_or_release_id, 
                        Unclipped_NoCWT=non_cwt_2nd_mark_count))%>% 
      group_by(tag_code_or_release_id) %>% 
      summarise_all(sum),
    
    by="tag_code_or_release_id") %>% 
   mutate_at(vars(contains("CWT")), ~tidyr::replace_na(.x, 0)) %>% # replace NA counts in new columns with 0
   mutate(DIT=if_else(Ad_CWT==0,"DIT",NA_character_)) # tag codes or id's with no Ad CWT releases are considered DIT groups

}


