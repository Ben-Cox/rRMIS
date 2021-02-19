
#' Calculate tagrates from reshaped release data
#'
#' @param release_data 
#'
#' @return
#' 
#'
calc_tagrates <- function(release_data){
  if(!any(colnames(release_data)=="rel_id")){stop("Release ids have not been assigned. Pipe releases through group_releases() first.")}
  
  release_data %>% group_by(comp_key,rel_id) %>% 
  summarise(Ad_CWT=sum(Ad_CWT,na.rm=TRUE),Ad_NoCWT=sum(Ad_NoCWT,na.rm=TRUE),Unclipped_CWT=sum(Unclipped_CWT,na.rm=TRUE),Unclipped_NoCWT=sum(Unclipped_NoCWT,na.rm=TRUE)) %>% 
  left_join(select(release_data,tag_code_or_release_id,comp_key,rel_id,DIT),by=c("comp_key"="comp_key","rel_id"="rel_id")) %>% 
  mutate(NonMS=Ad_CWT/(Ad_CWT+Ad_NoCWT+Unclipped_CWT+Unclipped_NoCWT), MS=Ad_CWT/(Ad_CWT+Ad_NoCWT)) %>% 
  ungroup %>% 
  left_join(select(release_data, tag_code_or_release_id, record_code),by=c("tag_code_or_release_id"="tag_code_or_release_id")) %>%
  filter(record_code=="T" & is.na(DIT)) %>% 
  select(TagCode=tag_code_or_release_id, MS, NonMS) %>% 
  tidyr::pivot_longer(cols=c(MS,NonMS),names_to="TagRateType",values_to="TR") %>% 
    arrange(TagCode)
  
}

