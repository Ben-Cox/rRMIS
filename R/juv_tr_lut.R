#' Create a juvenile lookup table txt file
#'
#' @param first_by first brood year to pull
#' @param last_by last brood year to pull
#' @param ... filter conditions for decoded release data e.g., `species_name=="Chinook"`
#'
#' @return
#' @export
#'
juv_tr_lut <- function(first_by, last_by, ...){
  filter_conditions <- rlang::quos(...)
read_releases(first_by, last_by) %>% 
  decode_release_data() %>% 
  filter(!!!filter_conditions) %>%  
  group_releases() %>% 
  releases_for_tr() %>% 
  calc_tagrates() 
}
