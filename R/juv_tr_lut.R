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

get_release_data(first_by=first_by,last_by=last_by) %>% 
  filter(!!!filter_conditions) %>%  
  releases_for_tr() %>%
  group_releases() %>% 
  calc_tagrates() 
}
