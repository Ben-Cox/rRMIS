#' Make a tag info lut
#'
#' @param first_by first brood year of releases
#' @param last_by  last brood year of releases
#' @param ...  other filter conditions passed in e.g., `species_name=="Chinook"`
#'
#' @return a table with release info for tag codes that meet brood year and other filter conditions
#' @export
#'
make_tag_info_lut <- function(first_by, last_by,...){
  filter_conditions <- rlang::quos(...)
  get_release_data(first_by=first_by,last_by=last_by) %>% 
    releases_for_tr() %>% 
    filter(record_code=="T",!!!filter_conditions) %>% 
    select(TagCode=tag_code_or_release_id, 
           brood_year,
           agency=release_agency, 
           hatchery, 
           run=run_name,
           stock, 
           release_loc=release_site,
          # rmis_region_name,
          # rmis_domain, 
           DIT)
}
