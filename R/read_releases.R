#' Read release data
#'
#' @param first_by first brood year of data
#' @param last_by  last brood year of data
#' @param dir Directory where release data were downloaded
#' @import readr
#' @return A data frame of the raw RMIS release data, filtered for brood years between first_by and last_by (inclusive)
#' @export
read_releases <- function(first_by=NULL, last_by=NULL, dir=NULL){
  require(readr)
  if(is.null(dir)){dir <- "RMIS"}
  file <- file.path(dir, "RL041_ALL_FULLSET.zip")
  if(!file.exists(file)){download_releases(dir=dir)}
  
 if (!is.null(first_by) & !is.null(last_by)) {  
  read_csv(file, 
           col_types=cols(.default="c",
                          species="i",
                          run="i",
                          avg_weight="d",
                          cwt_1st_mark_count="d",
                          cwt_2nd_mark_count="d",
                          non_cwt_1st_mark_count="d",
                          non_cwt_2nd_mark_count="d",
                          brood_year="i",
                          tag_loss_rate="d",
                          avg_length="d",
                          tag_loss_sample_size="i",
                          tag_loss_days="d"), 
           progress=FALSE)%>% 
    # Filter for Chinook (1) and Coho (2)
    filter( brood_year >= first_by, 
            brood_year <= last_by)
 } else {
   read_csv(file, 
           col_types=cols(.default="c",
                          species="i",
                          run="i",
                          avg_weight="d",
                          cwt_1st_mark_count="d",
                          cwt_2nd_mark_count="d",
                          non_cwt_1st_mark_count="d",
                          non_cwt_2nd_mark_count="d",
                          brood_year="i",
                          tag_loss_rate="d",
                          avg_length="d",
                          tag_loss_sample_size="i",
                          tag_loss_days="d"), 
           progress=FALSE)
  
 }
}  


