
#' Get recovery data
#'
#' @param first_by first brood year to retain in filter
#' @param last_by last brood year to retain in filter
#' @param ...  additional filter conditions passed in e.g., species_name=="Chinook"
#' @param rec_dir directory where recovery files are saved. Default = `RMIS/Recoveries`
#' @param lut_dir directory where lookup tables are saved. Default= `RMIS/LUTs` 
#' @return
#' @export
#'
#' @examples get_recoveries(first_by=1999,last_by=1999,species_name=="Chinook,run_name=="Spring")
get_recoveries <- function(first_by, last_by, ..., rec_dir="RMIS/Recoveries",lut_dir="RMIS/LUTs"){
  recs <- filter_and_combine_recoveries(first_by=first_by, last_by=last_by, ..., rec_dir, lut_dir)
    recs %>% tidy_recoveries(lut_dir=lut_dir)
}

