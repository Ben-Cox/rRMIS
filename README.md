# rRMIS
A package to gather data from [RMIS](https://www.rmpc.org/). 
Downloads CWT release and recovery data from backup files on ftp server. 
Uses parallel processing to download and combine backup .zip files.

# Install 
```r
devtools::install_github("Ben-Cox/rRMIS")
```

# Examples
```r
library(rRMIS)
#' Download dataframe of the RMIS release data
#'
#' @param first_by first brood year of releases 
#' @param last_by  last brood year of releases
#' @param dir directory where releases are held if `NULL` (the default) creates a Data folder in working directory.
#' @param lut_dir directory where releases are held
#' @param dl_now  Logical determines if release file should be downloaded now (if already present in `rel_dir`). Defaults to FALSE.

# this function downloads data from the RL_ALL table in RMIS pub/data, saves the RL file to a Data folder in working directory by default.
d <- get_release_data(first_by=2016, last_by=2016)

d
```

