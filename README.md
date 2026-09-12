# rRMIS
A package to gather data from [RMIS](https://www.rmpc.org/). 
Downloads CWT release and recovery data from backup files on ftp server. 
Uses parallel processing to download and combine backup .zip files. 
Was developed as a workaround to access RMIS data before the API:
Data can now be accessed via the API with the [rmisr](https://github.com/MattCallahan-NOAA/rmisr) package.

May still be useful if there are fields in the core tables you want that the API doesn't provide.

# Install 
```r
devtools::install_github("Ben-Cox/rRMIS")
```

# Examples
```r
library(rRMIS)

When RMIS data vesions or the pub/data url changes, download functions will break. 
User can update the global values for the current session with:

#' \dontrun{
#' set_url("the_new_url.com")
#' set_release_filename("RL0XX_ALL_FULLSET.zip")
#' set_location_filename("LC0XX_ALL_FULLSET.zip")
#'}

# this function downloads release data from RMIS pub/data for a specified range of brood years.
# saves the file to a Data folder in working directory by default.
# downloads LUTs from RMIS pub/data
# does lookups for foreign keys,including location codes, species, and run. converts first and last dates to date format.

d <- get_release_data(first_by=2016, last_by=2016)

d
```

