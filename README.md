# rRMIS
A package to gather data from [RMIS](https://www.rmpc.org/). 
Downloads CWT release and recovery data from backup files on ftp server. 
Uses parallel processing to download and combine backup .zip files.
This package is a workaround for lack of an API to database server backend.

# Install 
```r
devtools::install_github("Ben-Cox/rRMIS")
```

# Examples
```r
library(rRMIS)

d <- get_release_data(first_by=2016, last_by=2016)

d
```

