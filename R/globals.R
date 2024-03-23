# Environment to store global variables
RMIS.globals <- new.env()
# Global variables for the pkg
# URL to public data repo
RMIS.globals$url <- "https://www.rmpc.org/pub/data/"

# The complete release file
RMIS.globals$rel_file <- "RL042_ALL_FULLSET.zip"

# Folder to store downloads in working directory, if none specified
RMIS.globals$root <- "RMIS"
# Default folder names to hold downloads. 
RMIS.globals$rel_dir <- file.path(RMIS.globals$root,"Releases")
RMIS.globals$rec_dir <- file.path(RMIS.globals$root,"Recoveries")
RMIS.globals$lut_dir <- file.path(RMIS.globals$root,"LUTs")
