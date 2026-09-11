# Environment to store global variables
RMIS.globals <- new.env()
# Global variables for the pkg
# URL to public data repo
RMIS.globals$url <- "https://www.rmpc.org/pub/data/"

# The complete release file
RMIS.globals$rel_file <- "RL050_ALL_FULLSET.zip"
RMIS.globals$location_file <- "LC050_ALL_FULLSET.zip"

# Folder to store downloads in working directory, if none specified
RMIS.globals$root <- "RMIS"
# Default folder names to hold downloads. 
RMIS.globals$rel_dir <- file.path(RMIS.globals$root,"Releases")
RMIS.globals$rec_dir <- file.path(RMIS.globals$root,"Recoveries")
RMIS.globals$lut_dir <- file.path(RMIS.globals$root,"LUTs")

# names of lookup tables in pub/data
RMIS.globals$lut_filenames <- c(RMIS.globals$location_file, 
                    "run.zip",
                    "species.zip",
                    "study_type.zip",
                    "marks.zip",
                    "location_type.zip",
                    "gear.zip", 
                    "fishery.zip",
                    "period.zip",
                    "adclip_selective_fishery.csv"
                    )
