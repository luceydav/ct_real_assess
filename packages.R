
# Load packages
packages <- 
  c("data.table",
    "arules",
    "stringr",
    "readr",
    "readxl",
    "Hmisc",
    "drake",
    "RSocrata",
    "janitor",
    "lubridate",
    "zoo",
    "keyring",
    "dplyr", 
    "rsconnect")

if (length(setdiff(packages,rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

invisible(lapply(packages, library, character.only = TRUE))