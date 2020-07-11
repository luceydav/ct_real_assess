# Get cleaned data from Socrata for years available
load_socrata <- function() {
  
  data <- RSocrata::read.socrata("https://data.ct.gov/resource/5mzw-sjtu.csv",
                                 app_token = keyring::key_get("SOCRATA_TOKEN"),
                                 email=keyring::key_get("SOCRATA_EMAIL"),
                                 password=keyring::key_get("SOCRATA_PASSWORD"))
  data <- clean_big(data)
  
  return(data)
} 