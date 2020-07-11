
# Function to extract date from various date chr fields with regex
extract_date_from_chr <- function(date_field){
  
  # Extract and convert varying date fields
  date_field <- trimws(date_field)
  date_field <- fifelse(
    str_detect(
      date_field, "\\d{4}"),
    lubridate::mdy(
      str_extract(date_field,
                  "^\\d{2}\\/\\d{2}\\/\\d{4}")),
    lubridate::mdy(
      str_extract(date_field, 
                  "^\\d{2}\\/\\d{2}\\/\\d{2}"))
  )
  return(date_field)
}