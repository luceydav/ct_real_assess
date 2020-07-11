# Add time variables, drop remaining dupes, reset cases where Condo/Apt looks wrong
final_cleanup <- function(dt){
  
  # Add year and quarter variables
  dt[, `:=`(      
    qtr = zoo::as.yearqtr(date),
    year = year(date))]
  
  # Filter unique rows by data/address
  dt <- 
    unique(dt, by=c("address", "date", "sale_price", "town"))
  
  # Fix likely investment properties
  dt[(!town %in% c("Greenwich", "Darien", "New Canaan", "Westport") & 
        property_type == "Condo/Apartment" &
        sale_price > 1000000), 
     property_type := "Multi-Family"]
  
  # Return
  return(dt)
  
}
