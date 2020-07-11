# Add reval year
add_reval <- function(dt) {
  
  path <- "/Users/davidlucey/Desktop/David/Projects/ct_real_estate/data/"
  
  # Reval years
  reval <-
    fread(paste0(path, "Revaluation_Years_by_Town.csv"))[, 2:3]
  reval <- janitor::clean_names(reval)
  
  # Join with reval years
  dt <-
    reval[dt[,
             .(
               town,
               address,
               assessed_value,
               sale_price,
               sales_ratio,
               non_use_code,
               property_type,
               date,
               list_year,
               qtr,
               year,
               source
             )],
          on = "town"]
  
  # Calculate reval year based on every 5 years
  dt[, reval_yr := reshape_reval(next_reval_year, year)]
  
  # Return
  return(dt)
}