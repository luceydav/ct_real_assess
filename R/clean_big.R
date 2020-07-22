clean_big <- function(dt) {
  
  # Convert to dt
  dt <- setDT(dt)
  
  
  
  # Filter re_big for years without source data (ie: 2012-2017)
  # Add source identifier for socrata data
  #dt[, DateRecorded := extract_date_from_chr(DateRecorded)]
  dt[, date := as.Date(daterecorded)][, c("daterecorded", "id") := NULL][, source := 1]
  
  #Clean names, setkey and convert to be consistent with other data
  dt <- janitor::clean_names(dt)
  setnames(dt, 
           c("listyear", "serialnumber", "saleamount", "assessedvalue", "salesratio", "propertytype", "residentialtype", "nonusecode"),
           c("list_year", "serial_num", "sale_price", "assessed_value", "sales_ratio", "property_type", "residential_type", "non_use_code"))
  
  # Fix classifications
  dt[(property_type == "" & residential_type %in% c("Condo", "Apartments")), 
     property_type := "Condo/Apartment"]
  dt[(property_type == "" & residential_type == "Single Family"), 
     property_type := "Single Family"]
  dt[property_type %in% c("Condo", "Apartments"), 
     property_type := "Condo/Apartment"]
  dt[(property_type == "Residential" & residential_type == "Single Family"),
     property_type := "Single Family"]
  dt[(property_type %in% c("Residential", "") & 
        residential_type %in% c("Condo Family", "Four Family", "Multi Family", "Three Family", "Two Family")),
     property_type := "Multi-Family"]
  dt[(property_type == "" & residential_type == "Industrial"), 
     property_type := "Industrial"]
  dt[(property_type == "" & residential_type == "Public Utility"), 
     property_type := "Public Utility"]
  dt[(property_type == "" & residential_type == "Commercial"), 
     property_type := "Commercial"]
  dt[(property_type == "" & residential_type == "Vacant Land"), 
     property_type := "Vacant"]
  dt[property_type == "Vacant Land", 
     property_type := "Vacant"]
  
  # Fix missing sales_price when sales_ratio available, drop otherwise
  dt <- impute_sale_price(dt)
  
  # Drop NA sale_price and remaining duplicates if any
  dt <- dt[!is.na(sale_price)]
  dt <- unique(dt, by = c("date", "address", "sale_price"))
  
  # Return
  return(dt)
}
