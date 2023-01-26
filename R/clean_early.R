
# Customized to clean early years
clean_early <- function(dt){
  
  # Path to local
  path <- "/Users/davidlucey/Documents/Projects/ct_real_estate/data/"
  
  # First move date back to chr then convert blank to NA
  dt[, date_recorded := as.character(date_recorded)]
  dt[dt == "" | dt == " " | dt == "  "] <- NA
  
  # Trim white space for all variables in case data input was messy                 
  cols <- names(dt)
  dt <- dt[, (cols) := lapply(.SD, trimws), .SDcols = cols]
  
  cols <-
    c("total_assessment", "sale_amount")
  dt[, (cols) := lapply(.SD, readr::parse_number), .SDcols=cols]
  
  # Create multi-family column to denote if Single Family, Multiple or Unknown
  # This was made necessary for years where separate columns for number of families were disclosed
  dt[, residential_type := 
       create_residential_type(
         one_family,
         two_family,
         three_family)]
  
  # Create alternate PropertyType1 column to gather info from separate column types where available
  # uses create_property_type defined above
  dt[, 
     property_type_1 :=
       create_property_type(
         residential,
         residential_other,
         apartment,
         commercial,
         industrial,
         vacant_land,
         utility
       )]
  
  # Manual Allocations using all data
  dt[((property_type == "R" |
         property_type_1 == "Residential") &
        condo == "X" & 
        residential_type == "Single Family"),
     new_class := "Condo/Apartment"]
  dt[((property_type == "R" |
         property_type_1 == "Residential") &
        residential_type %in% c("Three Family", "Two Family")),
     new_class := "Multi-Family"]
  dt[((property_type == "R" | 
         property_type_1 == "Residential") &
        is.na(condo) & 
        residential_type == "Single Family"),
     new_class := "Single Family"]
  dt[property_type_1 == "Commercial" & 
       property_type != "R" &
       !is.na(new_class),
     new_class := "Commercial"]
  dt[property_type_1 == "Industrial" & 
       property_type != "R" &
       is.na(new_class),
     new_class := "Industrial"]
  dt[property_type_1 == "Vacant" | 
       property_type == "V" &
       is.na(new_class),
     new_class := "Vacant"]
  dt[property_type == "A" & 
       residential_type != "Single Family" &
       is.na(new_class),
     new_class := "Multi-Family"]
  dt[property_type == "A" & 
       residential_type == "Single Family" &
       is.na(new_class), 
     new_class := "Condo/Apartment"]
  dt[property_type == "C/I/U" & 
       property_type_1 == "Utility" &
       is.na(new_class),
     new_class := "Utility"]
  dt[is.na(new_class),
     new_class := "Unclear"]
  
  # Data in early years has city codes, but not names so need to extract names and add
  town_table <- # Extract town codes and clean
    as.data.table(as.matrix(
      Hmisc::mdb.get(paste0(path, "99_sales.mdb"), 
                     tables = "TownTable")))
  
  town_table <- # Trim white space in towns table
    as.data.table(sapply(town_table, trimws))
  town_table <- 
    clean_names(town_table)
  
  # Join to add Town names on town codes
  dt <- town_table[dt, on = "town_code"]
  
  # Fix names
  setnames(
    dt,
    c("town_name", "total_assessment", "property_location"),
    c("Town", "AssessedValue", "Address")
  )
  
  # Clean names
  dt <- janitor::clean_names(dt)
  
  # Choose vars to keep
  keeps <-
    c(
      "address",
      "non_use_code",
      "town",
      "date_recorded",
      "list_year",
      "new_class",
      "assessed_value",
      "sales_ratio",
      "sale_amount",
      "serial_number"
    )
  dt <- dt[,..keeps]
  
  # Rename keeps
  setnames(
    dt, 
    c("date_recorded", "new_class", "sale_amount", "serial_number"),
    c("date", "property_type", "sale_price", "serial_num")
  )
  
  #Convert back to date
  dt[, date := lubridate::ymd(date)]
  
  # Fix missing sales_price when sales_ratio available, drop otherwise
  dt <- impute_sale_price(dt)
  
  # Drop NA sale_price rows and drop remaining duplicates if any
  dt <- dt[!is.na(sale_price)]
  dt <- unique(dt, by = c("date", "address", "sale_price"))
  
  return(dt)
}