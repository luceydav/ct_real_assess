
# Customized to clean early years
clean_early <- function(dt){
  
  # First move date back to chr then convert blank to NA
  dt[, DateRecorded := as.character(DateRecorded)]
  dt[dt == "" | dt == " " | dt == "  "] <- NA
  
  # Trim white space for all variables in case data input was messy                 
  cols <- names(dt)
  dt <- dt[, (cols) := lapply(.SD, trimws), .SDcols = cols]
  
  cols <-
    c("TotalAssessment", "SaleAmount")
  dt[, (cols) := lapply(.SD, readr::parse_number), .SDcols=cols]
  
  # Create multi-family column to denote if Single Family, Multiple or Unknown
  # This was made necessary for years where separate columns for number of families were disclosed
  dt[, residential_type := 
       create_residential_type(
         OneFamily,
         TwoFamily,
         ThreeFamily)]
  
  # Create alternate PropertyType1 column to gather info from separate column types where available
  # uses create_property_type defined above
  dt[, 
     PropertyType1 :=
       create_property_type(
         Residential,
         ResidentialOther,
         Apartment,
         Commercial,
         Industrial,
         VacantLand,
         Utility
       )]
  
  # Manual Allocations using all data
  dt[((PropertyType == "R" |
         PropertyType1 == "Residential") &
        Condo == "X" & 
        residential_type == "Single Family"),
     new_class := "Condo/Apartment"]
  dt[((PropertyType == "R" |
         PropertyType1 == "Residential") &
        residential_type %in% c("Three Family", "Two Family")),
     new_class := "Multi-Family"]
  dt[((PropertyType == "R" | 
         PropertyType1 == "Residential") &
        is.na(Condo) & 
        residential_type == "Single Family"),
     new_class := "Single Family"]
  dt[PropertyType1 == "Commercial" & 
       PropertyType != "R" &
       !is.na(new_class),
     new_class := "Commercial"]
  dt[PropertyType1 == "Industrial" & 
       PropertyType != "R" &
       is.na(new_class),
     new_class := "Industrial"]
  dt[PropertyType1 == "Vacant" | 
       PropertyType == "V" &
       is.na(new_class),
     new_class := "Vacant"]
  dt[PropertyType == "A" & 
       residential_type != "Single Family" &
       is.na(new_class),
     new_class := "Multi-Family"]
  dt[PropertyType == "A" & 
       residential_type == "Single Family" &
       is.na(new_class), 
     new_class := "Condo/Apartment"]
  dt[PropertyType == "C/I/U" & 
       PropertyType1 == "Utility" &
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
  
  # Join to add Town names on town codes
  dt <- town_table[dt, on = c("Town.Code" = "TownCode")]
  
  # Fix names
  setnames(
    dt,
    c("Town.Name", "TotalAssessment", "PropertyLocation"),
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