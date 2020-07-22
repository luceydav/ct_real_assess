
clean_late <- function(dt) {
  
  # Trim white space for all variables
  cols <- names(dt)[!str_detect(names(dt), "Date")]
  dt[, (cols) := lapply(.SD, trimws), .SDcols = cols]
  
  # Align date type
  cols <- c("DateRecorded", "Date.Recorded")
  dt[, (cols) := lapply(.SD, as.Date), .SDcols=cols]
  dt <- dt[!is.na(DateRecorded)]
  
  # Convert numeric columns
  cols <- c("SalePrice", "AssessedValue", "Assessed.Value", "Sale.Price")
  dt[, (cols) := lapply(.SD, readr::parse_number), .SDcols=cols]
  
  # Coalesce variable names
  dt[,`:=`
     (
       address = Address,
       town =
         fcoalesce(Town, TownName, Name),
       date =
         fcoalesce(Date.Recorded, DateRecorded),
       list_year =
         fcoalesce(List.Year, ListYear),
       residential_type =
         fcoalesce(Residential.Type, ResidentialType, Description),
       property_type =
         fcoalesce(PropertyType, Land.Description, LandDescription),
       assessed_value =
         fcoalesce(Assessed.Value, AssessedValue),
       sales_ratio =
         fcoalesce(SalesRatio, Sales.Ratio),
       sale_price =
         fcoalesce(Sale.Price, SalePrice),
       serial_num =
         fcoalesce(Serial.Number, SerialNumber, SerialNbr),
       residential_units =
         fcoalesce(Residential.Units, ResidentialUnits
         )
     )]
  
  # Drop unneeded vars after having been coalesced
  drops <- names(dt)[str_detect(names(dt), "^[A-Z].*")]
  dt <- dt[,-drops, with = FALSE]
  
  dt[residential_type == "", 
     residential_type := "Unknown"]
  dt[residential_type == "Apartments", 
     property_type := "Condo/Apartment"]
  dt[residential_type == "Commercial", 
     property_type := "Commercial"]
  dt[residential_type == "Condo" &
       property_type == "Residential", 
     property_type := "Condo/Apartment"]
  dt[residential_type %in% c("Condo Family",
                             "Multi Family",
                             "Four Family",
                             "Three Family",
                             "Two Family"), 
     property_type := "Multi-Family"]
  dt[residential_type == "Industrial", 
     property_type := "Industrial"]
  dt[residential_type == "Public Utility", 
     property_type := "Utility"]
  dt[residential_type == "Residential", 
     property_type := "Unclear"]
  dt[residential_type == "Single Family" &
       property_type %in% c("Residential", ""), 
     property_type := "Single Family"]
  dt[residential_type == "Vacant Land" &
       property_type %in% c("", "Vacant Land"), 
     property_type := "Vacant"]
  dt[property_type %in% c("Apartments", "Condo"), 
     property_type := "Condo/Apartment"]
  dt[is.na(property_type) |
       property_type == "Residential", 
     property_type := "Unclear"]
  dt[property_type == "Vacant Land", 
     property_type := "Vacant"]
  dt[property_type == "Utility", 
     property_type := "Public Utility"]
  
  # Drop 2012 sales marked Unclear 
  dt <- dt[(year(date) != 2012 & property_type != "Unclear")]
  
  # Fix missing sales_price when sales_ratio available, drop otherwise
  dt <- impute_sale_price(dt)
  dt <- dt[!is.na(sale_price)]

  dt <- unique(dt, by = c("date", "address", "sale_price"))
  
  #Return
  return(dt)
}

