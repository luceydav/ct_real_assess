

# Load packages
packages <- 
  c("dplyr",
    "data.table",
    "arules",
    "stringr",
    "zoo",
    "readr",
    "Hmisc")

if (length(setdiff(packages,rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

invisible(lapply(packages, library, character.only = TRUE))

path <- "~/Desktop/David/Projects/ct_real_estate/"

#Function to equalize ResidentialType with other datasets
create_residential_type <- function(a, b, c, d) {
  new <- dplyr::case_when(
    !is.na(a) ~ "Single",
    !is.na(b) ~ "Multiple",
    !is.na(c) ~ "Multiple",
    !is.na(d) ~ "Multiple",
    TRUE ~ "Unknown"
  )
  return(new)
}

#Function to equalize ResidentialType with other datasets
create_property_type <- function(a, b, c, d, e, f, g) {
  new <- dplyr::case_when(
    !is.na(a) ~ "Residential",
    !is.na(b) ~ "Residential",
    !is.na(c) ~ "Residential",
    !is.na(d) ~ "Commercial",
    !is.na(e) ~ "Industrial",
    !is.na(f) ~ "Vacant",
    !is.na(g) ~ "Utility",
    TRUE ~ "Unknown"
  )
  return(new)
}

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

# Clean labels for use when imported mdb data.frame has labeled columns
clear_labels <- function(x) {
  if(is.list(x)) {
    for(i in seq_along(x)) {
      class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
      attr(x[[i]],"label") <- NULL
    } 
  } else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}

# Pre 2006 period loaded together because formatting is similar
# Source: https://portal.ct.gov/OPM/IGPP-MAIN/Publications/Real-Estate-Sales-Listing
# Go through annual CT disclosures for 1995-1999 and load into corresponding data.table
# These disclosures may be MS Access, csv or xls
# These annual databases are no longer disclosed on the State of CT website 
re_1999 <- # Load MS Access DB
  setDT(
    Hmisc::mdb.get(paste0(path, "data/99_sales.mdb"), 
                   tables = "MasterTownDetail")
    )
re_1999[, DateRecorded := extract_date_from_chr(DateRecorded)]
# Load 2000 sales from xls
re_2000 <- 
  setDT(readxl:: read_excel(
    paste0(path, "data/sales_2000.xls"),
    col_types = c(
      "numeric", "text", "text",
      "text", "text", "text", "text", "text",
      "text", "text", "text", "text", "text",
      "text", "text", "text", "text", "numeric", 
      "numeric", "numeric", "numeric",
      "text", "text", "numeric")))
re_2000[, DateRecorded := extract_date_from_chr(DateRecorded)]
# Load 2001 mdb and clear_labels
re_2001 <- 
  setDT(
    clear_labels(
      Hmisc::mdb.get(
        paste0(path, "data/2001RealEstateSales.mdb"), 
        tables = "dbo_MasterTownDetail"
        )
      )
  )
re_2001[, DateRecorded := extract_date_from_chr(DateRecorded)]
# 2002-2003 csv
re_2002 <- fread(paste0(path, "data/2002RealEstateSales.csv"))
re_2002[, DateRecorded := extract_date_from_chr(DateRecorded)]
re_2003 <- fread(paste0(path, "data/2003RealEstateSales.csv"))
re_2003[, DateRecorded := extract_date_from_chr(DateRecorded)]
# 2004-2005 mdb and clear_labels
re_2004 <-
  setDT(
    clear_labels(
      Hmisc::mdb.get(
        paste0(path, "data/2004RealEstateSales.mdb"),
        tables = "2004RealEstateSales"
        )
      )
  )
re_2004[, DateRecorded := extract_date_from_chr(DateRecorded)]
re_2005 <-
  setDT(
    clear_labels(
      Hmisc::mdb.get(
        paste0(path, "data/2005RealEstateSales.mdb"),
        tables = "2005RealEstateSales"
        )
      )
  )
re_2005[, DateRecorded := extract_date_from_chr(DateRecorded)]
# Bind all early years into an aggregated data.table
re_early <- rbindlist(
  list(re_1999, 
       re_2000, 
       re_2001, 
       re_2002, 
       re_2003, 
       re_2004, 
       re_2005),
  fill = TRUE,
  use.names = TRUE
)

# Convert blank to NA
re_early[, DateRecorded := as.character(DateRecorded)]
re_early[re_early == "" | re_early == " " | re_early == "  "] <- NA

# Trim white space for all variables in case data input was messy                 
cols <- names(re_early)
re_early <- 
  re_early[,
           (cols) := lapply(.SD, trimws),
           .SDcols = cols]

# Merge Residential type into 3 levels with create_residential_type function above
# Create multi-family column to denote if Single Family, Multiple or Unknown
# This was made necessary for years where separate columns for num of families were disclosed
re_early[, 
         multi_family :=
           create_residential_type(
             OneFamily,
             TwoFamily,
             ThreeFamily,
             FourFamily)]

# Create alternate PropertyType1 column to gather info from separate column types where available
# uses create_property_type defined above
re_early[, 
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

# Used cross tables to best use available info to classify transactions into three main categories
# Single family on its own, Single family which is an apartment/condo or multi-family commericial or likely investment
re_early[PropertyType == "A" &
           PropertyType1 %in% c("Residential", "Unknown"), PropertyType1 := "Condo/Apartment"]
re_early[PropertyType == "R" &
           PropertyType1 == "Residential", PropertyType1 := "Single Family"]
re_early[PropertyType == "R" &
           PropertyType1 == "Unknown", PropertyType1 := "Single Family"]
re_early[PropertyType == "V" &
           PropertyType1 %in% c("Residential", "Unknown"), PropertyType1 := "Unknown"]
re_early[PropertyType %in% c("C", "C/I/U") &
           PropertyType1 == "Commerical", PropertyType1 := "Commercial"]
re_early[PropertyType == "C/I/U" &
           PropertyType1 == "Industrial", PropertyType1 := "Industrial"]
re_early[Condo == "X" &
           PropertyType1 == "Single Family", PropertyType1 := "Condo/Apartment"]
re_early[multi_family == "Multiple" &
           PropertyType1 == "Single Family", PropertyType1 := "Condo/Apartment"]
re_early[multi_family == "Multiple" &
           PropertyType1 %in% c("Commercial",
                                "Condo/Apartment",
                                "Industrial",
                                "Residential",
                                "Unknown",
                                "Vacant"), PropertyType1 := "Multi-Family"]

# Data has city codes, but not names so need to extract names and add
town_table <- # Extract town codes and clean
  as.data.table(as.matrix(
    Hmisc::mdb.get(paste0(path, "data/99_sales.mdb"), 
                   tables = "TownTable")))

town_table <- # Trim white space in towns table
  as.data.table(sapply(town_table, trimws))

# Join to add Town names on town codes
re_early <- 
  town_table[
    re_early,
    on = c("Town.Code" = "TownCode")]

# Fix names
setnames(
  re_early,
    c("Town.Name", "TotalAssessment", "PropertyLocation"),
    c("Town", "AssessedValue", "Address")
  )

# Clean names
re_early <- janitor::clean_names(re_early)

# Choose vars to keep
keeps <-
  c(
    "address",
    "non_use_code",
    "town",
    "date_recorded",
    "list_year",
    "property_type1",
    "assessed_value",
    "sales_ratio",
    "sale_amount",
    "serial_number"
  )
re_early <- re_early[,..keeps]

# Rename keeps
setnames(
  re_early, 
  c(
    "address",
    "non_use_code",
    "town",
    "date_recorded",
    "list_year",
    "property_type1",
    "assessed_value",
    "sales_ratio",
    "sale_amount",
    "serial_number"
  ), 
  c(
    "address",
    "non_use_code",
    "town",
    "date" ,
    "list_year",
    "property_type",
    "assessed_value",
    "sales_ratio",
    "sale_price",
    "serial_num"
  )
)

#Convert back to date
re_early[, date := lubridate::ymd(date)]

# Period after 2006 loaded together because formatting is similar
# Load single year sets']
# 2006-2010 MDBs
re_2006 <- 
  setDT(
    clear_labels(
      Hmisc::mdb.get(
        paste0(path, "data/2006RealEstateSales.mdb"),
        tables = "2006RealEstateSales"
        )
      )
  )
re_2006[, Date.Recorded := extract_date_from_chr(Date.Recorded)]
re_2007 <-
    setDT(
      Hmisc::mdb.get(
        paste0(path, "data/2007RealEstateTransactions.mdb"),
        tables = "2007RealEstateTransactions"
        )
    )
re_2007[, DateRecorded := extract_date_from_chr(DateRecorded)]
re_2008 <- 
  setDT(
    Hmisc::mdb.get(
      paste0(path, "data/SalesRatio_2008.mdb"),
        tables = "TownDetails"
      )
  )
re_2008[, DateRecorded := lubridate::mdy(DateRecorded)]
re_2009 <- 
  setDT(
    Hmisc::mdb.get(
      paste0(path, "data/SalesRatio_2009.mdb"),
      tables = "TownDetail"
      )
  )
re_2009[, DateRecorded := lubridate::mdy(DateRecorded)]
re_2010 <- 
  setDT(
    Hmisc::mdb.get(
      paste0(path, "data/SalesRatio_2008.mdb"),
      tables = "TownDetails"
      )
  )
re_2010[, DateRecorded := lubridate::mdy(DateRecorded)]
# 2011 xlsx
re_2011 <- 
  setDT(readxl::read_excel(
    paste0(path, "data/SalesRatio2011xlsx.xlsx"),
    col_types =
      c(
        "text",
        "numeric",
        "numeric",
        "date",
        "text",
        "text",
        "text",
        "numeric",
        "numeric",
        "numeric",
        "text",
        "numeric",
        "text"
      )
  ))
# Date format was a problem and had to be cleaned upfront
re_2011[, DateRecorded := as.Date(DateRecorded)]

# Load 18 years of data from main CT property sales database
re_big <- 
  fread(paste0(path, "data/Real_Estate_Sales_2001-2017.csv"))
# Filter re_big for years without source data (ie: 2012-2017)
#re_big[ , DateRecorded := as.Date(as.POSIXct(DateRecorded, format = "%m/%d/%Y %H:%M:%S"))]
re_big[, DateRecorded := extract_date_from_chr(DateRecorded)]
re_big <- 
  re_big[year(DateRecorded) %in% c(2012:2018)]


# Bind all sets
re_late <- rbindlist(
  list(re_2006, 
       re_2007, 
       re_2008, 
       re_2009, 
       re_2010, 
       re_2011,
       re_big),
  fill = TRUE,
  use.names = TRUE)

cols <-
  c(
    "SalePrice",
    "AssessedValue",
    "Assessed.Value",
    "Sale.Price",
    "SaleAmount",
    "SalesRatio",
    "Sales.Ratio",
    "Serial.Number",
    "SerialNumber",
    "SerialNbr",
    "Residential.Units",
    "ResidentialUnits",
    "List.Year",
    "ListYear"
  )
re_late[, (cols) := lapply(.SD, as.integer), .SDcols=cols]

# Coalesce variable names
re_late[,`:=`
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
            fcoalesce(SaleAmount, Sale.Price, SalePrice),
          serial_num =
            fcoalesce(Serial.Number, SerialNumber, SerialNbr),
          residential_units =
            fcoalesce(Residential.Units, ResidentialUnits
            )
        )]
# Drop unneeded vars after coalesce
drops <- names(re_late)[str_detect(names(re_late), "^[A-Z].*")]
re_late <- re_late[,-drops, with = FALSE]

re_late[residential_type == "", residential_type := "Unknown"]
re_late[residential_type == "Apartments", property_type := "Condo/Apartment"]
re_late[residential_type == "Commercial", property_type := "Commercial"]
re_late[residential_type %in% c("Condo", "Condo Family") &
          (property_type %in% c("", "Residential") |
             is.na(property_type)), property_type := "Condo/Apartment"]
re_late[residential_type == "Condo" &
          property_type == "Commercial", property_type := "Multi-Family"]
re_late[residential_type %in% c("Multi Family", "Four Family", "Three Family", "Two Family"), property_type := "Multi-Family"]
re_late[residential_type == "Industrial", property_type := "Industrial"]
re_late[residential_type == "Public Utility", property_type := "Utility"]
#re_late[residential_type == "Residential", property_type := "Single Family"]
re_late[residential_type == "Residential" &
          property_type == "", property_type := "Single Family"]
re_late[residential_type == "Single Family" &
          (property_type %in% c("", "Residential") |
             is.na(property_type)), property_type := "Single Family"]
re_late[residential_type == "Vacant Land", property_type := "Vacant Land"]
re_late[property_type == "Residential", property_type := "Single Family"]
re_late[property_type %in% c("Apartments", "Condo"), property_type := "Condo/Apartment"]

# Trim white space for all variables
cols <- names(re_late[,!"date", with = FALSE])
re_late[, (cols) := lapply(.SD, trimws), .SDcols = cols]

#Bind early and late
re_total <- rbind(re_early, re_late, use.names = TRUE, fill = TRUE)

# Drop bad dates
re_total <- re_total[year(date) < 2019]

# Final clean up and sync variables types after join
re_total[,`:=`(
  sale_price = readr::parse_number(sale_price),
  assessed_value = readr::parse_number(assessed_value),
  sales_ratio = readr::parse_number(sales_ratio)
)]

# Add year and quarter variables
re_total[, `:=`(      
  qtr = zoo::as.yearqtr(date),
  year = year(date))]

# Fix sales_ratios in town years which are 100x
key <- # create key for which town years are 100x or 1x
  re_total[,
           .(median(as.numeric(sales_ratio), na.rm = TRUE)),
           .(town, year)]
# create conversion factor to mutate sales_ratio when 100x
key[, factor := fifelse(V1 < 5, 1, 0.01)]
# left join re_full with key
# add sales_ratio_1 as key$factor * sales_ratio
re_total <- 
  key[re_total[,year:=year(date)], 
      on = c("year", "town")][
      ][, sales_ratio := factor * sales_ratio][
      ][, `:=` (
        V1 = NULL,
        factor=NULL)]

# Filter unique rows by data/address
re_unique <- unique(re_total, by=c("address", "date", "sale_price"))

reval <-
  fread(paste0(path, "data/Revaluation_Years_by_Town.csv"))[, 2:3]
reval <- janitor::clean_names(reval)

source("~/Desktop/David/Projects/CT_data/reshape_reval.R")

# Join with reval years
re_unique <-
  reval[re_unique[,
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
                   year
                 )],
        on = "town"]

# Calculate reval year based on every 5 years
re_unique[,reval_yr := 
            reshape_reval(next_reval_year, year)]

# Save
saveRDS(re_unique, "ct_sales_99_2018.RDS")

# Clean up
rm(list=(ls()[!grepl('re_unique',ls())]))
