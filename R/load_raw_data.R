
# Pre 2006 period loaded together because formatting is similar
# Source: https://portal.ct.gov/OPM/IGPP-MAIN/Publications/Real-Estate-Sales-Listing
# Go through annual CT disclosures for 1999-2011 and load into corresponding data.table
# These disclosures may be MS Access, csv or xls stored on local disc
# The annual databases used here are no longer disclosed on the State of CT website 
load_raw_data <- function() {
  
    # Path to local
    path <- "/Users/davidlucey/Desktop/David/Projects/ct_real_estate/data/"
    
       #Load MS Access DB
      re_1999 <- 
        setDT(
        Hmisc::mdb.get(
          paste0(path, "99_sales.mdb"), 
          tables = "MasterTownDetail"
          )
        )
    
      re_1999[, DateRecorded := extract_date_from_chr(DateRecorded)]

    # Load 2000 sales from xls
    re_2000 <- 
      setDT(readxl:: read_excel(
        paste0(path, "sales_2000.xls"),
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
            paste0(path, "2001RealEstateSales.mdb"), 
            tables = "dbo_MasterTownDetail"
          )
        )
      )
    re_2001[, DateRecorded := extract_date_from_chr(DateRecorded)]
    
    # 2002-2003 csv and extract_date_from_chr
    re_2002 <- fread(paste0(path, "2002RealEstateSales.csv"))
    re_2002[, DateRecorded := extract_date_from_chr(DateRecorded)]
    
    re_2003 <- fread(paste0(path, "2003RealEstateSales.csv"))
    re_2003[, DateRecorded := extract_date_from_chr(DateRecorded)]
    
    # 2004-2005 mdb and extract_date_from_chr
    re_2004 <-
      setDT(
        clear_labels(
          Hmisc::mdb.get(
            paste0(path, "2004RealEstateSales.mdb"),
            tables = "2004RealEstateSales"
          )
        )
      )
    re_2004[, DateRecorded := extract_date_from_chr(DateRecorded)]
    
    re_2005 <-
      setDT(
        clear_labels(
          Hmisc::mdb.get(
            paste0(path, "2005RealEstateSales.mdb"),
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
    
    # Period after 2006 loaded together because formatting is similar
    # Load single year sets']
    # 2006-2010 MDBs
    re_2006 <- 
      setDT(
        clear_labels(
          Hmisc::mdb.get(
            paste0(path, "2006RealEstateSales.mdb"),
            tables = "2006RealEstateSales"
          )
        )
      )
    re_2006[, Date.Recorded := extract_date_from_chr(Date.Recorded)]
    re_2007 <-
      setDT(
        Hmisc::mdb.get(
          paste0(path, "2007RealEstateTransactions.mdb"),
          tables = "2007RealEstateTransactions"
        )
      )
    re_2007[, DateRecorded := extract_date_from_chr(DateRecorded)]
    re_2008 <- 
      setDT(
        Hmisc::mdb.get(
          paste0(path, "SalesRatio_2008.mdb"),
          tables = "TownDetails"
        )
      )
    re_2008[, DateRecorded := lubridate::mdy(DateRecorded)]
    re_2009 <- 
      setDT(
        Hmisc::mdb.get(
          paste0(path, "SalesRatio_2009.mdb"),
          tables = "TownDetail"
        )
      )
    re_2009[, DateRecorded := lubridate::mdy(DateRecorded)]
    re_2010 <- 
      setDT(
        Hmisc::mdb.get(
          paste0(path, "SalesRatio_2008.mdb"),
          tables = "TownDetails"
        )
      )
    re_2010[, DateRecorded := lubridate::mdy(DateRecorded)]
    # 2011 xlsx
    re_2011 <- 
      setDT(readxl::read_excel(
        paste0(path, "SalesRatio2011xlsx.xlsx"),
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
    
    # Bind all sets
    re_late <- rbindlist(
      list(re_2006, 
           re_2007, 
           re_2008, 
           re_2009, 
           re_2010, 
           re_2011),
      fill = TRUE,
      use.names = TRUE)
    
    # Customized cleaning for early and late
    re_early <- clean_early(re_early)
    re_late <- clean_late(re_late)
    
    #Bind early and late
    re_total <- 
      rbind(re_early, re_late, fill = TRUE)
    
    # Add source identifier for raw 
    re_total[, source := 2]
  
  # Return
  return(re_total)
  
}
