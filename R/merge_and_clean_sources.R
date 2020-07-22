# Rbind files from disc and Socrata in favor of files from disc
merge_and_clean_sources <- function(dt1, dt2) {
  
  # Merge both manual and OPM cleaned rows
  if(class(dt1) != "try-error") {
    dt <- rbind(dt1, dt2, fill=TRUE)
  } else {
    dt <- dt2
  }
  
  # Trim white space to get good matches
  cols <- "address"
  dt[, (cols) := lapply(.SD, trimws), .SDcols=cols]
  
  # Keep manual if available
  dt_filtered <- 
    dt[, .SD[which.max(source)], .(date, address, sale_price)]
  
  # Run final_cleanup to add year/qtr, remove dupes, fix some Multi-Family classifications
  dt_filtered <- dt_filtered %>% final_cleanup() 
    
  # Add column for years since assessment revaluation
  dt_filtered <- dt_filtered %>% add_reval()
  
  return(dt_filtered)
  
}