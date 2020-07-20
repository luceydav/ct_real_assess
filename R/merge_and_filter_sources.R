# Rbind files from disc and Socrata in favor of files from disc
merge_and_filter_sources <- function(dt1, dt2) {
  
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

  return(dt_filtered)
  
}