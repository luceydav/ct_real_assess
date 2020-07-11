# Rbind files from disc and Socrata in favor of files from disc
merge_and_filter_sources <- function(dt1, dt2) {
  
  if(class(dt1) != "try-error") {
    dt <- rbind(dt1, dt2, fill=TRUE)
  } else {
    dt <- dt2
  }
  cols <- "address"
  dt[, (cols) := lapply(.SD, trimws), .SDcols=cols]
  dt_filtered <- 
    dt[, .SD[which.max(source)], .(date, address, sale_price)]
  
  return(dt_filtered)
  
}