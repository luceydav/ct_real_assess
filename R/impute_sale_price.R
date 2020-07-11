
# If sales price is missing, impute with sales_ratio
impute_sale_price <- function(dt) {
  
  # Add year and quarter variables
  # sales_ratio and sale_price were converting to chr for unknown reason
  dt[, `:=`(      
    qtr = zoo::as.yearqtr(date),
    year = year(date),
    sales_ratio = as.numeric(sales_ratio),
    sale_price = as.numeric(sale_price)
  )]
  
  # Fix sales_ratios in town years which are 100x
  # create key for which town years are 100x or 1x
  key <- dt[, .(median(sales_ratio, na.rm = TRUE)),
            .(town, year)]
  # create conversion factor to mutate sales_ratio when 100x
  key[, factor := fifelse(V1 < 5, 1, 0.01)]
  # left join re_full with key
  # add sales_ratio_1 as key$factor * sales_ratio
  dt <- 
    key[dt, on = c("year", "town")][
    ][, sales_ratio := factor * sales_ratio][
    ][, `:=` (V1 = NULL,
              factor = NULL)]
  
  # If sales price is missing, impute with sales_ratio
  dt[sale_price == 0 & 
       assessed_value != 0 & 
       sales_ratio != 0, 
     sale_price := assessed_value / sales_ratio]
  
  # Return
  return(dt)
  
}