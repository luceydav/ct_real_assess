# spaghetti of every town of average sales price used in Shiny app
# town_name and property specified from Shiny ui
plot_spaghetti <- function(dt, highlight, type){
  
  dt <-  
    dt[((str_detect(non_use_code, "0|^$") |
           is.na(non_use_code)) &
          year(date) %in% c(1999:2018)), 
       mean(as.numeric(sale_price), na.rm = TRUE),
       .(town, year, property_type)][, . (
         `Average Price` = V1,
         `Year` = year,
         `Town` = town,
         property_type
       )][(property_type == type &
             `Average Price` > 20000)]
  
  town <- dt[`Town` == highlight]
  dt <- dt[`Town` != highlight]
  
  ggplot(dt,
         aes(`Year`,
             `Average Price`,
             group = `Town`)) +
    scale_y_log10(labels =
                    scales::unit_format(
                      unit = "K",
                      scale = 1e-3,
                      big.mark = ","
                    )) +
    geom_line() +
    geom_line(
      data = town,
      aes(`Year`,
          `Average Price`,
          col = "red"),
      size = 0.5
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(title = "CT Average Annual Sales Price by Town since 1999",
         x = "",
         y = "") 
}