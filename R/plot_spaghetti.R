# spaghetti of every town of average sales price used in Shiny app
# town_name and property specified from Shiny ui
plot_spaghetti <- function(dt, town_name, type){
  
  dt <-  
    dt[, 
       mean(as.numeric(sale_price), na.rm = TRUE),
       .(town, year, property_type)][, .(
         `Average Price` = V1,
         `Year` = year,
         `Town` = town,
         property_type
       )][(property_type == type &
             `Average Price` > 20000)]
  
  town <- dt[Town == town_name]
  
  ks <- function (x) { number_format(accuracy = 1,
                                     scale = 1/1000,
                                     suffix = "k",
                                     big.mark = ",")(x) }
  
  dt[Town != town_name,
     ggplot(.SD,
            aes(`Year`,
                `Average Price`,
                group = `Town`)) +
       geom_line() +
       theme_bw() +
       geom_line(data = town,
                 aes(`Year`,
                     `Average Price`,
                     col = "red"),
                 size = 0.5) +
      scale_y_continuous(trans = "log10",
                         labels = ks) +
       labs(title = "CT Average Annual Sales Price by Town since 1999",
            subtitle = "Selected municipality shown in red",
            caption = "Public data via CT data",
            y = "Average Price - Log Scale ($K)") +
       hrbrthemes::theme_ipsum_rc(grid = "XY",
                                  strip_text_face = "bold") +
       theme(legend.position = "none")]

}