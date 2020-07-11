# Plot to see that all rows are properly classified
# Not used in Shiny app
plot_results <- function(dt){
  
  plotly::ggplotly(
    dt[((str_detect(non_use_code, "0|^$") | is.na(non_use_code)) & 
          year(date) %in% c(1999:2018)), 
       mean(as.numeric(sale_price), na.rm = TRUE), .(town, year,  property_type)][
         (property_type %in% c("Single Family", "Condo/Apartment", "Multi-Family") &
            V1 > 20000), 
         ggplot(.SD, aes(year, V1, group = town)) +
           scale_y_log10(labels = scales::unit_format(unit = "K", scale = 1e-3)) +
           geom_line() +
           labs(
             title = "CT Average Annual Sales Price by Town since 1999",
             x = "",
             y = ""
           ) +
           facet_wrap( ~ property_type) +
           theme_bw()])
}
