# Dotplot used in Shiny app
# town_name and property specified from Shiny ui
plot_dotplot <- function(dt, town_name, property) {
  
  ## Build dotplot comparing selling prices to assessed values
  dt[(town == town_name &
        property_type == property),
     .(property_type,
       qtr,
       year, #Calc quarter
       sale_price,
       assessed_value = assessed_value / 0.7)][,
     .(
       SP_AV = sale_price / assessed_value,
       SP_levels = discretize(
         sale_price,
         method = "frequency",
         breaks = 3,
         labels = c("Lowest", "Medium", "Highest")
       )), year][
       ][SP_AV < 5
         , ggplot(.SD, aes(SP_AV, year, color = SP_levels)) +
           geom_quasirandom(groupOnX = F) +
           guides(colour = guide_legend(nrow = 1),
                  guide = guide_legend(title = NULL)) +
           labs(
             x = "Sales Price/Assessment Value",
             y = "",
             title = paste(
               town_name,
               property,
               "Sales Prices Exceed Assessment Values in Top Segments Relative To Bottom",
               sep = " "
             ),
             subtitle = "Data as of 2018 shows lesser valued home sales in purple",
             caption = "Public data via CT Data",
             color = ""
           ) +
           scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
           viridis::scale_color_viridis(discrete = TRUE, option = "D") +
           hrbrthemes::theme_ipsum_rc(grid = "XY",
                                      strip_text_face = "bold") +
           theme(legend.position = "bottom", legend.direction = "vertical")]
}
