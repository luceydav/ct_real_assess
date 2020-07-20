# timemplot of sales ratio used in Shiny app
# town_name and property specified from Shiny ui
plot_timeplot <- function(dt, town_name, property){
  
  ##Line chart over time
  dt[(town == town_name & 
        property_type == property &
        assessed_value > 1000), #some missing AssessedValue's throwing off Inf SP/AV
     .(property_type,
       qtr, #Calc quarter
       sale_price,
       assessed_value)][
       ][,
         .(
           qtr,
           sale_price,
           property_type,
           assessed_value,
           SP_AV = sale_price / (assessed_value * 1 / 0.7),
           SP_levels = discretize(
             sale_price,
             method = "frequency",
             breaks = 3,
             labels = c("Lowest", "Medium", "Highest")
           )
         )][, .(mean_sp_av = mean(SP_AV, na.rm = TRUE)), 
            by = c("SP_levels", "qtr", "property_type")][
            ][mean_sp_av < 5,
              ggplot(.SD) +
                geom_line(
                  aes(
                    qtr, 
                    mean_sp_av, 
                    color = SP_levels), 
                  linetype = "dashed") +
                labs(
                  title = paste(
                    "The Highest Priced 2/3 of",
                    property,
                    "in",
                    town_name,
                    "Appear to Be Under-Assessed",
                    sep = " "
                  ),
                  subtitle = "Lowest Segment Shown in Purple",
                  caption = "Public data via CT Data"
                ) +
                ylab("Assessment Ratio") +
                xlab("Quarter") +
                theme_bw() +
                facet_wrap( ~ property_type)+
                viridis::scale_color_viridis(discrete = TRUE, option = "D") +
                hrbrthemes::theme_ipsum_rc(grid = "XY", 
                                           strip_text_face = "bold") +
                theme(legend.position = "bottom", legend.direction = "horizontal") +
                scale_y_discrete()]
}
