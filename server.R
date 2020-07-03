library(shiny)
library(ggplot2)
library(lubridate)
library(arules)
library(data.table)
library(stringr)
library(highcharter)
library(formattable)
library(lubridate)
library(leaflet)
library(viridis)
library(ggbeeswarm)
library(hrbrthemes)

re <- readRDS("ct_sales_99_2018.RDS")

server <- function(input, output) {
  
  d <- reactive({
    
    ##Calculate summary stats
    re[town %in% input$town[1] & #filter FF county towns
         property_type %in% input$property_type[1],
       .(town,
         sale_price,
         assessed_value = assessed_value / 0.7,
         `Year` = year,
         reval_yr)][
           ,.(`Median`=format(round(median(sale_price, na.rm=TRUE),0),big.mark=","),
              `Average`=format(round(mean(sale_price, na.rm=TRUE),0),big.mark=","),
              `Units`=format(.N,big.mark=","),
              `R2` = summary(lm(sale_price~assessed_value))$adj.r.squared,
              `Since_Reval`=mean(reval_yr)),by=`Year`][
                order(`Year`,decreasing=FALSE)]
    
  })
  
  output$timechart <- renderPlot({

    ##Chart attributes
    mychartattributes <- theme_bw() + 
      theme(text=element_text(family="Avenir")) +
      theme(panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor = element_blank(),
            axis.line=element_line(colour="gray"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) 
    
      ##Line chart over time
      re[town %in% input$town[1] & 
         property_type %in% input$property_type[1] &
         assessed_value >1000, #some missing AssessedValue's throwing off Inf SP/AV
         .(property_type,
           qtr, #Calc quarter
           sale_price,
           assessed_value)][,
              .(qtr,
                sale_price,
                property_type,
                assessed_value,
                SP_AV=sale_price/(assessed_value*1/0.7),
                SP_levels=discretize(sale_price,method="frequency",breaks=3,labels=c("Lowest","Medium","Highest")))][
                    ,.(mean_sp_av=mean(SP_AV, na.rm=TRUE)),by=c("SP_levels","qtr","property_type")][
                     mean_sp_av < 5
                    ,ggplot(.SD) +
                    geom_line(aes(qtr,mean_sp_av,color=SP_levels),linetype="dashed") +
                    labs(title=paste("The Highest Priced 2/3 of",input$property_type[1],"in",input$town[1],"Appear to Be Under-Assessed",sep=" "),
                         subtitle = "Lowest Segment Shown in Purple", 
                          caption = "Public data via CT Data") +
                         ylab("Assessment Ratio") +
                         xlab("Quarter") +
                         mychartattributes +
                         facet_wrap(~property_type)+
                    viridis::scale_color_viridis(discrete = TRUE, option = "D") +
                    hrbrthemes::theme_ipsum_rc(grid="XY", strip_text_face="bold") +
                    theme(legend.position = "bottom", legend.direction = "horizontal") +
                    scale_y_discrete()]
      
  })
  
    output$dotplot <- renderPlot({
    
        ## Build dotplot comparing selling price to assessed value
        re[town %in% input$town[1] &
            property_type %in% input$property_type[1],
           .(property_type,
             qtr,
             year, #Calc quarter
             sale_price,
             assessed_value = assessed_value / 0.7)][,
               .(year,
                 SP_AV=sale_price/assessed_value,
                 SP_levels=discretize(sale_price,method="frequency",breaks=3,labels=c("Lowest","Medium","Highest")))][
                   SP_AV < 5
                   ,ggplot(.SD,aes(SP_AV, year, color = SP_levels)) +
                     geom_quasirandom(groupOnX = F) +
                     guides(colour = guide_legend(nrow = 1),
                            guide = guide_legend(title = NULL)) +
                     labs(
                       x = "Sales Price/Assessment Value",
                       y = "",
                       title = paste(input$town[1],input$property_type[1],"Sales Prices Exceed Assessment Values in Top Segments Relative To Bottom",sep=" "),
                       subtitle = "Data as of 2016 shows lesser valued home sales in purple",
                       caption = "Public data via CT Data",
                       color = ""
                     ) +
                     scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                     viridis::scale_color_viridis(discrete = TRUE, option = "D") +
                     hrbrthemes::theme_ipsum_rc(grid="XY", strip_text_face="bold") +
                     theme(legend.position = "bottom", legend.direction = "vertical")]
    
  })
  
    output$summary <- renderPrint({
      d()
    })
    
}