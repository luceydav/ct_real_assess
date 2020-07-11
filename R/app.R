library(shiny)
library(ggplot2)
library(lubridate)
library(arules)
library(data.table)
library(stringr)
library(plotly)
library(highcharter)
library(formattable)
library(leaflet)
library(viridis)
library(ggbeeswarm)
library(hrbrthemes)

run_shiny <- function(dt) {
  
  shinyApp(
  
    ui <- fluidPage(
      
      titlePanel("Analysis Comparing Connecticut Selling Prices to Assessment Values over Three Revaluation Cycles"),
      
      # App title ----
      titlePanel(""),
      
      sidebarLayout(
        sidebarPanel(
          helpText("Choose Inputs Here:",
                   br(),
                   "(Default is Hartford Single Family Residential)"),
          selectInput("town", 
                      label = "Choose a Town to display",
                      choices = c("Andover","Ansonia","Ashford","Avon","Barkhamsted","Beacon Falls","Berlin","Bozrah","Bridgeport",
                                  "Brooklyn","Burlington","Canaan","Canterbury","Canton","Chaplin","Cheshire","Chester",
                                  "Clinton","Colchester","Colebrook","Columbia","Cornwall","Coventry","Cromwell","Danbury",
                                  "Darien","Deep River","Derby","Durham","East Granby","East Haddam","East Hampton",
                                  "East Hartford","East Haven","East Lyme","East Windsor","Eastford","Easton","Ellington",
                                  "Enfield","Essex","Fairfield","Farmington","Franklin","Glastonbury","Goshen","Granby",
                                  "Greenwich","Griswold","Groton","Guilford","Haddam","Hamden","Lisbon","Hampton","Hartford",
                                  "Hartland","Harwinton","Hebron","Kent","Killingly","Killingworth","Lebanon","Ledyard","Litchfield",
                                  "Lyme","Madison","Manchester","Mansfield","Marlborough","Meriden","Middlebury","Middlefield",
                                  "Middletown","Milford","Monroe","Montville","Morris","Naugatuck","New Britain","New Canaan",
                                  "New Fairfield","New Hartford","New Haven","New London","New Milford","Newington","Newtown",
                                  "Norfolk","North Branford","North Canaan","North Haven","North Stonington","Norwalk","Norwich",
                                  "Old Lyme","Old Saybrook","Orange","Oxford","Plainfield","Plainville","Plymouth","Pomfret",
                                  "Portland","Preston","Prospect","Putnam","Redding","Ridgefield","Rocky Hill","Roxbury","Salem",
                                  "Salisbury","Scotland","Seymour","Sharon","Shelton","Sherman","Simsbury","Somers","South Windsor",
                                  "Southbury","Southington","Sprague","Stafford","Stamford","Sterling","Stonington","Stratford",
                                  "Suffield","Thomaston","Thompson","Tolland","Torrington","Trumbull","Union","Vernon","Voluntown",
                                  "Wallingford","Warren","Washington","Waterbury","Waterford","Watertown" ,"West Hartford","West Haven",
                                  "Westbrook" ,"Weston","Westport","Wethersfield","Willington","Wilton","Winchester","Windham","Windsor",
                                  "Windsor Locks","Wolcott","Woodbridge","Woodbury","Woodstock"),
                      selected = "Hartford"),
          radioButtons("property_type",
                       label="Choose a Property Type",
                       choices=c("Single Family", "Condo/Apartment", "Multi-Family"),
                       selected="Single Family")
        ),
        
        mainPanel(
          strong("Sources:"),
          br(),
          a(href="https://data.ct.gov/Housing-and-Development/Real-Estate-Sales-2001-2017/5mzw-sjtu", "Real Estate Sales 2001-2017"),
          br(),
          strong("Methodology"),
          p("The Summary tab shows the median, average and total units sold by year for the chosen town. 
        There are 599,330 Single Family, 139,967 Condo/Apartments and 57,591 Multi-Family Homes included. 
        Because the data is disclosed in annual databases which have varying formats and classifications, 
        Redwall Analytics used judgement in cleaning and reclassifying transactions. Another analyst might make 
        different decisions. The number of units sold during the period and  R2 is the coefficient of determination 
        of the assessment value on the sales price. The higher the R2, the more accurately the assessment value predicted 
        the selling price in that year. The Time Chart tab shows the median selling price divided by median assessment 
        value for that year and selected town. Assessment ratios close to 1x indicate the selling price was equal to the 
        assessed value. Values closer to 1.5x for example indicate the group was 'under-assessed' by 50% relative to a 
        group assessed at 1.0x. The Dot Plot shows the ratio of selling price to assessment price for each individual 
        home sold (ie: not grouped as in the Time Chart). A Dot Plot indicating fair assessment values should look purple
        with the darker (lesser valued) effectively covering the lighter (more expensive) homes sold. Separation with the
        dark dots on the left closer to 1 shows possible under assessment of more expensive homes. Hartford which is shown 
        as the default here is one of the worst cases with clear separation between purple and green/yellow.", value="title"),
          br(),
          br(),
          
          # Output: Tabset w/ plot, summary, and table ----
          tabsetPanel(type = "tabs", 
                      tabPanel("Summary", verbatimTextOutput("summary")),
                      tabPanel("Spaghetti", plotlyOutput("spaghetti")),
                      tabPanel("Timechart", plotOutput("timechart")),
                      tabPanel("Dotplot", plotOutput("dotplot"))
          )
        )
      )), 
    server <- function(input, output) {
      
      d <- reactive({
        
        ##Calculate summary stats
        dt[town %in% input$town[1] & #filter FF county towns
             property_type %in% input$property_type[1],
           .(town,
             sale_price,
             assessed_value = assessed_value / 0.7,
             `Year` = year,
             reval_yr)][
               , .(
                 `Median` =
                   format(round(median(sale_price, na.rm = TRUE), 0), big.mark = ","),
                 `Average` =
                   format(round(mean(sale_price, na.rm = TRUE), 0), big.mark = ","),
                 `Units` = format(.N, big.mark = ","),
                 `R2` = summary(lm(sale_price ~ assessed_value))$adj.r.squared,
                 `Since_Reval` = mean(reval_yr)
               ),
               by = `Year`][order(`Year`, decreasing = FALSE)]
        
      })
      
      output$spaghetti <- renderPlotly({
        
        highlight <- input$town[1]
        type <- input$property_type[1]
        
        plot_spaghetti(dt, highlight, type)
        
      })
      
      output$timechart <- renderPlot({
        
        town <- input$town[1]
        property <- input$property_type[1] 
        
        plot_timeplot(dt, town, property)
        
      })
      
      output$dotplot <- renderPlot({
        
        town_name <- input$town[1]
        property <- input$property_type[1] 
        
        plot_dotplot(dt, town_name, property)
        
      })
      
      output$summary <- renderPrint({
        d()
      })
      
    }
  )
}
