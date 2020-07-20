## Load your packages.
library(shiny)
library(data.table)
library(ggplot2)
library(stringr)
library(lubridate)
library(plotly) 
library(highcharter)
library(formattable)
library(leaflet)
library(viridis)
library(ggbeeswarm)
library(hrbrthemes)
library(scales)   
library(arules)

# Data
data_input <- 
  setDT(readRDS("ct_sales_99_2018.RDS"))

## Load your R files
lapply(list.files("./R", full.names = TRUE, pattern = "plot"), source)

shinyApp(
  
  ui <- fluidPage(
    
    titlePanel("Connecticut Property Selling Prices vs Assessment Values over Three Revaluation Cycles"),
    
    # App title ----
    titlePanel(""),
    
    sidebarLayout(
      sidebarPanel(
        helpText("Choose Inputs Here:",
                 br(),
                 "(Default is Hartford Single Family Residential)"),
        selectInput("town_name", 
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
        radioButtons("property",
                     label="Choose a Property Type",
                     choices = c(
                       "Single Family",
                       "Condo/Apartment",
                       "Multi-Family",
                       "Vacant",
                       "Commercial",
                       "Industrial"
                     ),
                     selected = "Single Family"), 
        
      ),
      
      mainPanel(
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs", 
                    tabPanel("Summary", verbatimTextOutput("summary")),
                    tabPanel("Spaghetti", plotly::plotlyOutput("spaghetti")),
                    tabPanel("Timechart", plotOutput("timechart")),
                    tabPanel("Dotplot", plotOutput("dotplot"))
        ),
        strong("Sources:"),
        br(),
        a(href="https://data.ct.gov/Housing-and-Development/Real-Estate-Sales-2001-2017/5mzw-sjtu", "Real Estate Sales 2001-2017"),
        br(),
        br(),
        strong("Methodology"),
        p(
          "The State of CT Office of Policy & Management discloses real estate sales in annual databases which have varying
          formats and classifications, Redwall Analytics used judgement in cleaning and reclassifying transactions, but another analyst
          might make different decisions. There are 580,503 Single Family, 113,659 Condo/Apartments, 86,700 Multi-Family, 52,618
          Vacant Lots, 19,991 Commercial and 4,100 Industrial Properties included. Only transactions which were judged to be arms
          length based on the town-specified non-use-code were included. The Summary tab shows the median, average and total units sold 
          by year for the chosen town. R2 is the coefficient of determination of the assessment value on sales price. The higher the R2, 
          the more accurately the assessment values predicted the selling prices in that year and town for the specified property category. 
          The Spaghetti Plot shows the average annual price over time of that town relative to all the other towns in CT for the selected 
          product category. The Time Chart tab shows the median selling price divided  by median assessment value for that year and 
          selected town. Assessment ratios close to 1x indicate the selling price was equal to the assessed value. Values closer to 1.5x 
          for example indicate the group was 'under-assessed' by 50% relative to a group assessed at 1.0x. The Dot Plot shows the ratio of 
          selling price to assessment price for each individual home sold (ie: not grouped as in the Time Chart). A Dot Plot indicating fair 
          assessment values should look purple with the darker (lesser valued) effectively covering the lighter (more expensive) homes sold. 
          Separation with the dark dots on the left closer to 1 shows possible under assessment of more expensive homes. Hartford which 
          is shown as the default here is one of the worst cases with clear separation between purple and green/yellow.", 
          value = "title"
        ), 
        br()
      )
    )),
  
  server <- function(input, output, session) {
    
    # Set up town_name() and property() for filter
    town_name <- reactive(input$town_name[1])
    property <- reactive(input$property[1])
    
    # Set up full sales data of arms length sales from 1999-2018
    d <- reactive(
      
      ##Calculate summary stats
      data_input[((
        non_use_code == "0" |
          str_detect(non_use_code, "^$") |
          is.na(non_use_code)) &
          year(date) < 2019 &
          property_type %in%
          c(
            "Single Family",
            "Condo/Apartment",
            "Multi-Family",
            "Vacant",
            "Commercial",
            "Industrial"
          ))]
    )
    
    header <- reactive(
      
      ##Calculate town and property_type summary stats
      d()[(town == town_name() &
             property_type == property()),
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
              by = `Year`][
              ][order(`Year`, decreasing = FALSE)]
    )
    
    output$spaghetti <- plotly::renderPlotly({
      
      plot_spaghetti(d(), town_name(), property())
      
    })
    
    output$timechart <- renderPlot({
      
      plot_timeplot(d(), town_name(), property())
      
    })
    
    output$dotplot <- renderPlot({
      
      plot_dotplot(d(), town_name(), property())
      
    })
    
    output$summary <- renderPrint({
      
      header()
      
    })
  }
)