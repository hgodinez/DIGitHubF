library(shiny)
library(shinythemes)
library(DT)
require(shinydashboard)
require(leaflet)

# Define UI for random distribution application 
shinyUI(fluidPage(theme = "superhero.min.css",
    
  # Application title
  titlePanel("Energy and Economic Factors"),
  
  # Note the use of the
  # br() element to introduce extra vertical spacing
  
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(condition="input.conditionedPanels==7",
                       selectInput("ticker", label = h3("Ticker/ETF/Commodity"),
                                    choices= list("WTI" = "WTI","Gold" = "GLD","United States Natural Gas ETF" = "UNG",
                                                  "United States OIL ETF"="USO", "United States Brent Oil Fund"="BNO",
                                                  "Continuous Commodity ETF"="GCC"#,"USD/GBP"="USD/GBP", "USD/EUR"="USD/EUR"
                                    ), selected="WTI"),
                       br(),
                       #textInput("ticketText", label = h4("Manual ticker input"), 
                        #         value = "GOOG"),
                       #selectInput("select", "Variables to display", 
                      #             choices = list("GDP, M2, Reserves, T-bills 3, Unemployment" = 3,"Commodity,Deposits, M1, Personal Consumption and Income" = 4,  
                       #                           "Average Earnings, Inflation, House Units, M Base, Treasury 10y and Wages" = 1,
                        #                          "Discount Rate, Disposable Income, House Prices, House Supply and Loans" =2), 
                         #          selected = 3),
                       
                       #dateRangeInput("daterange", "Date range:",
                      #                start = "1990-01-01",
                       #               end   = "2015-12-31", min = "1964-06-01", max = "2016-03-31"),
                       #br(),
                       helpText("Note: This module is under construction. ")#,
                       
                       #actionButton("goFin", "Update")
      ),
      
      
      conditionalPanel(condition="input.conditionedPanels==1",
                       radioButtons("economfact", label = h3("Economic Factor"),
                                    choices= list("Inflation" = "inf","Unemployment" = "unemp","GDP" = "gdp"
                                               ), selected="inf"),
                       br(),
                       
                       selectInput("select", "Variables to display", 
                                   choices = list("GDP, M2, Reserves, T-bills 3, Unemployment" = 3,"Commodity,Deposits, M1, Personal Consumption and Income" = 4,  
                                                  "Average Earnings, Inflation, House Units, M Base, Treasury 10y and Wages" = 1,
                                                  "Discount Rate, Disposable Income, House Prices, House Supply and Loans" =2), 
                                   selected = 3),
                       
                       dateRangeInput("daterange", "Date range:",
                                      start = "1990-01-01",
                                      end   = "2015-12-31", min = "1964-06-01", max = "2016-03-31"),
                       br(),
                       helpText("Note 1: While the Average Annual Log Returns",
                                "and Correlations tabs will show the specified inputs, the Neural Network and table still be based",
                                "on the full dataset. \n",
                                "Note 2. The model for GDP is under construction."),
                       
                       actionButton("go", "Update")
      ),
      
      conditionalPanel(condition="input.conditionedPanels==2",
                       
                       selectInput("select2", "Variables to display", 
                                   choices = list("GDP, M2, Reserves, T-bills 3, Unemployment" = 3,"Commodity,Deposits, M1, Personal Consumption and Income" = 4,  
                                                  "Average Earnings, Inflation, House Units, M Base, Treasury 10y and Wages" = 1,
                                                  "Discount Rate, Disposable Income, House Prices, House Supply and Loans" =2), 
                                   selected = 3),
                       
                       dateRangeInput("daterange2", "Date range:",
                                      start = "1990-01-01",
                                      end   = "2015-12-31", min = "1964-06-01", max = "2016-03-31"),
                       br(),
                       helpText("Note 1: While the Average Annual Log Returns",
                                "and Correlations tabs will show the specified inputs, the Neural Network and table still be based",
                                "on the full dataset. \n",
                                "Note 2. The model for GDP is under construction."),
                       
                       actionButton("go2", "Update")
      ),
      
      conditionalPanel(condition="input.conditionedPanels==3",
                       radioButtons("economfact3", label = h3("Economic Factor"),
                                    choices= list("Inflation" = "inf", "Unemployment" = "unemp"#,"GDP" = "gdp"
                                      ), selected="inf"),
                       br(),
                       
                       helpText(h4("The Neural Network is based on the full dataset.",
                                   "You can select different Economic Factors.")),
                       
                       br(),
                       helpText("Note 1: While the Average Annual Log Returns",
                                "and Correlations tabs will show the specified inputs, the Neural Network and table still be based",
                                "on the full dataset. \n",
                                "Note 2. The model for GDP is under construction."),
                       
                       actionButton("go3", "Update")
      ),
      
      conditionalPanel(condition="input.conditionedPanels==4",
                       helpText(h4("The Table display the full dataset.")),
                       br(),
                       helpText("Note 1: While the Average Annual Log Returns",
                                "and Correlations tabs will show the specified inputs, the Neural Network and table still be based",
                                "on the full dataset. \n",
                                "Note 2. The model for GDP is under construction.")
      ),
      conditionalPanel(condition="input.conditionedPanels==6",
                       helpText(h4("The map shows the location, plant name, energy source and total capacity (MW)", 
                                   "for the Power Plants located in the U.S.")),
                      helpText("In the bottom right of the map you can select between world-space modifier(WSM), Dark or Satellite view."),
                      helpText("The data was obtained from", 
                               "The U.S. Energy Information Administration. Further work will include", 
                               "interactive selection of type of Plants.")
      ),
      
      conditionalPanel(condition="input.conditionedPanels==5",
                       #radioButtons("StorageFacilityGas", label = h3("Storage Facilities Gas"),
                      #              choices= list("Aqua" = "aqua"#, "Unemployment" = "unemp"#,"GDP" = "gdp"
                       #             ), selected="inf"),
                       #br(),
                      helpText(h4("The map shows the location, type company and and total capacity (bcf)", 
                                  "for the Gas Storage Facilities in the U.S.")),
                       helpText("In the bottom right of the map you can select between world-space modifier(WSM), Dark or Satellite view."),
                       helpText("The data was obtained from", 
                                "Energy Analytics. Further work will include", 
                                "interactive selection of type of Storage types."),
                      helpText("Aquifer = Aquifer, DGF = Depleted Gas Field, SC = Salt Cavern.")
      
      ),
      
      br(),
      conditionalPanel(condition="input.conditionedPanels==8",
                       selectInput("year1", label = h3("Year"),
                                   choices= list("2012" = "2012","2013" = "2013","2014" = "2014",
                                                 "2015"="2015"), 
                                    selected="2012"),
                       br(),
                       radioButtons("monthTemp", label = h3("Month"),inline = TRUE,
                                    choices= list("January" = "Jan", "February" = "Feb",
                                                  "March" = "Mar","April"="Apr","May"="May", "June"="Jun",
                                                  "July"="Jul","August"="Aug","September"="Sept","October"="Oct",
                                                  "November"="Nov", "December"="Dec"
                                    ), selected="Jan"),
                       br(),
                       
                       helpText("Note 1. The data was obtained from the Climatic Research Unit - University of East Anglia.")
                       
                            ),
      
      br()

      
      
      
      
      
      
      #submitButton("Update")

    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(id = "conditionedPanels", 
        tabPanel("Power Plants", leafletOutput("map3"), value=6),
        tabPanel("Gas Storage Facilities", leafletOutput("map2"), value=5),
        tabPanel("Technical Analysis", plotOutput("plotFin"), value=7),
        tabPanel("Global Temperature", plotOutput("plotTemp"), value=8),
        tabPanel("Correlations -Economic Factors", plotOutput("plot1"), value=1), 
        tabPanel("Average Annual Log Returns -Economic Factors", plotOutput("plot"), value=2),
        tabPanel("Neural Network -Economic Factors", plotOutput("nnetw"), value=3)#,
        #tabPanel("Table", DT::dataTableOutput('Table'), value=4)
      )#,style='width: 100px; height: 100px'
    )
  )
))
