library(shiny)
require(ggplot2)
require(reshape2)
require(quantmod)
#require(FredR)
#require(Quandl)
require(pipeR)
require(plyr)
require(dplyr)
#require(gridExtra)
require(ggthemes)
require(gtools)
require(corrplot)
#require(wordcloud)
require(RColorBrewer)
require(forecast)
require(leaflet)
#require(googleVis)
#options(shiny.transcode.json = FALSE)
require(foreign)
library(ggmap)
library(maptools)
library(maps)
library(lubridate)
library(lattice)
require(gtools)


powerplants <- read.dbf("PowerPlants_US_201511.dbf")

data1 <- read.csv(file="data1.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)
data2 <- read.csv(file="data2.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)
data3 <-read.csv(file="data3.csv",header=TRUE, sep=",",stringsAsFactors = FALSE)
dcordata <- read.csv(file="dcordata.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)
data2$date<-as.Date(data2$date)
data3$date<-as.Date(data3$date)
data31 <-read.csv(file="data31.csv",header=TRUE, sep=",",stringsAsFactors = FALSE)
data31$date<-as.Date(data31$date)
data32 <-read.csv(file="data32.csv",header=TRUE, sep=",",stringsAsFactors = FALSE)

data32$date<-as.Date(data32$date)

storage <- data.frame(read.csv("Storage.csv", na.strings = c("na")), stringsAsFactors = FALSE)
temp20112014<-read.csv(file="temp20112014.csv",header=TRUE, sep=",", stringsAsFactors = FALSE)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
    
  # Create a reactiveValues object, to let us use settable reactive values
  values <- reactiveValues()
  
  dataInput <- reactive({
      if (input$ticker=="USD/GBP" | input$ticker =="USD/EUR"){
        getSymbols(input$ticker, src="oanda",
                   auto.assign = FALSE)
        }
      else{  
     getSymbols(input$ticker,
                        auto.assign = FALSE)
      }
    })
  

  observeEvent(input$economfact, {
  values$economfact <- input$economfact
  })
  
  observeEvent(input$year1, {
    values$year1 <- input$year1
  })
  
  observeEvent(input$monthTemp, {
    values$monthTemp <- input$monthTemp
  })

  
  observeEvent(input$daterange, {
    values$startdate <- input$daterange[1]
    values$enddate <- input$daterange[2]
  })
  
  observeEvent(input$select, {
    values$select <- input$select
  })
 ###

  observeEvent(input$daterange2, {
    values$startdate2 <- input$daterange2[1]
    values$enddate2 <- input$daterange2[2]
  })
  
  observeEvent(input$select2, {
    values$select2 <- input$select2
  }) 
 ###
  observeEvent(input$economfact3, {
    values$economfact3 <- input$economfact3
  })
  ###
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  
 output$plotTemp <- renderPlot({
    tempPlot<-paste0("tmp",values$monthTemp,values$year1)
    ggplot(data=temp20112014, aes_string(x="lon", y="lat")) + geom_tile(aes_string(fill=tempPlot)) + 
     scale_fill_distiller("Temperature in C", palette = "RdBu",breaks=seq(-40,50,by=10))+
     theme(axis.title.x=element_blank(),
           axis.title.y=element_blank(),
           legend.position = 'bottom',
           legend.key.size = unit(1, "cm")
   ) +
     ggtitle(paste0("Global Temperature in ",values$monthTemp, ", ", values$year1)) 
     })
  
  output$plotFin <- renderPlot({
    chartSeries(dataInput(), name=input$ticker, TA="addBBands();addCCI();addRSI()")
  })
  
  # Plot the Storage map
  output$map2 <- renderLeaflet({
    content <- paste0("<strong>Company: </strong>",
                      storage$Company,
                      "<br><strong>Field: </strong>",
                      storage$Field,
                      "<br><strong>Total Capacity (BCF): </strong>",
                      storage$Total)
    pal <- colorFactor(c("navy", "red", "green"), domain = c("DGF", "SC", "Aquifer"))
    
      leaflet(storage) %>% 
        addProviderTiles("Esri.WorldStreetMap", group = "WSM") %>%
        addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addCircles(radius = ~Total/100, color = ~pal(Type), popup = ~content) %>%
        addLegend("topright", pal = pal, values = ~Type, title = "Storage Type") %>%
        addLayersControl(baseGroups = c("WSM", "Dark", "Satellite"), 
                         position = "bottomright", 
                         options = layersControlOptions(collapsed = TRUE))
      })
  
  # Plot the Power Plants map
  output$map3 <- renderLeaflet({
    content <- paste0("<strong>Plant Name: </strong>",
                      powerplants$Plant_Name,
                      "<br><strong>Description: </strong>",
                      powerplants$source_des,
                      "<br><strong>Technology: </strong>",
                      powerplants$tech_desc,
                      "<br><strong>Total (MW): </strong>",
                      powerplants$Total_MW)
    pal <- colorFactor(c("cyan","grey","navy", "red", "green","black","yellow","pink","orange","coral2","bisque"), domain = c("biomass", "coal", "geothermal","hydroelectric","natural gas","nuclear","other","petroleum","solar","wind","pumped storage"))
    
    leaflet(powerplants) %>% 
      addProviderTiles("Esri.WorldStreetMap", group = "WSM") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addCircles(radius = ~Total_MW, color = ~pal(PrimSource), popup = ~content) %>%
      addLegend("topright", pal = pal, values = ~PrimSource, title = "Type") %>%
      addLayersControl(baseGroups = c("WSM", "Dark", "Satellite"), 
                       position = "bottomright", 
                       options = layersControlOptions(collapsed = TRUE))
  })
  
    
  output$plot <- renderPlot({
    input$go2
    isolate(
    if(values$select2 == 3){
      my.plot1.3<- ggplot(data=data2, aes(x=date,group=1)) + 
        geom_line(aes(y=Lindprod,color="GDP",group=1)) + 
        geom_line(aes(y=Lm2,color="M2",group=1)) +
        geom_line(aes(y=Ltb3m,color="T-Bills 3M",group=1)) + 
        geom_line(aes(y=Ltb6m,color="T-Bills 6M",group=1)) + 
        geom_line(aes(y=Lunemployment,color="Unemployment",group=1)) + 
        geom_line(aes(y=Lreserves,color="Reserves",group=1)) + 
        theme_economist()+scale_colour_economist() + 
        theme(legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(0.5,.235),plot.title = element_text(size = 13))+ # modifying background color
        scale_color_manual(values=c("GDP"="forestgreen","M2"="darkorange",
                                    "Unemployment"="brown1", "T-Bills 3M"="blue4", "T-Bills 6M"="darkred", "Reserves"="grey0"))+
        xlab("Time") + ylab("Average Log Return") + 
        scale_x_date(limits = c(values$startdate2,values$enddate2)) + ggtitle("Average Annual Log Return of Selected Economic Factors") # Adding titles
      
      my.plot1.3
      
    } else if(values$select2 == 1){
      my.plot1.1<- ggplot(data=data2, aes(x=date)) + 
        geom_line(aes(y=Linflation,color="Inflation",group=1)) + # Adding a colored line
        geom_line(aes(y=Lavehourearn,color="Average Earnings",group=1)) +
        geom_line(aes(y=Lwages,color="Wages",group=1)) +
        geom_line(aes(y=Lhouseunits,color="House Units",group=1)) +
        geom_line(aes(y=Ltresu10y,color="Treasury 10y",group=1)) +
        geom_line(aes(y=Lmbase,color="M Base",group=1)) +
        theme_economist()+scale_colour_economist() + 
        theme(legend.background = element_rect(fill=alpha(0.0001, 0.0001)),legend.text=element_text(size=10),legend.title=element_blank(),
              legend.direction="horizontal",legend.position=c(.55,.155),
              plot.title = element_text(size = 13))+ # modifying background color
        scale_color_manual(values=c("Inflation"="deepskyblue2", "Average Earnings"="forestgreen","Wages"="gray60","House Units"="gray0",
                                    "Treasury 10y"="darkred","M Base"="purple3"))+
        xlab("Time") + ylab("Average Log Return") + 
        scale_x_date(limits = c(values$startdate2,values$enddate2)) + ggtitle("Average Annual Log Return of Selected Economic Factors") # Adding titles
      
      my.plot1.1
      
    } else if(values$select2 == 2){
      my.plot1.2<- ggplot(data=data2, aes(x=date,group=1)) + 
        geom_line(aes(y=Lhousesalesprice,color="House Prices",group=1)) +
        geom_line(aes(y=Ldispincome,color="Disposable Income",group=1)) +
        geom_line(aes(y=Lloansbanks,color="Loans",group=1)) +
        geom_line(aes(y=LIntDiscUS,color="Discount Rate",group=1)) +
        geom_line(aes(y=Lhousesupply,color="House Supply",group=1)) +
        theme_economist()+scale_colour_economist() + 
        theme(legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),
              legend.direction="horizontal",legend.position=c(0.4,.21),
              plot.title = element_text(size = 13))+ # modifying background color
        scale_color_manual(values=c("House Prices"="deepskyblue2","Disposable Income"="forestgreen","Loans"="darkorange","Discount Rate"="darkred",
                                    "House Supply"="gray0"))+
        xlab("Time") + ylab("Average Log Return") + 
        scale_x_date(limits = c(values$startdate2,values$enddate2)) + ggtitle("Average Annual Log Return of Selected Economic Factors") # Adding titles
      
      my.plot1.2
      
    } else {
      my.plot1.4<- ggplot(data=data2, aes(x=date,group=1)) + 
        geom_line(aes(y=Lpersincome,color="Personal Income",group=1)) +
        geom_line(aes(y=Lpersconsind,color="Personal Consumption",group=1)) +
        geom_line(aes(y=Lpcomodind,color="Commodity index",group=1)) +
        geom_line(aes(y=Lm1,color="M1",group=1)) +
        geom_line(aes(y=Ldeposits,color="Deposits",group=1)) +
        theme_economist()+scale_colour_economist() + 
        theme(legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),
              legend.direction="horizontal",legend.position=c(.4,.69),plot.title = element_text(size = 13))+ # modifying background color
        scale_color_manual(values=c("Personal Income"="forestgreen","Personal Consumption"="darkorange","Commodity index"="grey0", "M1"="deepskyblue2", "Deposits"="darkred"))+
        xlab("Time") + ylab("Average Log Return") + 
        scale_x_date(limits = c(values$startdate2,values$enddate2)) + ggtitle("Average Annual Log Return of Selected Economic Factors") # Adding titles
      
      my.plot1.4
    } ### finish of the if for the choice of variables
    
    ) #from isolate
  })
  
  
  output$plot1 <- renderPlot({
    input$go
    isolate(
    if (values$economfact=='inf') {
    if(values$select == 3){
      my.plot2.3<- ggplot(data=data3, aes(x=date,group=1)) + 
        geom_line(aes(y=corun.inf,color="Unemployment vs Inf.",group=1)) +
        geom_line(aes(y=corm2.inf,color="M2 vs Inf.",group=1)) +
        geom_line(aes(y=corGDP.inf,color="GDP vs Inf.",group=1)) +
        geom_line(aes(y=cortb3m.inf,color="T-bills 3M vs Inf.",group=1)) +
        #geom_line(aes(y=cortb6m.inf,color="T-bills 6M vs Inf.",group=1)) +
        geom_line(aes(y=correserves.inf,color="Reserves vs Inf.",group=1)) +
        theme_economist()+scale_colour_economist() + 
        theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.box = "horizontal",legend.direction="horizontal",legend.position=c(.7,0.85),plot.title = element_text(size = 13))+ # modifying background color
        scale_color_manual(values=c("Unemployment vs Inf."="forestgreen","M2 vs Inf."="darkorange","GDP vs Inf."="grey0", "T-bills 3M vs Inf."="deepskyblue2", "T-bills 6M vs Inf."="darkred", "Reserves vs Inf."="darkred"))+
        ylab("Correlation") + 
        scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation Inflation vs Economic Factors (3-year smooth)") # Adding titles
      
      my.plot2.3
    } else if(values$select == 1){
      my.plot2.1<- ggplot(data=data3, aes(x=date,group=1)) + 
        geom_line(aes(y=coraveearn.inf,color="Average Income vs Inf.",group=1)) +
        geom_line(aes(y=corhouseuni.inf,color="House Unites vs Inf.",group=1)) +
        geom_line(aes(y=cormbase.inf,color="M-Base vs Inf.",group=1)) +
        geom_line(aes(y=cortres10y.inf,color="Treasury 10y vs Inf.",group=1)) +
        geom_line(aes(y=corwages.inf,color="Wages vs Inf.",group=1)) +
        theme_economist()+scale_colour_economist() + 
        theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(.5,.18),plot.title = element_text(size = 13))+ # modifying background color
        scale_color_manual(values=c("Average Income vs Inf."="forestgreen","House Unites vs Inf."="darkorange","M-Base vs Inf."="grey0", "Treasury 10y vs Inf."="deepskyblue2", "Wages vs Inf."="darkred"))+
        ylab("Correlation") + 
        scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation Inflation vs Economic Factors (3-year smooth)") # Adding titles
      
      my.plot2.1
    } else if(values$select == 2){
      my.plot2.2<- ggplot(data=data3, aes(x=date,group=1)) + 
        geom_line(aes(y=cordisc.inf,color="Discount vs Inf.",group=1)) +
        geom_line(aes(y=cordispinc.inf,color="Disposable Inc. vs Inf.",group=1)) +
        geom_line(aes(y=corhouseprice.inf,color="House Prices vs Inf.",group=1)) +
        geom_line(aes(y=corhousesupp.inf,color="Houses vs Inf.",group=1)) +
        geom_line(aes(y=corloans.inf,color="Loans vs Inf.",group=1)) +
        theme_economist()+scale_colour_economist() + 
        theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(.45,.14),plot.title = element_text(size = 13))+ # modifying background color
        scale_color_manual(values=c("Discount vs Inf."="forestgreen","Disposable Inc. vs Inf."="darkorange","House Prices vs Inf."="grey0", "Houses vs Inf."="deepskyblue2", "Loans vs Inf."="darkred"))+
        ylab("Correlation") + 
        scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation Inflation vs Economic Factors (3-year smooth)") # Adding titles
      
      my.plot2.2
    } else {
      my.plot2.4<- ggplot(data=data3, aes(x=date,group=1)) + 
        geom_line(aes(y=corcomind.inf,color="Commodities vs Inf.",group=1)) +
        geom_line(aes(y=cordeposits.inf,color="Deposits vs Inf.",group=1)) +
        geom_line(aes(y=corm1.inf,color="M1 vs Inf.",group=1)) +
        geom_line(aes(y=corperscons.inf,color="Personal Cons. vs Inf.",group=1)) +
        geom_line(aes(y=corpersinc.inf,color="Personal Inc. vs Inf.",group=1)) +
        theme_economist()+scale_colour_economist() + 
        theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(.45,.14),plot.title = element_text(size = 13))+ # modifying background color
        scale_color_manual(values=c("Commodities vs Inf."="forestgreen","Deposits vs Inf."="darkorange","M1 vs Inf."="grey0", "Personal Cons. vs Inf."="deepskyblue2", "Personal Inc. vs Inf."="darkred"))+
        ylab("Correlation") + 
        scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation Inflation vs Economic Factors (3-year smooth)") # Adding titles
      
      my.plot2.4
    } #end for the plot ifs
    } else if (values$economfact=='unemp'){
 #start of ifs unemp
      if(values$select == 3){
        my.plot3.3<- ggplot(data=data31, aes(x=date,group=1)) + 
          geom_line(aes(y=corm2.unem,color="M2 vs Unemp.",group=1)) +
          geom_line(aes(y=corGDP.unem,color="GDP vs Unemp.",group=1)) +
          geom_line(aes(y=cortb3m.unem,color="T-bills 3M vs Unemp.",group=1)) +
          #geom_line(aes(y=cortb6m.unem,color="T-bills 6M vs Unemp.",group=1)) +
          geom_line(aes(y=correserves.unem,color="Reserves vs Unemp.",group=1)) +
          theme_economist()+scale_colour_economist() + 
          theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.box = "horizontal",legend.direction="horizontal",legend.position=c(.5,0.85),plot.title = element_text(size = 13))+ # modifying background color
          scale_color_manual(values=c("M2 vs Unemp."="darkorange","GDP vs Unemp."="grey0", "T-bills 3M vs Unemp."="deepskyblue2", "T-bills 6M vs Unemp."="darkred", "Reserves vs Unemp."="darkred"))+
          ylab("Correlation") + 
          scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation Unemployment vs Economic Factors (3-year smooth)") # Adding titles
        
        my.plot3.3
        
      } else if(values$select == 1){
        my.plot3.1<- ggplot(data=data31, aes(x=date,group=1)) + 
          geom_line(aes(y=corun.unem,color="Inflation vs Unemp.",group=1)) +
          geom_line(aes(y=coraveearn.unem,color="Average Income vs Unemp.",group=1)) +
          geom_line(aes(y=corhouseuni.unem,color="House Unites vs Unemp.",group=1)) +
          geom_line(aes(y=cormbase.unem,color="M-Base vs Unemp.",group=1)) +
          geom_line(aes(y=cortres10y.unem,color="Treasury 10y vs Unemp.",group=1)) +
          geom_line(aes(y=corwages.unem,color="Wages vs Unemp.",group=1)) +
          theme_economist()+scale_colour_economist() + 
          theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(.5,.85),plot.title = element_text(size = 13))+ # modifying background color
          scale_color_manual(values=c("Inflation vs Unemp."="pink","Average Income vs Unemp."="forestgreen","House Unites vs Unemp."="darkorange","M-Base vs Unemp."="grey0", "Treasury 10y vs Unemp."="deepskyblue2", "Wages vs Unemp."="darkred"))+
          ylab("Correlation") + 
          scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation Unemployment vs Economic Factors (3-year smooth)") # Adding titles
        
        my.plot3.1
        
      } else if(values$select == 2){
        my.plot3.2<- ggplot(data=data31, aes(x=date,group=1)) + 
          geom_line(aes(y=cordisc.unem,color="Discount vs Unemp.",group=1)) +
          geom_line(aes(y=cordispinc.unem,color="Disposable Inc. vs Unemp.",group=1)) +
          geom_line(aes(y=corhouseprice.unem,color="House Prices vs Unemp.",group=1)) +
          geom_line(aes(y=corhousesupp.unem,color="Houses vs Unemp.",group=1)) +
          geom_line(aes(y=corloans.unem,color="Loans vs Unemp.",group=1)) +
          theme_economist()+scale_colour_economist() + 
          theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(.45,.85),plot.title = element_text(size = 13))+ # modifying background color
          scale_color_manual(values=c("Discount vs Unemp."="forestgreen","Disposable Inc. vs Unemp."="darkorange","House Prices vs Unemp."="grey0", "Houses vs Unemp."="deepskyblue2", "Loans vs Unemp."="darkred"))+
          ylab("Correlation") + 
          scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation Unemployment vs Economic Factors (3-year smooth)") # Adding titles
        
        my.plot3.2
        
      } else {
        my.plot3.4<- ggplot(data=data31, aes(x=date,group=1)) + 
          geom_line(aes(y=corcomind.unem,color="Commodities vs Unemp.",group=1)) +
          geom_line(aes(y=cordeposits.unem,color="Deposits vs Unemp.",group=1)) +
          geom_line(aes(y=corm1.unem,color="M1 vs Unemp.",group=1)) +
          geom_line(aes(y=corperscons.unem,color="Personal Cons. vs Unemp.",group=1)) +
          geom_line(aes(y=corpersinc.unem,color="Personal Inc. vs Unemp.",group=1)) +
          theme_economist()+scale_colour_economist() + 
          theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(.45,.85),plot.title = element_text(size = 13))+ # modifying background color
          scale_color_manual(values=c("Commodities vs Unemp."="forestgreen","Deposits vs Unemp."="darkorange","M1 vs Unemp."="grey0", "Personal Cons. vs Unemp."="deepskyblue2", "Personal Inc. vs Unemp."="darkred"))+
          ylab("Correlation") + 
          scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation Unemployment vs Economic Factors (3-year smooth)") # Adding titles
        
        my.plot3.4
        
      } #end for the plot ifs unemp
      
    } else{
      #start of ifs unemp
      if(values$select == 3){
        my.plot4.3<- ggplot(data=data32, aes(x=date,group=1)) + 
          geom_line(aes(y=corun.gdp,color="Unemployment vs GDP",group=1)) +
          geom_line(aes(y=corm2.gdp,color="M2 vs GDP",group=1)) +
          geom_line(aes(y=cortb3m.gdp,color="T-bills 3M vs GDP",group=1)) +
          #geom_line(aes(y=cortb6m.gdp,color="T-bills 6M vs GDP",group=1)) +
          geom_line(aes(y=correserves.gdp,color="Reserves vs GDP",group=1)) +
          theme_economist()+scale_colour_economist() + 
          theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.box = "horizontal",legend.direction="horizontal",legend.position=c(.5,0.85),plot.title = element_text(size = 13))+ # modifying background color
          scale_color_manual(values=c("M2 vs GDP"="darkorange","Unemployment vs GDP"="grey0", "T-bills 3M vs GDP"="deepskyblue2", "T-bills 6M vs GDP"="darkred", "Reserves vs GDP"="darkred"))+
          ylab("Correlation") + 
          scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation GDP vs Economic Factors (3-year smooth)") # Adding titles
        
        my.plot4.3
        
      } else if(values$select == 1){
        my.plot4.1<- ggplot(data=data32, aes(x=date,group=1)) + 
          geom_line(aes(y=corInf.gdp,color="GDP vs Inf.",group=1)) +
          geom_line(aes(y=coraveearn.gdp,color="Average Income vs GDP",group=1)) +
          geom_line(aes(y=corhouseuni.gdp,color="House Unites vs GDP",group=1)) +
          geom_line(aes(y=cormbase.gdp,color="M-Base vs GDP",group=1)) +
          geom_line(aes(y=cortres10y.gdp,color="Treasury 10y vs GDP",group=1)) +
          geom_line(aes(y=corwages.gdp,color="Wages vs GDP",group=1)) +
          theme_economist()+scale_colour_economist() + 
          theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(.5,.18),plot.title = element_text(size = 13))+ # modifying background color
          scale_color_manual(values=c("GDP vs Inf."="pink","Average Income vs GDP"="forestgreen","House Unites vs GDP"="darkorange","M-Base vs GDP"="grey0", "Treasury 10y vs GDP"="deepskyblue2", "Wages vs GDP"="darkred"))+
          ylab("Correlation") + 
          scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation GDP vs Economic Factors (3-year smooth)") # Adding titles
        
        my.plot4.1
        
      } else if(values$select == 2){
        
        my.plot4.2<- ggplot(data=data32, aes(x=date,group=1)) + 
          geom_line(aes(y=cordisc.gdp,color="Discount vs GDP",group=1)) +
          geom_line(aes(y=cordispinc.gdp,color="Disposable Inc. vs GDP",group=1)) +
          geom_line(aes(y=corhouseprice.gdp,color="House Prices vs GDP",group=1)) +
          geom_line(aes(y=corhousesupp.gdp,color="Houses vs GDP",group=1)) +
          geom_line(aes(y=corloans.gdp,color="Loans vs GDP",group=1)) +
          theme_economist()+scale_colour_economist() + 
          theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(.45,.14),plot.title = element_text(size = 13))+ # modifying background color
          scale_color_manual(values=c("Discount vs GDP"="forestgreen","Disposable Inc. vs GDP"="darkorange","House Prices vs GDP"="grey0", "Houses vs GDP"="deepskyblue2", "Loans vs GDP"="darkred"))+
          ylab("Correlation") + 
          scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation GDP vs Economic Factors (3-year smooth)") # Adding titles
        
        my.plot4.2
        
      } else {
        my.plot4.4<- ggplot(data=data32, aes(x=date,group=1)) + 
          geom_line(aes(y=corcomind.gdp,color="Commodities vs GDP",group=1)) +
          geom_line(aes(y=cordeposits.gdp,color="Deposits vs GDP",group=1)) +
          geom_line(aes(y=corm1.gdp,color="M1 vs GDP",group=1)) +
          geom_line(aes(y=corperscons.gdp,color="Personal Cons. vs GDP",group=1)) +
          geom_line(aes(y=corpersinc.gdp,color="Personal Inc. vs GDP",group=1)) +
          theme_economist()+scale_colour_economist() + 
          theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(.45,.14),plot.title = element_text(size = 13))+ # modifying background color
          scale_color_manual(values=c("Commodities vs GDP"="forestgreen","Deposits vs GDP"="darkorange","M1 vs GDP"="grey0", "Personal Cons. vs GDP"="deepskyblue2", "Personal Inc. vs GDP"="darkred"))+
          ylab("Correlation") + 
          scale_x_date(limits = c(values$startdate,values$enddate)) + ggtitle("Correlation GDP vs Economic Factors (3-year smooth)") # Adding titles
        
        my.plot4.4
        
      } #end for the plot ifs unemp
      
    }
    ) #from isolate
  })
  
  #####
  
  
  
  ######
  output$nnetw <- renderPlot({
    require(neuralnet)
    input$go3
    isolate(
    if (values$economfact3 =='inf') {
    data1<-data1[,-1]
    
    #First we need to check that no datapoint is missing, otherwise we need 
    #to fix the dataset.
    
    
    apply(data1,2,function(x) sum(is.na(x)))
    
    #We proceed by randomly splitting the data into a train and a test set, 
    #then we fit a linear regression model and test it on the test set
    
    idx <- seq(1,round(0.90*nrow(data1)))
    train <- data1[idx,]
    test <- data1[-idx,]
    #lm.fit <- glm(Linflation~., data=train)
    #summary(lm.fit)
    #prlm <- predict(lm.fit,test)
    #MSElm <- sum((prlm - test$Linflation)^2)/nrow(test)
    
    LinfArima<-ts(data1$Linflation,start=c(1964,2), end=c(2016,2),frequency=12)
    LinfArima<-LinfArima[-564:-625]
    fitArima <- auto.arima(LinfArima,stationary = TRUE)
    f.fitArima<-forecast(fitArima,h=63)
    
    #Preparing to fit the neural network
    mx <- apply(data1, 2, max) 
    mini <- apply(data1, 2, min)
    
    scaled <- as.data.frame(scale(data1, center = mini, scale = mx - mini))
    
    train_ <- scaled[idx,]
    test_ <- scaled[-idx,]
    
    n <- names(train_)
    formul <- as.formula(paste("Linflation~", paste(n[!n %in% "Linflation"], collapse = " + ")))
    nn <- neuralnet(formul,data=train_,hidden=c(2,7),linear.output=TRUE)
    #plot(nn)
    
    prNN <- compute(nn,test_[,c("Lm2","Lunemployment","Ltb3m",
                                "Ltb6m","Lreserves","Lpersincome","Lindprod","Lpersconsind","Lpcomodind",
                                "Lm1","Ldeposits","Lavehourearn","Lwages","Lhouseunits","Ltresu10y","Lmbase",
                                "Lhousesalesprice","Ldispincome","Lloansbanks","LIntDiscUS","Lhousesupply")])
    
    prNN_ <- prNN$net.result*(max(data1$Linflation)-min(data1$Linflation))+min(data1$Linflation)
    test.r <- (test_$Linflation)*(max(data1$Linflation)-min(data1$Linflation))+min(data1$Linflation)
    
    MSE.nn <- sum((test.r - prNN_)^2)/nrow(test_)
    
    
    
    
    
    prNNTrain <- compute(nn,train[,c("Lm2","Lunemployment","Ltb3m",
                                     "Ltb6m","Lreserves","Lpersincome","Lindprod","Lpersconsind","Lpcomodind",
                                     "Lm1","Ldeposits","Lavehourearn","Lwages","Lhouseunits","Ltresu10y","Lmbase",
                                     "Lhousesalesprice","Ldispincome","Lloansbanks","LIntDiscUS","Lhousesupply")])
    prNN_Train <- prNNTrain$net.result*(max(data1$Linflation)-min(data1$Linflation))+min(data1$Linflation)
    
    train$Arimaf<-as.numeric(f.fitArima$fitted)
    train$nn<-prNN_Train
    train$nnsd<-prNN_Train+1*sd(prNN_Train)
    train$nn.sd<-prNN_Train-1*sd(prNN_Train)
    
    test$Arimaf<-as.numeric(f.fitArima$mean)
    test$nn<-prNN_
    test$nnsd<-prNN_+1*sd(prNN_)
    test$nn.sd<-prNN_-1*sd(prNN_)
    
    
    NN.<-as.data.frame(data2)
    #NN.<-NN.[-1,]
    NN1.<-rbind(train,test)
    NN1.<-NN1.[-1,]
    NN.<-cbind(NN.,NN1.)
    NN.f<-NN.[-1:-562,]
    NN.f$ArimaL80<-f.fitArima$lower[,1]
    NN.f$ArimaU80<-f.fitArima$upper[,1]
    
    plotNN.f<- ggplot(data=NN.f, aes(x=date)) + 
      geom_ribbon(aes(ymin=nn.sd, ymax=nnsd, fill="lightblue"))+
      geom_ribbon(aes(ymin=ArimaL80, ymax=ArimaU80, fill="lightblue3"),alpha=0.5)+
      geom_line(aes(y=Linflation,color="Inflation"),size=1.4) + # Adding a colored line
      geom_line(aes(y=nn,color="Neural Network"),size=1.01) + 
      geom_line(aes(y=Arimaf,color="Arima"),size=1.01)+
      theme_economist()+scale_colour_economist() + 
      theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(0.18,.85),plot.title = element_text(size = 13))+ # modifying background color
      scale_fill_identity(name = element_blank(), guide = 'legend',labels = c('Std NN',"80% Arima")) +
      scale_color_manual(values=c("Std NN"="lightblue","80% Arima"="lightblue3","Inflation"="blue4", "Neural Network"="darkorange","Arima"="darkred"))+
      ylab("Inflation Growth Rate") + ggtitle("Predicting the Inflation of US: Neural Networks (21:2:7) vs ARIMA (4,0,2)") # Adding titles
    
  plotNN.f
    } else {
      
      
      data4<-data2[,-1:-2]
      data4<-data4[,-23:-44]
      
      
      
      #First we need to check that no datapoint is missing, otherwise we need 
      #to fix the dataset.
      
      
      apply(data4,2,function(x) sum(is.na(x)))
      
      #We proceed by randomly splitting the data into a train and a test set, 
      #then we fit a linear regression model and test it on the test set
      
      idxunemp <- seq(1,round(0.75*nrow(data4)))
      trainunemp <- data4[idxunemp,]
      testunemp <- data4[-idxunemp,]
      #lm.fit <- glm(Linflation~., data=train)
      #summary(lm.fit)
      #prlm <- predict(lm.fit,test)
      #MSElm <- sum((prlm - test$Linflation)^2)/nrow(test)
      
      LunempArima<-ts(data4$unemployment,start=c(1964,2), end=c(2016,2),frequency=12)
      LunempArima<-LunempArima[-(round(0.75*nrow(data4))+1):-625]
      fitArimaunemp <- auto.arima(LunempArima,stationary = TRUE)
      f.fitArimaunemp<-forecast(fitArimaunemp,h=156)
      
      #Preparing to fit the neural network
      mxunemp <- apply(data4, 2, max) 
      miniunemp <- apply(data4, 2, min)
      
      scaledunemp <- as.data.frame(scale(data4, center = miniunemp, scale = mxunemp - miniunemp))
      
      train_unemp <- scaledunemp[idxunemp,]
      test_unemp <- scaledunemp[-idxunemp,]
      
      nunemp <- names(train_unemp)
      formulunemp <- as.formula(paste("unemployment~", paste(nunemp[!nunemp %in% "unemployment"], collapse = " + ")))
      nnunemp<-neuralnet(formulunemp,data=train_unemp,hidden=c(7,8),linear.output=TRUE)
      #nnunemp<-neuralnet(formulunemp,data=train_unemp,hidden=c(2,7),linear.output=FALSE)
      
      #nnunemp <- neuralnet(formulunemp,data=train_unemp,hidden=c(3,5),linear.output=FALSE)
      #plot(nn)
      
      prNNunemp <- compute(nnunemp,test_unemp[,c("inflation","m2","tb3m",
                                                 "tb6m","reserves","persincome","indprod","persconsind","pcomodind",
                                                 "m1","deposits","avehourearn","wages","houseunits","tresu10y","mbase",
                                                 "housesalesprice","dispincome","loansbanks","IntDiscUS","housesupply")])
      
      prNN_unemp <- prNNunemp$net.result*(max(data4$unemployment)-min(data4$unemployment))+min(data4$unemployment)
      test.runemp <- (test_unemp$unemployment)*(max(data4$unemployment)-min(data4$unemployment))+min(data4$unemployment)
      
      MSE.nnunemp <- sum((test.runemp - prNN_unemp)^2)/nrow(test_unemp)
      
      
      
      
      
      prNNTrainunemp <- compute(nnunemp,trainunemp[,c("m2","indprod","tb3m",
                                                      "tb6m","reserves","persincome","inflation","persconsind","pcomodind",
                                                      "m1","deposits","avehourearn","wages","houseunits","tresu10y","mbase",
                                                      "housesalesprice","dispincome","loansbanks","IntDiscUS","housesupply")])
      
      prNN_Trainunemp <- prNNTrainunemp$net.result *(max(data4$unemployment)-min(data4$unemployment))+min(data4$unemployment)
      
      
      trainunemp$Arimafunemp<-as.numeric(f.fitArimaunemp$fitted)
      trainunemp$nnunemp<-prNN_Trainunemp
      trainunemp$nnsdunemp<-prNN_Trainunemp+1*sd(prNN_Trainunemp)
      trainunemp$nn.sdunemp<-prNN_Trainunemp-1*sd(prNN_Trainunemp)
      
      testunemp$Arimafunemp<-as.numeric(f.fitArimaunemp$mean)
      testunemp$nnunemp<-prNN_unemp
      testunemp$nnsdunemp<-prNN_unemp+1*sd(prNN_unemp)
      testunemp$nn.sdunemp<-prNN_unemp-1*sd(prNN_unemp)
      
      
      NN.unemp<-as.data.frame(data2)
      #NN.<-NN.[-1,]
      NN1.unemp<-rbind(trainunemp,testunemp)
      #NN1.unemp<-NN1.unemp[-1,]
      NN.unemp<-cbind(NN.unemp,NN1.unemp)
      NN.funemp<-NN.unemp[-1:-469,]
      NN.funemp$ArimaL80unemp<-f.fitArimaunemp$lower[,1]
      NN.funemp$ArimaU80unemp<-f.fitArimaunemp$upper[,1]
      
      plotNN.funemp<- ggplot(data=NN.funemp, aes(x=date)) + 
        geom_ribbon(aes(ymin=nn.sdunemp, ymax=nnsdunemp, fill="lightblue"))+
        geom_ribbon(aes(ymin=ArimaL80unemp, ymax=ArimaU80unemp, fill="lightblue3"),alpha=0.5)+
        geom_line(aes(y=unemployment,color="Unemployment"),size=1.4) + # Adding a colored line
        geom_line(aes(y=nnunemp,color="Neural Network"),size=1.01) + 
        geom_line(aes(y=Arimafunemp,color="Arima"),size=1.01)+
        theme_economist()+scale_colour_economist() + 
        theme(axis.title.x = element_blank(),legend.background = element_rect(fill=alpha(0.01, 0.01)),legend.text=element_text(size=10),legend.title=element_blank(),legend.direction="horizontal",legend.position=c(0.18,.85),plot.title = element_text(size = 13))+ # modifying background color
        scale_fill_identity(name = element_blank(), guide = 'legend',labels = c('Std NN',"80% Arima")) +
        scale_color_manual(values=c("Std NN"="lightblue","80% Arima"="lightblue3","Unemployment"="blue4", "Neural Network"="darkorange","Arima"="darkred"))+
        ylab("Unemployment Growth Rate") + ggtitle("Predicting the Unemployment of US: Neural Networks (21:7:8) vs ARIMA (2,0,2)") # Adding titles
      
      plotNN.funemp
    }
    )#end isolate
  })  
  #######
  #####
  # Generate a Java table view of the data
  output$Table <- DT::renderDataTable({
    datatable(data2, options = list(initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().body()).css({'background-color': 'black', 'color': 'black'});",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "var css = document.createElement('style');
      css.type = 'text/css';
      css.innerHTML = '.table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover { background-color: #9c4242 !important }';
      document.body.appendChild(css);",  
      "}"), searching = FALSE, lengthMenu = c(10, 25,50, 100), pageLength = 100))
  })
}) 



  






