library(shiny)
library(tidyverse)
library(leaflet)
library(rgdal)
library(plotly)
library(pkgnet)
library(stringr)
library(reshape2)
library(DT)
library(graphics)
library(scales)

#overview
ind_def <- read.csv("./data/industries_definition.csv", stringsAsFactors = FALSE) %>%
  select(-X)

#map and map metadata
world_spdf <- readOGR(dsn= "./data/world_map/", 
                      layer="TM_WORLD_BORDERS_SIMPL-0.3")
map_metadata <- read.csv("./data/map_metadata_new.csv")
world_spdf@data <- world_spdf@data %>% 
  select(m49code = UN, NAME, LON, LAT) 
world_spdf@data <- map_metadata %>%
  left_join(world_spdf@data, by = "m49code")

world_spdf$total_mig <- as.numeric(as.character(world_spdf$total_mig))
world_spdf$top_ordered <- as.character(world_spdf$top_ordered)
world_spdf$mig_perc <- as.numeric(as.character(world_spdf$mig_perc))

#second viz plot_ly
pop_mig_gender <- read.csv("./data/pop_mig_gender.csv", stringsAsFactors = FALSE)

#third viz industry_trend  
df_industry <- read.csv("./data/df_industry.csv", stringsAsFactors = FALSE) 
color_range <- c("#7496D2", '#E6A2C5', rep("#000000", 100))

#forth viz relationship

combine <- read.csv("./data/combine.csv", stringsAsFactors = FALSE)
growth <- read.csv("./data/growth.csv", stringsAsFactors = FALSE) 

function(input, output, session) {
  
  # overview, table of results, rendered using data table
  output$dataTable0 <- renderDataTable(datatable({
    dataSet<- ind_def
    colnames(dataSet) <- c("Measurement Variable", "Industries Included")
    dataSet},
    options = list(lengthMenu = c(10), columnDefs = list(list(className = 'dt-center', targets = "_all"), list(width = '330px', targets = 1))),
    caption = 'United Nation 2008 ISIC Industry Standard'))  
  
  
  #start of map visualization
  
  mapdatasubset <- reactive({
    world_spdf@data <- world_spdf@data[world_spdf$Year == input$Year,]
    world_spdf
  })
  
  # "#FFFFB2", "#FF0022"  yellow to red
  output$mig_map<-renderLeaflet({
    leaflet(data = world_spdf, options = leafletOptions(minZoom = 1, maxZoom = 4))
  })
  
  observe({
    theData <- mapdatasubset()
    #map parameters
    pal <- colorQuantile(c("#63A945","#F0C345","#AF3236"), theData$mig_perc, n = 10)
    # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
    leafletProxy("mig_map", data = theData) %>%
      clearShapes() %>%
      setView(lat=50, lng=20, zoom=1) %>%
      addPolygons(data = theData, stroke = TRUE, smoothFactor = 0.5,
                  weight=1, color='#94A1A9', opacity=1, 
                  # Add colors for life exp quantiles
                  fillColor = pal(theData$mig_perc), 
                  fillOpacity = 1,
                  # Add label 
                  label = ~NAME,
                  labelOptions = labelOptions(direction = 'auto'),
                  # Add hightlighting for polygons
                  highlightOptions = highlightOptions( 
                    color='#5E8654', weight = 1.5, 
                    bringToFront = TRUE, sendToBack = TRUE),
                  # Popup with more info 
                  popup = paste("Country: ", theData$NAME,"<br/>",
                                "Year: ", theData$Year, "<br/>",
                                "Total migrant stock: ",formatC(theData$total_mig,
                                                            big.mark = ',', format='d'),
                                "<br/>", "Top 3 migrant sources: ", 
                                theData$top_ordered, "<br/>", 
                                "Migrant stock as percentage of population: ",
                                round(theData$mig_perc, 1), "%", sep = ""))
    
  })
  
  # end of map visualisation
  
  #start of second viz -- plotly
  output$countrySelect<-renderUI({
    countryList<- sort(unique(pop_mig_gender$country_name))
    selectInput(inputId = "Country", label = "Choose a country", choices=countryList, selected="United States")
  })
  
  popmigsubset <- reactive({
    pop_mig_gender <- pop_mig_gender[pop_mig_gender$country_name == input$Country,]
    pop_mig_gender
  })
  
  output$plotly <- renderPlotly({
    theData1 <- popmigsubset()
    # build graph with ggplot syntax
    p <- plot_ly(data = theData1, x = ~Year, y = ~round(as.numeric(total_pop/1000000), 2), name = 'Total Pop', type = 'scatter', mode = 'lines',
                 line = list(color = '#000000', width = 4), text = 'million(s)') %>%
      add_trace(y = ~round(as.numeric(total_male_pop/1000000), 2), name = 'Male Pop', line = list(color = '#7496D2', width = 4)) %>%
      add_trace(y = ~round(as.numeric(total_female_pop/1000000), 2), name = 'Female Pop', line = list(color = '#E6A2C5', width = 4)) %>%
      add_trace(y = ~round(as.numeric(total_mig/1000000), 2), name = 'Total Mig', line = list(color = '#000000', width = 4, dash = 'dash')) %>%
      add_trace(y = ~round(as.numeric(total_male_mig/1000000), 2), name = 'Male Mig', line = list(color = '#7496D2', width = 4, dash = 'dash')) %>%
      add_trace(y = ~round(as.numeric(total_female_mig/1000000), 2), name = 'Female Mig', line = list(color = '#E6A2C5', width = 4, dash = 'dash')) %>%
      layout(title = "Population and Migrant Stock at country level",
             xaxis = list(title = "Year", type = 'category'),
             yaxis = list (title = "Count (in millions)", hoverformat = 'y, text')) %>%
      layout(hovermode = 'compare', annotations = list(yref='paper',xref="paper",y=0.54,x=1.20, font = list(size = 10),
                                                       text="*Click on variables\ninside the legend\nbox above to remove\nor add the variables\nonto the graph",
                                                       showarrow=F, align = "left"))
    p
  })
  
  #start of the third viz -- industry trend  
  # selection
  
  output$regionSelection0 <- renderUI({
    region_choice <- unique(df_industry$Region)
    selectInput(inputId = "region0", label = "Choose a region:", choices = region_choice, selected = "North America")
  })
  
  output$secondSelection0 <- renderUI({
    arealist <- unique(df_industry$Area[df_industry$Region == input$region0])
    arealist <- arealist[is.na(arealist) == FALSE]
    selectInput("area0", "Country/Area:", choices = arealist, selected = "* All Countries/Areas(Sum)")
  })
  
  output$industrySelection0 <- renderUI({
    industry_choice <- unique(df_industry$Industry)
    selectInput(inputId = "industry0", label = "Value added by departments:", choices = industry_choice, selected = "Total Value Added")
  })
  
  output$allindustrySelection0 <- renderUI({
    checkboxInput(inputId = "allindustry0", label = "All industries of the Country/Area", value = FALSE)
  })
  
  # get the dataset prepared
  getDataSet1 <-reactive({
    # Get a subset of data
    if(isTRUE(input$area0 == "* All Countries/Areas(Sum)")){
      if (isTRUE(input$allindustry0 == TRUE)){
        df_industry2 <- df_industry[df_industry$Region == input$region0, ]
      }else{
        df_industry1 <- df_industry[df_industry$Industry == input$industry0, ]
        df_industry2 <- df_industry1[df_industry1$Region == input$region0, ]
      }
    }else{
      if (isTRUE(input$allindustry0 == TRUE)){
        df_industry1 <- df_industry[df_industry$Region == input$region0, ]
        df_industry2 <- df_industry1[df_industry1$Area == input$area0, ]
      }else{
        df_industry1 <- df_industry[df_industry$Region == input$region0, ]
        df_industry1 <- df_industry1[df_industry1$Area %in% c(input$area0, "* All Countries/Areas(Average)"), ]
        df_industry2 <- df_industry1[df_industry1$Industry == input$industry0, ]
      }
    }
    data <- df_industry2[, c("Area", "Year", "Industry", "Value_Added")]
    colnames(data) <- c("Countries/Areas", "Year", "Industry", "Value Added (in Billions)")
    data$`Countries/Areas`[data$`Countries/Areas` == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
    data
  })
  
  
  # plot
  output$linePlot <- renderPlotly({
    data <- getDataSet1() 
    shiny::validate(
      need(input$area0, "Input a value!")
    )
    data <- data %>%
      tidyr::separate(Industry, c("Industry", "Industry Code"), "\\(")
    data1 <- data %>%
      filter(`Countries/Areas`  == "* All Countries/Areas(Sum)")
    gg0 <- ggplot() + {if (isTRUE(input$allindustry0 == TRUE) & (isTRUE(input$area0 == "* All Countries/Areas(Sum)"))) geom_line(data = data1, aes(x = Year, y = `Value Added (in Billions)`, color = Industry),alpha = 0.5) else if (isTRUE(input$allindustry0 == TRUE) & (isTRUE(input$area0 != "* All Countries/Areas(Sum)"))) geom_line(data = data, aes(x = Year, y = `Value Added (in Billions)`, color = Industry),alpha = 0.5) else geom_line(data = data, aes(x = Year, y = `Value Added (in Billions)`, color = `Countries/Areas`), alpha = 0.5) } + 
      {if (isTRUE(input$allindustry0 == FALSE)) scale_color_manual(values = as.vector(color_range))} + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), 
            axis.text.x = element_text(angle = 60, hjust = 1), axis.line = element_line(size=0.5, colour = "black"), legend.background = element_rect(), legend.title = element_blank()) +
      {if(isTRUE(input$area0 == "* All Countries/Areas(Sum)")) ggtitle(paste0("Industry Value-Added of All countries in ", input$region0, " : \n", input$industry0 )) else ggtitle(paste0("Industry Value-Added of ", input$area0, " : \n", input$industry0 ))} +
      scale_y_continuous(labels = comma)  + labs(x="",  y="Value Added (in Billions)")
    
    ggplotly(gg0) %>%
      layout(legend = list(orientation = "v",   # show entries horizontally
                           xanchor = "right",  # use center of legend as anchor
                           x = 1.8,
                           size = 1,
                           bgcolor = 'rgba(0,0,0,0)',
                           text=NULL
      ))  
    
  })
  
  # table of results, rendered using data table
  output$dataTable2 <- renderDataTable(datatable({
    dataSet<-getDataSet1()
    dataSet},
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, columnDefs = list(list(className = 'dt-center', targets = "_all"))),
    caption = 'Industry Valued Added Across Time'))  
  
  
  #start of the forth viz -- relationship   
  # options
  output$regionSelection1 <- renderUI({
    region_choice <- unique(df_industry$Region)
    selectInput(inputId = "region", label = "Choose a region:", choices = region_choice, selected = "North America")
  })
  
  output$secondSelection1 <- renderUI({
    arealist <- unique(combine$Area[combine$Region == input$region])[is.na(unique(combine$Population[combine$Region == input$region])) == FALSE]
    arealist1 <- arealist[is.na(arealist) == FALSE]
    selectInput("area", "Country/Area:", choices = arealist1, selected = "United States")
  })
  
  output$industrySelection1 <- renderUI({
    industry_choice <- unique(df_industry$Industry)
    selectInput(inputId = "industry", label = "Value added by departments:", choices = industry_choice, selected = "Total Value Added")
  })
  
  output$growthSelection <- renderUI({
    radioButtons(inputId = "growthornot", label = NA, 
                 choices = c("Country Absolute Value", "Country 5-year growth rate", "Region Absolute Value", "Region 5-year growth rate", "World-wide Absolute Value", "World-wide 5-year growth rate"), selected = "Country Absolute Value")
  })
  
  output$genderSelection <- renderUI({
    checkboxGroupInput(inputId = "gender", label = NA, choices = c("Female","Male","All"), selected = c("Female","Male"), inline = TRUE)
  })
  
  output$populationSelection <- renderUI({
    radioButtons(inputId = "population", label = NA, inline = TRUE,
                 choices = c("Total Population", "Migrant Population", "Migrant Stock as % of Population"), selected = "Migrant Population")
  })
  
  # outputs
  # get the dataset prepared for the graph
  getDataSet2 <- reactive({
    # Get a subset of data
    if (isTRUE(str_detect(input$growthornot, "rate"))){
      datahere <- growth
    }else{
      datahere <- combine
    }
    
    if (isTRUE(input$growthornot %in% c("Country Absolute Value", "Country 5-year growth rate"))){
      data13 <- datahere[datahere$Region == input$region, ]
      data13 <- data13[data13$Industry == input$industry, ]
      data13 <- data13[data13$Area == input$area, ]
      data13 <- data13[data13$Gender_type %in% input$gender, ]
      data13 <- data13[data13$Population_type == input$population, ]
    }else if (isTRUE(input$growthornot %in% c("Region Absolute Value", "Region 5-year growth rate"))){
      data13 <- datahere[datahere$Region == input$region, ]
      data13 <- data13[data13$Industry == input$industry, ]
      data13 <- data13[data13$Gender_type %in% input$gender, ]
      data13 <- data13[data13$Population_type == input$population, ]
    }else{
      data13 <- datahere[datahere$Gender_type %in% input$gender, ]
      data13 <- data13[data13$Industry == input$industry, ]
      data13 <- data13[data13$Population_type == input$population, ]
    }
    
    colnames(data13)[5] <- "Gender type"
    data13$Area[data13$Area == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
    data13
  })
  
  # outputs
  output$scatterPlot <- renderPlotly({
    data13 <- getDataSet2()
    gg2 <- ggplot(data13) + { if(isTRUE(str_detect(input$growthornot,"World-wide"))) geom_point(aes(x = Value_added, y = Population, color = Region, shape = `Gender type`), alpha = 0.5) else if (isTRUE(str_detect(input$growthornot,"Region"))) geom_point(aes(x = Value_added, y = Population, color = Area, shape =`Gender type`), alpha = 0.5) else geom_point(aes(x = Value_added, y = Population, color = `Gender type`), alpha = 0.5)} + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), axis.text.x = element_text(angle = 60, hjust = 1),axis.line = element_line(size=0.5, colour = "black"), legend.background = element_rect(), legend.title = element_blank()) + 
      { if(isTRUE(str_detect(input$growthornot,"World-wide"))) ggtitle(paste0("Relationship between ", input$population  , " and Value-Added of \n", input$industry, " : world")) else if (isTRUE(str_detect(input$growthornot,"Region"))) ggtitle(paste0("Relationship between ", input$population  , " and Value-Added of \n", input$industry, " : ", input$region)) else ggtitle(paste0("Relationship between ", input$population  , " and Value-Added of \n", input$industry, " : ", input$area))} +
      labs(x = paste0(input$industry, " (in Billions) ") , y = input$population) + coord_flip() + {if(isTRUE(str_detect(input$growthornot, "rate"))) scale_x_continuous(labels = percent) else scale_x_continuous(labels = comma)} + {if(isTRUE(str_detect(input$growthornot, "rate"))) scale_y_continuous(labels = percent) else scale_y_continuous(labels = comma)}  
    
    ggplotly(gg2) %>%
      layout(legend = list(bgcolor = 'rgba(0,0,0,0)',
                           text=NULL))
    
  })
  
  # table of results, rendered using data table
  output$dataTable3 <- renderDataTable(datatable({
    dataSet<-getDataSet2()
    dataSet <- dataSet %>%
      select(-X)
    dataSet$Value_added <- as.numeric(dataSet$Value_added)
    dataSet$Population <- as.numeric(dataSet$Population)
    dataSet$Value_added <- round(dataSet$Value_added, 4)
    dataSet$Population <- round(dataSet$Population, 4)
    if(isTRUE(str_detect(input$growthornot, "rate"))){
      colnames(dataSet) <- c("Country/Area", "Year", "Population type", "Gender type", "Population growth (%)", "Region", "Industry", "Value added growth (%)")
    }else{
      colnames(dataSet) <- c("Country/Area", "Year", "Population type", "Gender type", "Population", "Region", "Industry", "Value added (in Billions)")
    }
    
    
    dataSet},
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, columnDefs = list(list(className = 'dt-center', targets = "_all"))),
    caption = 'Population and Industry Valued Added Across Time'))
  
}