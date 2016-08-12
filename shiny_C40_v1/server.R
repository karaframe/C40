
# load packages
library(shiny)
library(dplyr)
library(leaflet)
require(readxl)
require(plyr)
library(plotly)
library(shinyjs)
library(V8)
library(pbapply)
library(shinydashboard)


# Clear all objects
rm(list = ls(all = TRUE))
# setwd("C:/C40_Rose/shiny_C40")
source("importWorksheets.R")

city <- c("Buenos Aires", "London", "New York", "Milano")
latitude <- c(-34.61, 51.5, 40.71, 45.46)
longitude <- c(-58.37, -0.12, -74.35, 9.18)
file <- c("FK_Buenos Aires_GPC_2015_Final_ORIGINAL.xlsm","FK_Buenos Aires_GPC_2015_Final_ORIGINAL.xlsm",
          "FK_Buenos Aires_GPC_2015_Final_ORIGINAL.xlsm", "FK_Buenos Aires_GPC_2015_Final_ORIGINAL.xlsm")
Map_data <- data.frame(city, longitude, latitude, file)

shinyServer(function(input, output) {
  
## Map------------------------------------------------------------------------------------------   
  finalMap <- reactive({
    # filter coordinates by date
    coord_city <- filter(Map_data, city == input$name)
    
    # define popup for the iteractive map
    popup_name <- paste0(
      "<strong><i>",
      input$name,
      "</i></strong>")
    
    map <- leaflet(coord_city) %>%
      addTiles() %>%
      setView(as.numeric(coord_city$longitude), as.numeric(coord_city$latitude), 10) %>%
      addProviderTiles("Thunderforest.Transport", group = "Thunderforest") %>%
      addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
      addProviderTiles("Hydda.Base", group = "Hydda_Base") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      
      addCircleMarkers(
        lng = ~ longitude, lat = ~ latitude,
        popup = popup_name,
        weight = 3, radius = 10,
        group = "city"
      ) %>%
    
    addLayersControl(
      # baseGroups = background,
      baseGroups = c("Hydda_Full", "Toner Lite","Thunderforest", "Hydda_Base", "Satellite"),
      overlayGroups = c("city"),
      options = layersControlOptions(collapsed = TRUE)
    )
    
  })

# Return to client
output$myMap = renderLeaflet(finalMap())



## read xslm file-----------------------------------------------------------------------------------------------

output$contents <- renderTable({  
  
  inFile <- input$file1   #.xlsm file to be loaded

  withProgress(message = "processing.....",  detail = 'this may take a while...', value = 0.25, { background= "blue"  
    # Number of times we'll go through the loop
    n <- 100
      
  # if (is.null(inFile) | input$goButton == 0)  # the start position of the button is 0 (OFF)
  #   return(NULL)
  # else if (input$goButton == 1)
    
    
    if (is.null(inFile) | input$goButton == 0)  # the start position of the button is 0 (OFF)
      return(NULL)
    
    # total <- 1
    # for(i in 1:total){
    # pb <- winProgressBar(title = "progress bar", min = 0,
    #                    max = total, width = 300)
    
  file <- inFile$datapath
  
  # read file with the impotWorsheets function
   ext <- tools::file_ext(inFile$name)
   file.rename(inFile$datapath,
               paste(inFile$datapath, ext, sep="."))
      GPC_sheet <-  importWorksheets(paste(inFile$datapath, ext, sep="."))   #Table B. Summary
      
      
      # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("gathering data....."))
      # Sys.sleep(0.1)
      # setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),
      #                                       "% done"))
      
      # Pause for 0.5 seconds to simulate a long computation.
        Sys.sleep(0.5)
      
      
  })
  
 # close(pb)
      
     # })

    })

## tables with sectors & subsectors  data---------------------------------------------


output$sectors <- renderTable({
  if(input$goButton == 0)
   return(NULL)
    read.csv("sectors.csv")
})



output$subsectors <- renderTable({
  if (input$goButton == 0)
     return(NULL)
    read.csv("subsectors.csv")
})


## ggplotly for sectors------------------------------------------------------

output$sectors_plot <- renderPlotly ({
  
   if(input$goButton == 1) {
    
  sectors <- read.csv("sectors.csv")[-1]
  sectors <- melt(sectors, id.var="Sectors")
  
  # The palette with grey:
  sectors_Palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  s <- ggplot(sectors, aes(x = variable, y = value, fill = Sectors)) + 
    scale_fill_manual(values=sectors_Palette) +
    theme_bw() + 	                # Change background theme to white with grey grids
    geom_bar(stat = "identity") +
    ggtitle("GHG Emissions Source (By Sector)") + 
    theme(legend.title=element_blank()) +
    theme(axis.text.x=element_text(size=12,face="bold", colour = "black"),
          axis.title.x = element_blank()) +
    theme(plot.title = element_text(lineheight=.8, face="bold")) +
    xlab("\n") + ylab("Emissions (mtCO2e)\n\n\n") +
    # ylab(expression(paste("Emissions", " ", "mt", CO[2], "e"))) +
    theme(axis.text.y=element_text(size=8,face="bold", colour = "black"),
          axis.title.y = element_text(face="bold", colour="black", size=8)) 
  # scale_y_continuous(labels = scientific_format()) 
  
  s <- ggplotly(s) }
  
  else ggplot()
    
})



## ggplotly for subsectors--------------------------------------------------------------

output$subsectors_plot <- renderPlotly ({
 
  if(input$goButton == 1) {
    
subsectors <- read.csv("subsectors.csv")[-1]
subsectors <- melt(subsectors, id.var="Sub_sectors")
# remove NA values
# subsectors <- subsectors[!(is.na(subsectors$value)), ]
Sub_sectors_Palette <- c("#999999", "#E69F00", "#56B4E9")

# keep xaxis in the same order as in the dataframe
subsectors$`Sub_sectors` <- factor(subsectors$`Sub_sectors`, levels=unique(subsectors$`Sub_sectors`))

ss <- ggplot(subsectors, aes(x = `Sub_sectors` , y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=Sub_sectors_Palette) +
  theme_bw() + 	                # Change background theme to white with grey grids
  ggtitle("GHG Emissions Source (By Sub-Sector)") + 
  theme(legend.title=element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  xlab("\n") + ylab("Emissions (mtCO2e)\n\n") +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=90, hjust=1, size=13))


ss <- ggplotly(ss) }

else ggplot()

})


observeEvent(input$refresh, {
  js$refresh();
})


})

