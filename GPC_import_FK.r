
## Required packages
#install.packages("readxl")
#install.packages("openair")
#install.packages("plyr")

## Load packages
require(readxl)
require(openair)
require(plyr)
library(threadr)
library(reshape2)
library(ggplot2)
library(plotly)
library(scales)
library(leaflet)
library(htmlwidgets)

setwd("C:/C40_Rose")
# file <- "FK_GPC Reporting Tool_V2.0 Beta_ENG_COM.xlsm"
file <- "FK_Buenos Aires_GPC_2015_Final_ORIGINAL.xlsm"


## make a dataframe with the city location (lat, long)------------------

city <- c("Bueons Aires", "London", "New York", "Milano")
latitude <- c(-34.61, 51.5, 40.71, 45.46)
longitude <- c(-58.37, -0.12, -74.35, 9.18)
Map_data <- data.frame(city, longitude, latitude)

##############################################################################

importWorksheets <- function (file, exclude_sheets = 17, skip_rows = 8) {
  # Get sheet names
  sheet_names <- readxl::excel_sheets(file)
  # select only sheet names of interest
#  sheet_names <- subset(sheet_names, sheet_names %in% c("I - Stationary", "II - Transport", "III - Waste", "IV - IPPU", "V - AFOLU"))
#  sheet_names <- subset(sheet_names, sheet_names %in% c("Overview"))
#  sheet_names <- subset(sheet_names, sheet_names %in% c("Graphs"))
   sheet_names <- subset(sheet_names, sheet_names %in% c("Table B. Summary"))
 # sheet_names <- sheet_names[exclude_sheets:(exclude_sheets+5)]
  sheet_names
  # Apply function to all sheets, with plyr
  df <- plyr::ldply(sheet_names, function (x) readxl::read_excel(file, x, skip = skip_rows, 
                      col_names = FALSE), .progress = "time")
  df <- data.frame(df)
} 

  # Filter NAs: Remove row from the data frame if the first column is empty
  #  df <- df[!is.na(df[, 1]), ]

# sheet_names <- readxl::excel_sheets("FK_Buenos Aires_GPC_2015_Final_ORIGINAL.xlsm")
# sheet_names <- subset(sheet_names, sheet_names %in% c("I - Stationary", "II - Transport", "III - Waste", "IV - IPPU", "V - AFOLU"))

# subsitute NA with o
# d[is.na(d)] <- 0


#### Import data from the xlsm spreadsheet------------------------------------------------
# GPC_sheet <- importWorksheets("FK_GPC Reporting Tool_V2.0 Beta_ENG_COM.xlsm")
GPC_sheet <- importWorksheets("FK_Buenos Aires_GPC_2015_Final_ORIGINAL.xlsm")
GPC_sheet <- subset(GPC_sheet, select = c(2:9))

# SECTORS--------------------------------------------------------------
sectors <- GPC_sheet[1:9,-2]

# Join names of sectors and their specifications
sectors$Sectors <- paste(sectors$X1, sectors$X3, sep=" ")

# remove NA from names (data cleaning!)
sectors$Sectors <- gsub("NA ", "", sectors$Sectors, fixed = TRUE)
sectors <- sectors[-1 ,-(1:2)]

colnames(sectors) <- c("Scope_1", "Scope_2", "Scope_3", "Basic", "Basic+", "Sectors")

sectors <- sectors %>%
select(Sectors,
       Basic,
       `Basic+`,
       Scope_1)

# sectors <- as.data.frame(sectors)
#  GPC_sheets["Total",] <- colSums(GPC_sheets)

# convert into numeric
sectors[, c(2:4)] <- sapply(sectors[, c(2:4)], as.numeric)
row.names(sectors) = NULL
write.csv(sectors, "sectors.csv")





# SUBSECTORS----------------------------------------------------------
subsectors <- GPC_sheet[13:52, -c(1,3,4,8)]
rownames(subsectors) <- 1:nrow(subsectors)

# remove totals and intermediate headers
# subsectors <- subsectors[-c(1,2,7,12,13,19,20,21, 22, 25, 26, 27, 28, 29,30,33,34,38,39),]

subsectors <- subsectors[c(3,4,5,6,8, 9, 10, 11, 
                           14, 15, 16, 17, 18,
                           21, 22, 23,24,
                           31, 32,
                           35, 36, 37),]

colnames(subsectors) <- c("Sub_sectors", "Scope_1", "Scope_2", "Scope_3")
options(warn=-1)
subsectors[, c(2:4)] <- sapply(subsectors[, c(2:4)], as.numeric)
# rename rows
subsectors$`Sub_sectors` <- c("Residential",
                          "Commercial / institutional",
                          "Manufacturing / construction",
                          "Energy industries",
                          "Agriculture",
                          "Non-specified sources",
                          "Fugitive emissions (coal)",
                          "Fugitive emissions (oil and gas)",
                          "On-road",
                          "Railways",
                          "Waterborne",
                          "Aviation",
                          "Off-road",
                          "Solid waste",
                          "Biological waste",
                          "Incineraton",
                          "Wastewater",
                          "Industrial processes",
                          "Product use",
                          "Livestock",
                          "Land",
                          "Aggregate sources")
row.names(subsectors) = NULL
write.csv(subsectors, "subsectors.csv")





#######################################################################################################################
### Interactive plots #################################################################################################
#######################################################################################################################

sectors <- read.csv("sectors.csv")[-1]
sectors <- melt(sectors, id.var="Sectors")
# remove NA values
# sectors <- sectors[!(is.na(sectors$value)), ]

# The palette with grey:
sectors_Palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
# sectors_Palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2") # "#D55E00", "#CC79A7")

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
s

s <- ggplotly(s)
htmlwidgets::saveWidget(as.widget(s), "sectors_Buenos_Aires.html")
s


##=========================================================================================


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

ss

ss <- ggplotly(ss)
htmlwidgets::saveWidget(as.widget(ss), "subsectors_Buenos_Aires.html")
ss


## make a simple leaflet map----------------------------------------------------

# define popup for the iteractive map
  popup_name <- paste0(
    "<strong><i>",
    Map_data$city,
    "</i></strong>")

  map <- leaflet(Map_data) %>%
    addTiles() %>%
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
      baseGroups = c("Thunderforest", "Hydda_Full", "Hydda_Base", "Satellite", "Toner Lite"),
      overlayGroups = c("city"),
      options = layersControlOptions(collapsed = TRUE)
    )
  
  map
  


############################################################################################################
############################################################################################################
############################################################################################################


