
require(readxl)
require(plyr)

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


#### Import data from the xlsm spreadsheet------------------------------------------------
# GPC_sheet <- importWorksheets("FK_GPC Reporting Tool_V2.0 Beta_ENG_COM.xlsm")
# GPC_sheet <- importWorksheets("FK_Buenos Aires_GPC_2015_Final_ORIGINAL.xlsm")
  df <- subset(df, select = c(2:9))

# SECTORS--------------------------------------------------------------
sectors <- df[1:9,-2]

# Join names of sectors and their specifications
sectors$Sectors <- paste(sectors$X1, sectors$X3, sep=" ")

# remove NA from names (data cleaning!)
sectors$Sectors <- gsub("NA ", "", sectors$Sectors, fixed = TRUE)
sectors <- sectors[-1 ,-(1:2)]

colnames(sectors) <- c("Scope 1", "Scope 2", "Scope 3", "Basic", "Basic+", "Sectors")

sectors <- sectors %>%
  select(Sectors,
         Basic,
         `Basic+`,
         `Scope 1`)

# sectors <- as.data.frame(sectors)
#  GPC_sheets["Total",] <- colSums(GPC_sheets)

# convert into numeric
sectors[, c(2:4)] <- sapply(sectors[, c(2:4)], as.numeric)
write.csv(sectors, "sectors.csv")



# SUBSECTORS----------------------------------------------------------
subsectors <- df[13:52, -c(1,3,4,8)]
rownames(subsectors) <- 1:nrow(subsectors)

# remove totals and intermediate headers
# subsectors <- subsectors[-c(1,2,7,12,13,19,20,21, 22, 25, 26, 27, 28, 29,30,33,34,38,39),]

subsectors <- subsectors[c(3,4,5,6,8, 9, 10, 11, 
                           14, 15, 16, 17, 18,
                           21, 22, 23,24,
                           31, 32,
                           35, 36, 37),]

colnames(subsectors) <- c("Sub-sectors", "Scope-1", "Scope-2", "Scope-3")
options(warn=-1)
subsectors[, c(2:4)] <- sapply(subsectors[, c(2:4)], as.numeric)
# rename rows
subsectors$`Sub-sectors` <- c("Residential",
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
write.csv(subsectors, "subsectors.csv")

}
