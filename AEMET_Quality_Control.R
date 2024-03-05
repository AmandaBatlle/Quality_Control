# Amanda Batlle a.batlle@crefa.uab.cat

# meteorological Data quality Control

#Connect the project to Github
library(usethis)
use_github() # a repositotory will be created in Github


#Set working Directory:

setwd("XXXXX")

#Install libraries

install.packages("sf")
install.packages("ggplot2")
install.packages ("tidyr")
install.packages('lubridate')
install.packages("dplyr")

install.packages("patchwork")

# Install MeteoSpain from Github (new version allows to download data before 2008)
install.packages("remotes")
library(remotes)
remotes::install_github("emf-creaf/meteospain", ref = "devel")

#Open libraries
library(sf)
library(ggplot2)
library(tidyr)
library(lubridate)
library(dplyr)

library(meteospain)
library(patchwork) #merge several GGPLOT graphs in one page



# Read Study area Polygon: 
shapefile_path <- "C:/Users/a.batlle/Documents/local_I-CISK/Ambit/50km_Land_GuadalquivirPedroches/50km_Land_GuadalquivirPedroches.shp"
study_area <- st_read(shapefile_path)

#Get a list of METEO data files to process.
# List all CSV files that start with "Cuarto_Bloque_Precipitacion_mensual_"
csv_files <- list.files(path="C:/Users/a.batlle/Documents/DADES/AEMET/Miquel_2023/dades_aemet/PETICIO_4", pattern = "^Cuarto_Bloque_Precipitacion_mensual_.*\\.csv$", full.names=TRUE )

# NOTE: About the pattern
#   ^ asserts the start of the string.
#   Segundo matches the literal characters "Segundo" at the beginning of the string.
#   .* matches any character (except for line terminators) zero or more times.
#   \.csv matches the literal characters ".csv". The . is escaped with a backslash \ because . in regular expressions usually means "match any single character", but in this case, you want to match the period character literally.
#   $ asserts the end of the string.

# Read first element from the list to generate patterns dataframe

AEMET_data <- read.csv(csv_files[1], fileEncoding = "latin1", sep = ";")

csv_files <- csv_files[-1] #Remove firts element from the list before starting the loop

for (csv in csv_files) {
  csv_data <- read.csv(csv, fileEncoding = "latin1", sep = ";")
  
  # Merge with pattern file: 
  AEMET_data <- rbind(AEMET_data,csv_data )
}
#Correct coordinates format
AEMET_data$LONGITUD <- as.numeric(gsub(",", ".", AEMET_data$LONGITUD))
AEMET_data$LATITUD <- as.numeric(gsub(",", ".", AEMET_data$LATITUD))

# Create a spatial object from dataframe and convert to Projected CRS: ETRS89 / UTM zone 30N
AEMET_data_sf <- st_as_sf(AEMET_data, coords = c("LONGITUD", "LATITUD"), crs = 4326)
AEMET_data_sf <-st_transform(AEMET_data_sf, crs = 25830)


# Intersection with the study area: 
StudyArea_data_sf <- st_intersection(AEMET_data_sf, study_area)

# Plot using ggplot to check correct clipping
ggplot() +
  geom_sf(data = study_area) +
  geom_sf(data = StudyArea_data_sf) +
  theme_minimal()


StudyArea_data <- as.data.frame(StudyArea_data_sf)
#  # Fix Coordinates Fields
# Rebuild latitude and longitude attributes from geometry
StudyArea_data <- StudyArea_data %>%
  separate(geometry, into=c("XPR", "YPR"), sep=" ")
#Correct Coordinate values
StudyArea_data$XPR <- gsub("^c\\(", "", StudyArea_data$XPR)
StudyArea_data$XPR <- gsub(",", "", StudyArea_data$XPR)
StudyArea_data$YPR <- gsub("\\)", "", StudyArea_data$YPR)

# Create New Field YYYYMM
StudyArea_data <- StudyArea_data %>% 
  mutate(YYYYMMdate = make_date(year = AÑO, month = MES))

StudyArea_data$YYYYMM <- paste(StudyArea_data$AÑO,sprintf("%02d", StudyArea_data$MES), sep="-") #sprintf("%02d", df$Mes) is used to format the month values as two digits, padding single-digit months with a leading zero if necessary.

# Correct variable name
StudyArea_data <- StudyArea_data %>% rename(PL = PMES77) # Rename the column from "PMES77" to "PL"


# QUALITY CONTROL ______________________________________________________________

# Quality Control function evaluates a data set and identify potential issues on meterological data from weather stations. 
# Variables are : 
#           - data: data frame
#           - variable: Column name of the variable to evaluate 
#           _ THRESHOLD_serieslength: Minimum lenght (in years) that you want your point data to be. 
#           - THRESHOLD_NApc: Maximum % of NA data fo the station. 

QUALITYCONTROLfunction <- function (dataframe, variable,THRESHOLD_serieslength, THRESHOLD_NApc, ) {} # Under construction


# Creating base dataframes 
stationID_list <- unique(StudyArea_data$INDICATIVO)
ShortSeries <- data.frame(station=character(), initial_date=Date(), end_date=Date(), series_years_length=numeric())
StationHighNApc <- data.frame(station = character(), NApc = numeric())
StationNApc <- data.frame(station = character(), NApc = numeric())


# Set up QUALITY THRESHOLDS

THRESHOLD_serieslength <- 1 # minimum lenght of a series
THRESHOLD_NApc <- 30 # Maximun NA%

for (station in stationID_list) {
  subset_data <- StudyArea_data[StudyArea_data$INDICATIVO == station, ]
  # Create a sequence of all dates between the minimum and maximum dates in your data
  all_dates <- seq(from = as.Date("2020-01-01"), to = as.Date("2023-10-01"), by = "1 month")  
  # Use the complete function to fill in missing dates
  subset_data <- subset_data %>%
    complete(YYYYMMdate = all_dates)
  
  # 1. Series length threshold____________________________________________________
  
  # Find the earliest and oldest dates
  earliest_date <- min(StudyArea_data$YYYYMMdate, na.rm = TRUE)
  
  oldest_date <- max(StudyArea_data$YYYYMMdate, na.rm = TRUE)
  
  # Calculate the difference in years between the dates
  difference_in_years <- as.numeric(difftime(oldest_date, earliest_date, units = "weeks") / 52.1775)
  
  # Check if the difference in years is less than the Lenght Threshold
  if (difference_in_years < THRESHOLD_serieslength) {
    print(paste("Time series for", station, "is shortes than", THRESHOLD_serieslength, "year."))
    info <- c(station, earliest_date, oldest_date, difference_in_years)
    ShortSeries <- rbind(ShortSeries, info)
  } 
  
  # 2.. Number of NA values. Who complete are the series. 
  
  # Calculate the number of NA values in the PREC column
  num_na <- sum(is.na(StudyArea_data$PMES77))
  # Calculate the total number of values in the PREC column
  total_values <- length(all_dates)
  
  # Calculate the percentage of NA values
  percent_na <- (num_na / total_values) * 100
  StationNAresult <- c(station, percent_na)
  StationNApc <- rbind(StationNApc, StationNAresult)
  
  # Check if the % of NA values is greater than the Threshold %NA
  if (percent_na > THRESHOLD_NApc) {
    print(paste(station, "have more than", THRESHOLD_NApc, "% NA values."))
    StationHighNApc <- rbind( StationHighNApc,StationNAresult )
  } 
  
  
  
}

# WRITE REPORT TXT: UNDER CONSTRUCTION

# Write report header line
header_line <- "QUALITY CONTROL REPORT"
write(header_line, "Quality Control.txt")

# Write Series lenght info:
header_line <- paste("List of stations with a time series shorter than the threshold:", THRESHOLD_serieslength)
write(header_line, "Quality Control.txt", append = TRUE)

# Write the extracted dataframe to a text file
write.table(ShortSeries, file = "Quality Control.txt", col.names = FALSE, append = TRUE)

# Write NA% info
header_line <- paste("List of stations with a NA% higher than the threshold:", THRESHOLD_NApc, "%.")
writeLines(header_line, "Quality Control.txt", append = TRUE)
# Write the extracted data frame to a text file
write.table(StationHighNApc, file = "Quality Control.txt", row.names = FALSE, append = TRUE)

# Return a message indicating successful completion
return("Extraction complete. Check Quality Control.txt files.")


# WRITE REPORT ECXEL: UNDER CONSTRUCTION

install.packages("openxlsx")
library(openxlsx)


# Create a new Excel workbook
wb <- createWorkbook()

# Add a worksheet for each section of the report
addWorksheet(wb, "Series Length", 
             header = c("QUALITY CONTROL REPORT", paste("List of stations with a time series shorter than the threshold:", THRESHOLD_serieslength), NA))
addWorksheet(wb, "High NA Percentage", 
             header= c("QUALITY CONTROL REPORT", paste("List of stations with an NA% higher than the threshold:", THRESHOLD_NApc, "%."), NA))
addWorksheet(wb, "NA Percentage", 
             header= c("QUALITY CONTROL REPORT","List of stations with an NA%.", NA))


# Write Series length info to Excel
writeData(wb, sheet = "Series Length", ShortSeries, startRow = 2)

# Write NA% info to Excel
writeData(wb, sheet = "High NA Percentage", StationHighNApc, startRow = 2)
writeData(wb, sheet = "NA Percentage", StationNApc, startRow = 2)
# Save the workbook to an Excel file
saveWorkbook(wb, "Quality Control.xlsx")



}



# 2. Outlier identification


# 3. Coordinates change 


# 4. Neighboring comparisons


# CONVERTING TO MIRAMON FORMAT. 
all_dates <- seq(from = as.Date("2020-01-01"), to = as.Date("2023-10-01"), by = "1 month") 
month <- "2020-01-01"
for (month in all_dates) {
  subset_data <- StudyArea_data[StudyArea_data$YYYYMMdate == month, ]
  date <- as.Date(month)
  PLmonthNUM <- as.numeric(format(date, "%m"))
  PLyear <- format(date, "%Y")
  
  # PLmonthNUM <-as.numeric(date[[1]][2])
  # PLmonth <- NULL # Initialize PLmonth
  if(PLmonthNUM == 1) { PLmonth <- "GE"}
  if(PLmonthNUM == 2 ) { PLmonth <- "FE"}
  if(PLmonthNUM == 3) { PLmonth <- "MR"}
  if(PLmonthNUM == 4) { PLmonth <- "AB"}
  if(PLmonthNUM == 5) { PLmonth <- "MG"}
  if(PLmonthNUM == 6) { PLmonth <- "JN"}
  if(PLmonthNUM == 7) { PLmonth <- "JL"}
  if(PLmonthNUM == 8) { PLmonth <- "AG"}
  if(PLmonthNUM == 9) { PLmonth <- "SE"}
  if(PLmonthNUM == 10) { PLmonth <- "OC"}
  if(PLmonthNUM == 11) { PLmonth <- "NO"}
  if(PLmonthNUM == 12) { PLmonth <- "DE"}
  #export the results table to .csv, will be stored in the working directory
  write.csv(subset_data, file=paste0("PL_",PLmonth, "_",PLyear,".csv"), row.names = FALSE)
  
} 




install.packages('meteospain') # older version in CRAN

install.packages("ggplot2")
install.packages ("tidyr")
install.packages('lubridate')
install.packages("dplyr")



# Install MeteoSpain from Github (new version allows to download data before 2008)
install.packages("remotes")
library(remotes)
remotes::install_github("emf-creaf/meteospain", ref = "devel")

#Open libraries
library(sf)
library(ggplot2)
library(tidyr)
library(lubridate)
library(dplyr)

library(meteospain)
library(patchwork) #merge several GGPLOT graphs in one page

# Read Study area Polygon: 
shapefile_path <- "C:/Users/a.batlle/Documents/local_I-CISK/Ambit/50km_Land_GuadalquivirPedroches/50km_Land_GuadalquivirPedroches.shp"
study_area <- st_read(shapefile_path)

#Get a list of METEO data files to process.
# List all CSV files that start with "Cuarto_Bloque_Precipitacion_mensual_"
csv_files <- list.files(path="C:/Users/a.batlle/Documents/DADES/AEMET/Miquel_2023/dades_aemet/PETICIO_4", pattern = "^Cuarto_Bloque_Precipitacion_mensual_.*\\.csv$", full.names=TRUE )

# NOTE: About the pattern
#   ^ asserts the start of the string.
#   Segundo matches the literal characters "Segundo" at the beginning of the string.
#   .* matches any character (except for line terminators) zero or more times.
#   \.csv matches the literal characters ".csv". The . is escaped with a backslash \ because . in regular expressions usually means "match any single character", but in this case, you want to match the period character literally.
#   $ asserts the end of the string.

# Read first element from the list to generate patterns dataframe

AEMET_data <- read.csv(csv_files[1], fileEncoding = "latin1", sep = ";")

csv_files <- csv_files[-1] #Remove firts element from the list before starting the loop

for (csv in csv_files) {
  csv_data <- read.csv(csv, fileEncoding = "latin1", sep = ";")
  
  # Merge with pattern file: 
  AEMET_data <- rbind(AEMET_data,csv_data )
}
#Correct coordinates format
AEMET_data$LONGITUD <- as.numeric(gsub(",", ".", AEMET_data$LONGITUD))
AEMET_data$LATITUD <- as.numeric(gsub(",", ".", AEMET_data$LATITUD))

# Create a spatial object from dataframe and convert to Projected CRS: ETRS89 / UTM zone 30N
AEMET_data_sf <- st_as_sf(AEMET_data, coords = c("LONGITUD", "LATITUD"), crs = 4326)
AEMET_data_sf <-st_transform(AEMET_data_sf, crs = 25830)


# Intersection with the study area: 
StudyArea_data_sf <- st_intersection(AEMET_data_sf, study_area)

# Plot using ggplot to check correct clipping
ggplot() +
  geom_sf(data = study_area) +
  geom_sf(data = StudyArea_data_sf) +
  theme_minimal()


StudyArea_data <- as.data.frame(StudyArea_data_sf)
#  # Fix Coordinates Fields
# Rebuild latitude and longitude attributes from geometry
StudyArea_data <- StudyArea_data %>%
  separate(geometry, into=c("XPR", "YPR"), sep=" ")
#Correct Coordinate values
StudyArea_data$XPR <- gsub("^c\\(", "", StudyArea_data$XPR)
StudyArea_data$XPR <- gsub(",", "", StudyArea_data$XPR)
StudyArea_data$YPR <- gsub("\\)", "", StudyArea_data$YPR)

# Create New Field YYYYMM
StudyArea_data <- StudyArea_data %>% 
  mutate(YYYYMMdate = make_date(year = AÑO, month = MES))

StudyArea_data$YYYYMM <- paste(StudyArea_data$AÑO,sprintf("%02d", StudyArea_data$MES), sep="-") #sprintf("%02d", df$Mes) is used to format the month values as two digits, padding single-digit months with a leading zero if necessary.

# Correct variable name
StudyArea_data <- StudyArea_data %>% rename(PL = PMES77) # Rename the column from "PMES77" to "PL"


# QUALITY CONTROL ______________________________________________________________

# Quality Control function evaluates a data set and identify potential issues on meterological data from weather stations. 
# Variables are : 
#           - data: data frame
#           - variable: Column name of the variable to evaluate 
#           _ THRESHOLD_serieslength: Minimum lenght (in years) that you want your point data to be. 
#           - THRESHOLD_NApc: Maximum % of NA data fo the station. 

QUALITYCONTROLfunction <- function (data, variable,THRESHOLD_serieslength, THRESHOLD_NApc, ) {}


# Creating base dataframes 
stationID_list <- unique(StudyArea_data$INDICATIVO)
ShortSeries <- data.frame(station=character(), initial_date=Date(), end_date=Date(), series_years_length=numeric())
StationHighNApc <- data.frame(station = character(), NApc = numeric())
StationNApc <- data.frame(station = character(), NApc = numeric())


# Set up QUALITY THRESHOLDS

THRESHOLD_serieslength <- 1 # minimum lenght of a series
THRESHOLD_NApc <- 30 # Maximun NA%

for (station in stationID_list) {
  subset_data <- StudyArea_data[StudyArea_data$INDICATIVO == station, ]
  # Create a sequence of all dates between the minimum and maximum dates in your data
  all_dates <- seq(from = as.Date("2020-01-01"), to = as.Date("2023-10-01"), by = "1 month")  
  # Use the complete function to fill in missing dates
  subset_data <- subset_data %>%
    complete(YYYYMMdate = all_dates)
  
  # 1. Series length threshold____________________________________________________
  
  # Find the earliest and oldest dates
  earliest_date <- min(StudyArea_data$YYYYMMdate, na.rm = TRUE)
  
  oldest_date <- max(StudyArea_data$YYYYMMdate, na.rm = TRUE)
  
  # Calculate the difference in years between the dates
  difference_in_years <- as.numeric(difftime(oldest_date, earliest_date, units = "weeks") / 52.1775)
  
  # Check if the difference in years is less than the Lenght Threshold
  if (difference_in_years < THRESHOLD_serieslength) {
    print(paste("Time series for", station, "is shortes than", THRESHOLD_serieslength, "year."))
    info <- c(station, earliest_date, oldest_date, difference_in_years)
    ShortSeries <- rbind(ShortSeries, info)
  } 
  
  # 2.. Number of NA values. Who complete are the series. 
  
  # Calculate the number of NA values in the PREC column
  num_na <- sum(is.na(StudyArea_data$PMES77))
  # Calculate the total number of values in the PREC column
  total_values <- length(all_dates)
  
  # Calculate the percentage of NA values
  percent_na <- (num_na / total_values) * 100
  StationNAresult <- c(station, percent_na)
  StationNApc <- rbind(StationNApc, StationNAresult)
  
  # Check if the % of NA values is greater than the Threshold %NA
  if (percent_na > THRESHOLD_NApc) {
    print(paste(station, "have more than", THRESHOLD_NApc, "% NA values."))
    StationHighNApc <- rbind( StationHighNApc,StationNAresult )
  } 
  
  
  
}

# WRITE REPORT TXT: 



# Write report header line
header_line <- "QUALITY CONTROL REPORT"
write(header_line, "Quality Control.txt")

# Write Series lenght info:
header_line <- paste("List of stations with a time series shorter than the threshold:", THRESHOLD_serieslength)
write(header_line, "Quality Control.txt", append = TRUE)

# Write the extracted dataframe to a text file
write.table(ShortSeries, file = "Quality Control.txt", col.names = FALSE, append = TRUE)

# Write NA% info
header_line <- paste("List of stations with a NA% higher than the threshold:", THRESHOLD_NApc, "%.")
writeLines(header_line, "Quality Control.txt", append = TRUE)
# Write the extracted data frame to a text file
write.table(StationHighNApc, file = "Quality Control.txt", row.names = FALSE, append = TRUE)

# Return a message indicating successful completion
return("Extraction complete. Check Quality Control.txt files.")


# WRITE REPORT ECXEL:

install.packages("openxlsx")
library(openxlsx)


# Create a new Excel workbook
wb <- createWorkbook()

# Add a worksheet for each section of the report
addWorksheet(wb, "Series Length", 
             header = c("QUALITY CONTROL REPORT", paste("List of stations with a time series shorter than the threshold:", THRESHOLD_serieslength), NA))
addWorksheet(wb, "High NA Percentage", 
             header= c("QUALITY CONTROL REPORT", paste("List of stations with an NA% higher than the threshold:", THRESHOLD_NApc, "%."), NA))
addWorksheet(wb, "NA Percentage", 
             header= c("QUALITY CONTROL REPORT","List of stations with an NA%.", NA))


# Write Series length info to Excel
writeData(wb, sheet = "Series Length", ShortSeries, startRow = 2)

# Write NA% info to Excel
writeData(wb, sheet = "High NA Percentage", StationHighNApc, startRow = 2)
writeData(wb, sheet = "NA Percentage", StationNApc, startRow = 2)
# Save the workbook to an Excel file
saveWorkbook(wb, "Quality Control.xlsx")



}



# 2. Outlier identification


# 3. Coordinates change 


# 4. Neighboring comparisons


 




