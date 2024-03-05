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

#___________________________________________________________________________________________________________________________________________
#### AEMET API ####
#___________________________________________________________________________________________________________________________________________
# Get AEMET list of stations inside the study Area: 
Met_options <- aemet_options(api_key="eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJhLmJhdGxsZUBjcmVhZi51YWIuY2F0IiwianRpIjoiYWMyMGRkZWUtMDFhMS00YTYxLWFmODMtMTNiMTM2NTIyOGYwIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2ODI2MTA0ODQsInVzZXJJZCI6ImFjMjBkZGVlLTAxYTEtNGE2MS1hZjgzLTEzYjEzNjUyMjhmMCIsInJvbGUiOiIifQ.Ml_Iri16hjadDFBgRw3ahGaPFneKuBSjIrMkNrKvbv8", 
                             resolution = "yearly", start_date=as.Date("2000-01-01") , end_date=as.Date("2022-12-31"))

info_stations_API <- get_stations_info_from ('aemet', Met_options)
info_stations_API <- st_transform(info_stations_API, crs=4258)

stations_studyarea <- st_intersection(info_stations_API, study_area)
list_stations <- as.character(stations_studyarea$station_id)

#Access AEMET API.

#Since daily data request can only be downloaded every 31 days, need to set up a recurrent request. 
start_date_request <- as.Date("2000-01-01")
end_date_request <- as.Date("2000-01-31")

#Get firts request and the loop will append next requests to this dataset. 
Met_options <- aemet_options(api_key="eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJhLmJhdGxsZUBjcmVhZi51YWIuY2F0IiwianRpIjoiYWMyMGRkZWUtMDFhMS00YTYxLWFmODMtMTNiMTM2NTIyOGYwIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2ODI2MTA0ODQsInVzZXJJZCI6ImFjMjBkZGVlLTAxYTEtNGE2MS1hZjgzLTEzYjEzNjUyMjhmMCIsInJvbGUiOiIifQ.Ml_Iri16hjadDFBgRw3ahGaPFneKuBSjIrMkNrKvbv8", 
                             resolution = "daily", start_date= start_date_request, end_date=end_date_request, stations=list_stations)

aemet_meteo <- get_meteo_from('aemet', Met_options)


last_date <- as.Date("2022-12-31")


while (end_date_request<= last_date ) {
  start_date_request <-start_date_request + 31
  end_date_request <- end_date_request + 31
  
  Met_options <- aemet_options(api_key="eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJhLmJhdGxsZUBjcmVhZi51YWIuY2F0IiwianRpIjoiYWMyMGRkZWUtMDFhMS00YTYxLWFmODMtMTNiMTM2NTIyOGYwIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2ODI2MTA0ODQsInVzZXJJZCI6ImFjMjBkZGVlLTAxYTEtNGE2MS1hZjgzLTEzYjEzNjUyMjhmMCIsInJvbGUiOiIifQ.Ml_Iri16hjadDFBgRw3ahGaPFneKuBSjIrMkNrKvbv8", 
                               resolution = "daily", start_date= start_date_request, end_date=end_date_request, stations=list_stations)
  
  aemet_meteo_2 <- get_meteo_from('aemet', Met_options)
  
  aemet_meteo <- rbind(aemet_meteo, aemet_meteo_2)
  
}

#exporting as csv: 
write.csv(aemet_meteo, "AEMET_meteo_2000_2022.csv" , row.names=FALSE)
# Export data as shapefile
st_write(aemet_meteo, "AEMETopendata_meteo_Tordera.shp")


#### END ####

#___________________________________________________________________________________________________________________________________________
#### SMC Servei Meteorol?gic de Catalunya API ####
#___________________________________________________________________________________________________________________________________________

# Get SMC list of stations inside the study Area: ______________________________________________####
# I've been recomended by Jessica Amaro Royo from SMC (api.meteocat@gencat.cat) to better make a request for all Catalunya and make a subset after.  

Met_options <- meteocat_options(api_key="rGld0Hyrt69G4Czn2M4pHaFvuO39KKX3azYU3I6t", 
                                resolution = "daily", start_date=as.Date("2001-01-01"))

info_stations_API <- get_stations_info_from ('meteocat', Met_options)
info_stations_API <- st_transform(info_stations_API, crs=4258)

stations_studyarea <- st_intersection(info_stations_API, study_area)
list_stations <- as.character(stations_studyarea$station_id)

#Access SMC API.
start_date_request <- as.Date("2000-01-01") 
#end_date_request <- as.Date("2000-01-31")

Met_options <- meteocat_options(api_key="rGld0Hyrt69G4Czn2M4pHaFvuO39KKX3azYU3I6t", 
                                resolution = "daily", start_date= start_date_request)

meteocat_meteo <- get_meteo_from('meteocat', Met_options) #API return are month by month


last_date <- as.Date("2022-12-01")

while (start_date_request<= last_date ) {
  start_date_request <-start_date_request  %m+% months(1)
  #Get stations List for the month
  Met_options <- meteocat_options(api_key="rGld0Hyrt69G4Czn2M4pHaFvuO39KKX3azYU3I6t", 
                                  resolution = "daily", start_date=start_date_request)
  
  info_stations_API <- get_stations_info_from ('meteocat', Met_options)
  info_stations_API <- st_transform(info_stations_API, crs=4258)
  
  stations_studyarea <- st_intersection(info_stations_API, study_area)
  list_stations <- as.character(stations_studyarea$station_id)
  
  # Get meteorological data
  Met_options <- meteocat_options(api_key="rGld0Hyrt69G4Czn2M4pHaFvuO39KKX3azYU3I6t", 
                                  resolution = "daily", start_date= start_date_request, stations=list_stations)
  
  meteocat_meteo_2 <- get_meteo_from('meteocat', Met_options)
  #meteocat_meteo_2$mean_wind_speed <- NA #Add field when needed
  meteocat_meteo <- rbind(meteocat_meteo, meteocat_meteo_2)
  
}
meteocat_meteo_2$mean_wind_speed <- NA
meteocat_meteo$mean_wind_direction <- NA
meteocat_meteo$max_relative_humidity <- NA
#exporting as csv: 
write.csv(meteocat_meteo, "METEOCAT_meteo_2008_2022.csv" , row.names=FALSE)

#Access SMC API. For all stations.______________________________________________________________#### 

start_date_request <- as.Date("2000-01-01") 
start_date_request <- as.Date("2001-11-01")

Met_options <- meteocat_options(api_key="rGld0Hyrt69G4Czn2M4pHaFvuO39KKX3azYU3I6t", 
                                resolution = "daily", start_date= start_date_request)

meteocat_meteo <- get_meteo_from('meteocat', Met_options) #API return are month by month

write.csv(meteocat_meteo, paste0("SMC/SMC_DadesDiàries_serie 2000-2022/Daily_data_RAW/meteocat_",start_date_request,"_v2.csv")) #store in a .csv
last_date <- as.Date("2022-12-01")

start_date_request <- as.Date("2003-09-01") # Data download stopped, restart here. Missing data 2003-10/11/12
start_date_request <- as.Date("2001-09-01") #Missing MIn temperature redownloaded 
start_date_request <- as.Date("2020-07-01") # Missing max_relative_humidity SMC data is like this (added NA field manually)

while (start_date_request<= last_date ) {
  
  start_date_request <-start_date_request  %m+% months(1)
  
  # Get meteorological data
  Met_options <- meteocat_options(api_key="rGld0Hyrt69G4Czn2M4pHaFvuO39KKX3azYU3I6t", 
                                  resolution = "daily", start_date= start_date_request)
  
  meteocat_meteo <- get_meteo_from('meteocat', Met_options)
  write.csv(meteocat_meteo, paste0("SMC/meteocat_",start_date_request,".csv"))
}



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


 




