#--------------------------------------------------------------------------
 # aWhere R Tutorial: Forecast summary table 
 #
 # Purpose of script: 
 # This code will show you how to access aWhere's forecast data from the API 
 # (Application Programming Interface) for your location(s) of interest. 
 # Prior to running this script, we enourage you to find the latitude and 
 # longitude of an area of interest by using Google Maps, QGIS and aWhere's 
 # geospatial files found on apps.awhere.com, or by using GPS points that you 
 # have previously collected. 
 #
 # The outputs of this script include a csv of the forecast 
 # (7-day, 168 hours) for your location of interest and a table of the 
 # forecast that can be embedded in reports or visuals for quick interpretation.
 #
 # You will need to be connected to the internet to run this script.

#--------------------------------------------------------------------------
 # Install and load packages -----------------------------------------------
 #
 # Clear your environment and remove all previous variables
rm(list = ls())

 # Install the aWhere R Charts package, if you have not already
devtools::install_github("awhereAPI/aWhere-R-Charts")

 # Load the packages needed for this script.
 # If they have not been installed yet on your computer, 
 # using this code to install them: install.packages("NAME OF PACKAGE")
library(aWhereAPI)
library(data.table)
library(gridExtra)
library(grid)
library(gtable)
library(grDevices)
library(dplyr)

 # Load aWhere credentials -------------------------------------------------
 #
 # You will need to load your credentials file which includes your aWhere 
 # key and secret, like a username and password. This gives you a token which 
 # shows that you have access to the API and all of aWhere's data. Your 
 # credentials should be kept in a location where you can easily find them. 
 # Copy the pathfile name and paste it below over the phrase, 
 # "YOUR CREDENTIALS HERE"
aWhereAPI::load_credentials("YOUR CREDENTIALS HERE")

 # Set working & output directories ----------------------------------------
 #
 # Next, you need to set your working directory. This is the location on your 
 # computer where R will automatically save the output files created by this 
 # script.
 #
 # To set your working directory, find the folder on your computer where you 
 # would like the outputs of this script to be saved, copy the pathfile name 
 # and paste it over the phrase, "YOUR WD HERE"
 #
 # This sets your working directory to the working_dir path
working_dir <- "YOUR WD HERE" 
setwd(working_dir) 
             
 # Now you will create the folder within your working directory where your 
 # output csv files will be saved. This line creates a folder in your working 
 # directory called outputCSVs. You can navigate to your working directory on 
 # your computer and see that this folder was created.
dir.create(path = 'outputCSVs/', showWarnings = FALSE, recursive = TRUE) 

 # Location(s) of interest -------------------------------------------------
 #
 # In this section, we will pull forecast data for your location of interest. 
 # First, determine the location's name, latitude, and longitude. 
 # You can use QGIS, Google Maps, or your own data to find this information.
 # Next, create a text file with this location information. Refer to 
 # the "locations.txt" text file example in the RunSet folder for formatting
 # this file. It must have 3 columns called place_name, latitude, longitude. 
 # An example of a row with location information would thus be:
 #     place_name, latitude, longitude
 #     Nairobi, -1.283, 36.816
 #
 # CHANGE THIS to the path and name of your locations text file
locations_file <- "RunSet/locations.txt"

 # Read the location(s) text file 
locations <- read.csv(locations_file)

 # Forecast summary table --------------------------------------------------
 #
 # The following is called a loop process - you only need to run the line 
 # that starts with "for", and the subsequent lines will automatically run 
 # until the end of the script. The forecast data and table outputs will be 
 # saved in your working directory.

for (i in(1:nrow(locations))) { 

   # Get the first latitude, longitude, and name of the current location
  lat <- locations$latitude[i]
  lon <- locations$longitude[i]
  place_name <- locations$place_name[i]
  
   # Pull the weather forecast directly from the aWhere API
  forecast<- aWhereAPI::forecasts_latlng(lat
                                        ,lon 
                                        ,day_start = as.character(Sys.Date()) 
                                        ,day_end = as.character(Sys.Date()+3)
                                        ,block_size = 6)  # MUST use 6 hours
  
   # Save .csv file of the dataset in the outputCSVs folder created within 
   # your working directory
  write.csv(forecast, 
            file = paste0("outputCSVs/",place_name,"_Forecast-6hour.csv"), 
            row.names=F) 
   # Format the forecast data for a summary table --------------------------
   #
   # Select specific columns for the forecast summary
  df_sub <- subset(forecast, select = c(1,2,3,6,7,8,9,10, 18))
  
   # Here, is.num() is TRUE for numeric columns and FALSE otherwise. 
   # We then apply the "round" function to the numeric columns:
  is.num <- sapply(df_sub, is.numeric)
  df_sub[is.num] <- lapply(df_sub[is.num], round, 2)
  
   # Shorten column names
  colnames(df_sub) <- c("Lat"
                        ,"Lon"
                        ,"dateTime"
                        ,"Conditions"
                        ,"Max_Temp"
                        ,"Min_Temp"
                        ,"Chance_Precip"
                        ,"Amount_Precip"
                        ,"Max_Wind")

   # Convert the forecast data.frame to a data.table object
  df_sub <- data.table::as.data.table(df_sub)
  
   # Shorten the string for conditions - drop GMT time zone
  df_sub[, c('date','startTime') := tstrsplit(x = dateTime
                                             ,split = 'T'
                                             ,fixed = TRUE)]
  
  df_sub[, c('startTime', 'timeZone') := tstrsplit(x = startTime
                                                 ,split = '\\+|-')]
  
  df_sub[, c('startHour', 'startMin', 'startSec') := tstrsplit(x = startTime
                                                            ,split = ':'
                                                            ,fixed = TRUE
                                                        ,type.convert = TRUE)]
  
   # Wrap midnight (24:00) to become 00:00
  df_sub[, endHour := startHour + 6]
  df_sub[endHour == 24, endHour := 0]
  
   # Create timeString based on the hourly time block
  df_sub[startHour == 0,  timeString := 'Midnight-0600']
  df_sub[startHour == 6,  timeString := '0600-Noon']
  df_sub[startHour == 12, timeString := 'Noon-1800']
  df_sub[startHour == 18, timeString := '1800-Midnight']
  
   # Remove Columns not to be included in output
  df_sub[, c('dateTime'
            ,'timeZone'
            ,'startTime'
            ,'startMin'
            ,'startSec'
            ,'startHour'
            ,'endHour') := NULL]
  
   # Reorder Columns
  df_sub = df_sub %>% dplyr::select(date
                                    ,timeString
                                    ,Max_Temp
                                    ,Min_Temp
                                    ,Chance_Precip
                                    ,Amount_Precip
                                    ,Max_Wind
                                    ,Conditions)          
  
   # Convert Table to new format for formatting
  table <- gridExtra::tableGrob(df_sub)
  
  title <- grid::textGrob(paste0("Place = ", place_name,
                           '   Latitude = ', lat,
                           '  Longitude = ', lon, '\n')
                           ,gp = grid::gpar(fontsize = 25))
  
  padding <- grid::unit(0.1,"line")
  
  table <- gtable::gtable_add_rows(table
                           ,heights = grid::grobHeight(title) + padding
                           ,pos = 0)
  
  table <- gtable::gtable_add_grob(table
                           ,list(title)
                           ,t = 1
                           ,l = 1
                           ,r = ncol(table))

   # Save the forecast summary table as an image
  out_file = paste0(place_name,"_table.png")
  grDevices::png(filename = out_file, 
                 width = 980,
                 height = 780,
                 bg = "white")
  
  grid::grid.draw(table)
  dev.off()
  
}

