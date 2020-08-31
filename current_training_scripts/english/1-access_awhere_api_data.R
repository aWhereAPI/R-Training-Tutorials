#--------------------------------------------------------------------------
 # aWhere R Tutorial: Access aWhere API Data 
 # Tutuorial de R de aWhere: Accesando a los datos del API de aWhere
 #
 # Purpose of script: 
 # This code will show you how to access aWhere's ag-weather datasets from 
 # the API (Application Programming Interface) for your location of interest. 
 # Prior to running this script, we enourage you to find the latitude and 
 # longitude of an area of interest by using Google Maps, QGIS and aWhere's 
 # geospatial files found on apps.awhere.com, or by using GPS points that you 
 # have previously collected. 
 #
 # This script provides the following datasets for your location of interest:
 # 1. A csv output of the Forecast (Hourly, 6 hour, 12-hour, 
 #      or daily blocks of time) 
 # 2. Observed data for any time period between 2008 and present
 # 3. Long-Term Normals (LTN) for chosen time period between 2008 and present
 # 4. A csv output called the "aWhere Weather Dataset" which includes all 
 #      observed variables and all LTN variables including the differences 
 #      from normal. 
 #
 # You will need to be connected to the internet to run this script.
 #

#--------------------------------------------------------------------------
 # Install and load packages -----------------------------------------------
 #
 # Clear your environment and remove all previous variables
rm(list = ls())

 # Install the aWhere R packages, if you have not already
devtools::install_github("aWhereAPI/aWhere-R-Library")
devtools::install_github("aWhereAPI/aWhere-R-Charts")

 # Load the packages needed for this script.
 # If they have not been installed yet on your computer, 
 # using this code to install them: install.packages("NAME OF PACKAGE")
library(devtools)
library(rgeos)
library(raster)
library(foreach)
library(aWhereAPI)
library(aWhereCharts)

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
working_dir <- "YOUR WD HERE" 

 # This sets your working directory to the working_dir path
setwd(working_dir) 
                  
 # Now you will create the folder within your working directory where your 
 # output csv files will be saved. This line creates a folder in your working 
 # directory called outputCSVs. You can navigate to your working directory on 
 # your computer and see that this folder was created.
dir.create(path = "outputCSVs/", showWarnings = FALSE, recursive = TRUE) 

 # Now that your parameters have been set for this script, you are ready to 
 # begin requesting data from the API and investigating your area of interest.
 #
 # Forecast ----------------------------------------------------------------
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
 # CHANGE THIS to the path of your locations text file
locations_file <- "YOUR LOCATION FILE.txt" 

 # Read the location(s) text file 
locations <- read.csv(locations_file)

for (i in(1:nrow(locations))) { 
  
   # Get the first latitude, longitude, and name of your location(s) of interest 
  lat <- locations$latitude[i]
  lon <- locations$longitude[i]
  place_name <- locations$place_name[i]
  
   # Pull the weather forecast directly from the aWhere API  
  forecast <- aWhereAPI::forecasts_latlng(lat
                                           ,lon 
                                           ,day_start = as.character(Sys.Date()) 
                                           ,day_end = as.character(Sys.Date()+7)
                                           ,block_size = 6) 
  
   #  The default forecast parameters in the code above are: 
   #  Starting date is today, Sys.Date()
   #  Ending date is seven days from now, Sys.Date() + 7
   #  Block size refers to the number of hours each data point will consist 
   #  of. By default, this value is 6, which pulls forecast data in 6-hour blocks. 
   #  A block size of 1 would yield hourly blocks of forecast data.
   #
   # Save a .csv file of the forecast data in the outputCSVs folder that you 
   # created within your working directory
  write.csv(forecast, file = paste0("outputCSVs/Forecast-6hour-",place_name,".csv"), row.names=F) 
  
   # You can also click on the forecast dataframe in the "environment" tab in the 
   # top right console to see the data in RStudio!
   #
   # Observed Data -----------------------------------------------------------
   #
   # Here you will pull the historical data for your location of interest.
   #
   # Set the starting and ending dates to a time period of interest
   #
   # January 1, 2016
  starting_date <- "2018-01-01" 
   
   # two days ago
  ending_date <- as.character(Sys.Date() - 2) 
                                  
   # Pull observed weather data from the aWhere API 
  observed <- aWhereAPI::daily_observed_latlng(latitude = lat,
                                               longitude = lon,
                                               day_start = starting_date,
                                               day_end = ending_date)
  
  write.csv(observed, file = paste0("outputCSVs/observedData-",place_name,".csv"), row.names=F) 
  
   # The parameters for this function can have many formats.
   # You can change the starting/ending dates for a timeframe of interest. 
   #   The starting date can be as early as 2008. 
   #   You can use the "YYYY-MM-DD" format for a specific date.
   #   You can also use Sys.Date() to make your end date today, 
   #   or similarly, use Sys.Date() - 1 to make your end date yesterday. 
   #   NOTE that observed data can ONLY be in the past. You will get an error 
   #   if a future date is selected! 
   #
   # Click the "observed" dataframe in the "environment" tab on the top right 
   # console to see the data!
   #
   # Agronomic data ----------------------------------------------------------
   #
   # Here you will pull agronomic data for your location and time of interest. 
   # If you do not change the "starting_date" and "ending_date" variables,
   # then the time period will remain the same from the observed data pulled above. 
   #
   # Pull agronomic weather data from the aWhere API
  ag <- aWhereAPI::agronomic_values_latlng(lat
                                            ,lon 
                                            ,day_start = starting_date 
                                            ,day_end = ending_date)
  
   # Click the "ag" dataframe in the "environment" tab on the top right 
   # console to see the data!

  write.csv(ag, file = paste0("outputCSVs/agronomicsData-",place_name,".csv"), row.names=F) 
  
   # Long Term Normals -------------------------------------------------------
   #
   # Here you will pull the long-term normals (LTN) for your location and time 
   # period of interest. 
   #
   # LTN values will be calculated across this range of years 
  year_start <- 2011
  year_end <- 2018
  
   # Specify the starting and ending month-day of interest, 
   # such as the growing season in your region 
   #
   # January 1
  monthday_start <- "01-01" 
                            
   # June 16
  monthday_end <- "06-16"   
                            
   # Pull LTN weather data from the aWhere API 
   #
   # you can choose to exclude years from the LTN
  ltn <- weather_norms_latlng(lat, lon,
                               monthday_start = monthday_start,
                               monthday_end = monthday_end,
                               year_start = year_start,
                               year_end = year_end,
                               # you can choose to exclude years from the LTN
                               exclude_years = c("2011", "2016")) 

   # Click the "ltn" dataframe in the "environment" tab on the top right 
   # console to see the data!  
  write.csv(ltn, file = paste0("outputCSVs/ltnData-",place_name,".csv"), row.names=F) 
  
   # Full aWhere Ag-Weather Dataset ------------------------------------------
   #
   # This section combines all of the above datasets into one cohesive .csv for 
   # analysis. You can change the location and time period as needed in 
   # the lines of code below. 
  starting_date <- "2018-01-01"
  ending_date <- "2019-06-16"
  year_start <- 2008
  year_end <- 2018
  
   # This function generates a clean dataset with observed AND forecast 
   # agronomics AND Long Term Normals!
  weather_df <- generateaWhereDataset(lat = lat, 
                                      lon = lon, 
                                      day_start = starting_date, 
                                      day_end = ending_date, 
                                      year_start = year_start, 
                                      year_end = year_end)
  
   # Save .csv file of the dataset in the outputCSVs folder created within 
   # your working directory
  
  write.csv(weather_df, 
            file = paste0("outputCSVs/aWhereWeatherDataset-",place_name,".csv"), 
            row.names=F) 
}


