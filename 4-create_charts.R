#--------------------------------------------------------------------------
# aWhere R Tutorial: Create Charts
#
# Purpose of script: 
# This code will show you how to quickly create a series of charts of your 
# locations of interest to help you understand the weather/agronomic 
# conditions in those locations based on aWhere's ag-weather dataset (includes 
# variables such as temperature, precipitation, potential evapotranspiration, 
# comparisons to long-term normals, and many more.) These charts of can be 
# added to reports for a quick analysis of the ag-weather conditions in your 
# locations of interest. The weather data pulled for your location and time 
# period of interest are also saved as a .csv file for future analysis. 
# 
# Prior to running this script, we encourage you to find the name, latitude, 
# and longitude of your location(s) of interest by using Google Maps, QGIS 
# and aWhere's geospatial files found on apps.awhere.com, or by using GPS 
# points that you have previously collected. 
# 
# This script uses a locations text file which allows you to create the
# climatology chart for all of your locations of interest at once by using a
# process called a "loop." You will need to create a text file with your 
# location(s) of interest. You may edit the "locations.txt" text file sample 
# in the RunSet folder. The locations file must have 3 columns called 
# place_name, latitude, longitude: 
#     place_name, latitude, longitude
#     Nairobi, -1.283, 36.816
#     Addis Ababa, 8.980, 38.757
# 
# Before continuing, please format your locations text file.
# 
# You will need to be connected to the internet to run this script.
#
# Date updated: 2019-12-03
#--------------------------------------------------------------------------

# Install and load packages -----------------------------------------------

# Clear your environment and remove all previous variables
rm(list = ls())

# Load the packages needed for this script
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(aWhereAPI)
library(aWhereCharts)


# Load aWhere credentials -------------------------------------------------

# You will need to load your credentials file which includes your aWhere 
# key and secret, like a username and password. This gives you a token which 
# shows that you have access to the API and all of aWhere's data. Your 
# credentials should be kept in a location where you can easily find them. 
# Copy the pathfile name and paste it below over the phrase, 
# "YOUR CREDENTIALS HERE"

aWhereAPI::load_credentials("YOUR CREDENTIALS HERE")


# Set working & output directories ----------------------------------------

# The working directory is the location on your computer where R will 
# automatically save the output files created by this script.

# To set your working directory, find the folder on your computer where you 
# would like the outputs of this script to be saved, copy the pathfile name 
# and paste it over the phrase, "YOUR WD HERE"

working_dir <- "YOUR WD HERE" 
setwd(working_dir) # This sets your working directory to the working_dir path

# Now you will create the folders within your working directory where your 
# output files will be saved. These lines of code create 3 folders in your 
# working directory called outputCSVs and charts. You can navigate 
# to your working directory on your computer and see that these folders 
# were created.
csv_path <- "outputCSVs/"
chart_path <- "charts/"
dir.create(path = csv_path, showWarnings = FALSE, recursive = TRUE) 
dir.create(path = chart_path, showWarnings = FALSE, recursive = TRUE)

# In the following two, you can specify whether you want to show the 
# charts in the bottom right console, and/or save the charts 
# within the output folders created previously. 
show_charts <- TRUE # TRUE will show charts. FALSE will not show charts. 
save_charts <- TRUE # TRUE will save charts. FALSE will not save charts. 


# Supporting functions ----------------------------------------------------

# This script requires you to load a supporting functions file that is 
# typically saved in the Source folder in your aWhere tutorial folder 
# structure. This step loads additional functions required to create
# the climatology chart. 

# Modify the file path and name below to load the supporting functions file. 
# source("YOUR PATHNAME/supporting_functions.R")
source("Source/supporting_functions.R")


# Location(s) of interest -------------------------------------------------

# In this section, we will pull forecast data for your location of interest. 
# If using a file named "locations.txt" in the RunSet folder: 
locations_file <- "RunSet/locations.txt" 
# Change the path and filename above as needed based on you locations file. 

# Read the location(s) text file 
locations <- read.csv(locations_file)


# Time period of interest -------------------------------------------------

# Specify the starting and ending dates of interest. 
# The starting date year can be as early as 2008. 
# The ending date can include forecast data up to 7 days from now. 
# You may provide these starting and ending dates in multiple formats.  
# You can use the "YYYY-MM-DD" format for a specific YEAR-MONTH-DAY date.
# Or, you can also choose dates relative to the current date recorded 
# by your computer. For example:   
#   day_end <- as.character(Sys.Date())     # Today
#   day_end <- as.character(Sys.Date() + 7) # Forecast 7 days from now

date_start <- "2018-05-15"  # CHANGE TO THE STARTING DATE OF YOUR CHOICE 
date_end <- "2019-05-14"    # CHANGE TO THE ENDING DATE OF YOUR CHOICE 

# Long-term normal (LTN) values will be calculated across this range of years.
year_start <- 2008  # Starting year can be as early as 2008
year_end <- 2018

# Optional: Add additional selected year(s) to charts. 
# This will plot the data from the specified years explicitly. 
#   add_years <- NA             # If you do not want to add additional years. 
#   add_years <- c(2016, 2017)  # To specify multiple additional years. 
add_years <- c(2016, 2017) 



# Additional chart parameters ---------------------------------------------
# (No need to modify these defaults)

# Do not change: effective precip amount 
# (cap to large precipitation events so runoff is not included)
eP <- 30    

# Size of rolling aggregates to calculate when smoothing 
roll_window <- 30

# Adjust the start date to be "roll.avg" days earlier
date_start_extended <- as.character(as.Date(date_start) - roll_window)

# Set colors for the charts: 

# List of original colors for current and LTN lines 
    #colors_orig <- c("#1F83B4", "#FF810E") # blue , orange from original charts
colors_orig <- c("#4575b4", "#fdae61") # blue, orange from new Colorbrewer palette

# list of unique colors for additional lines added 
    #colors_additional <- c("black", "red", "yellow", "purple")
colors_additional <- c("#000000", "#d73027", "#fee090", "#abd9e9") 
                        #black     #red       #yellow    #light blue 

# Just take the number of line colors that are needed for the final chart.
# Since additional years will be plotted first, put those colors first.
# Then Current and LTN colors. 
colors_final <- c(colors_additional[1:length(add_years)],colors_orig)

# Set the width of the lines on the chart
line_width <- 1


# Create charts  ----------------------------------------------------------

# The following is called a loop process - you only need to run the line 
# that starts with "for", and the subsequent lines will automatically run 
# until the end of the script. A set of charts will be generated 
# for each location in your locations text file. Weather data will be written
# to .csv files. The outputs will be saved within your working directory.

for (i in 1:nrow(locations)) {
  
  # Get the latitude, longitude, and name of the current location 
  lat <- locations$latitude[i]
  lon <- locations$longitude[i]
  place_name <- locations$place_name[i]
  
  # Print a status message about the current location 
  print(paste0("Creating charts for location ",
               "(", i, "/", nrow(locations), "): ", place_name))
  
  # Create directories to store outputs for the current location 
  current_csv_path <- paste0(csv_path
                             ,formatGraphTitleForFileName(place_name), "/")
  current_chart_path <- paste0(chart_path
                               ,formatGraphTitleForFileName(place_name), "/")
  dir.create(path = current_csv_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(path = current_chart_path, showWarnings = FALSE, recursive = TRUE)
  
  # Pull the weather data directly from the aWhere API
  cat("   Pulling Weather Data\n")
  weather_df <- aWhereCharts::generateaWhereDataset(lat = lat 
                                                    ,lon = lon
                                                    ,day_start = date_start
                                                    ,day_end = date_end
                                                    ,year_start = year_start
                                                    ,year_end = year_end)
  
  # Pull the weather datasets for the extended date range, which includes a 
  # start date earlier based on the "roll_avg" amount (using the variable
  # roll_window to determine the length). It is necessary to do both this call
  # and the one above instead of just this one and subsetting based on date
  # because the SD info for the accumulated columns will not be linearly
  # related in each set.
  weather_df_extended <- aWhereCharts::generateaWhereDataset(lat = lat
                                         ,lon = lon
                                        ,day_start = date_start_extended
                                        ,day_end = date_end
                                        ,year_start = year_start
                                        ,year_end = year_end)
  
  # Additional years --------------------------------------------------------
  # Generate Data for Plotting Specific Years
  
  # create a data frame to store additional selected year data.
  # the "long" format of this data frame is ideal for plotting with ggplot.
  add_years_df <- data.frame(date = character(),
                             data = double(),
                             var = character(),
                             year = double())
  
  add_years_df_extended <- data.frame(date = character(),
                                      data = double(),
                                      var = character(),
                                      year = double())
  
  # Pull the data sets for additional selected year(s)
  for (year in add_years) {
    
    # If there are no additional selected years to add, break out of loop
    if(is.na(year)) {
      break
    }
    
    # Print a status message to the console 
    cat(paste0("\n    Pulling weather data for selected year: ", year, "\n"))
    
    # Calculate dates to use for data pull of additional lines to be plotted 
    year_diff <- lubridate::year(date_start) - year
    
    date_start_mod <- as.character(lubridate::ymd(date_start) - 
                                    lubridate::years(year_diff))
    date_end_mod   <- as.character(lubridate::ymd(date_end) - 
                                    lubridate::years(year_diff))
    date_start_mod_extended <- as.character(lubridate::ymd(date_start_extended) - 
                                             lubridate::years(year_diff))
    
    
    # Pull data for the extended start date (to calculate rolling averages).
    # Since we will not be using and of the SD info, we can make one call
    # unlike the situation earlier in the code
    weather_df_mod_extended <- aWhereCharts::generateaWhereDataset(lat = lat
                                                                   ,lon = lon
                                          ,day_start = date_start_mod_extended
                                          ,day_end = date_end_mod
                                          ,year_start = year_start
                                          ,year_end = year_end)
    
    
    # Save the accumulated precip data needed for charts in "long" format
    temp_data <- data.frame(date = weather_df[, date] 
                            ,data = weather_df_mod_extended[date >= 
                                                      date_start_mod,
                                          cumsum(precipitation.amount)]
                            ,var = "accumulatedPrecipitation.amount"
                            ,year = year) 
    
    # Combine the accumulated precip data with other additional year data
    add_years_df <- rbind(add_years_df, temp_data)                       
    
    # Calculate P/PET rolling average values for chart in "long" format
    temp_ppet <- zoo::rollapply(weather_df_mod_extended$ppet.amount
                                ,width = roll_window
                                ,align = "right"
                                ,FUN = mean
                                ,na.rm = TRUE
                                ,fill = NA)
    
    temp_data <- data.frame(date = weather_df_extended$date, 
                            data = temp_ppet,
                            var = "ppet.amount.rollAvg",
                            year = year) 
    
    # Combine the rolling average P/PET with other additional year data
    add_years_df_extended <- rbind(add_years_df_extended, temp_data) 
    
    # Calculate eP/PET rolling average. first, use the ClipValues function to
    # calculate effective precipitation, then divide eP by PET for eP/PET. 
    weather_df_mod_extended$ePPET <- ClipValues(weather_df_mod_extended$precipitation.amount
                                                ,max.thresh = eP) /
      weather_df_mod_extended$pet.amount
    
    temp_eppet <- zoo::rollapply(weather_df_mod_extended$ePPET
                                 ,width = roll_window
                                 ,align = "right"
                                 ,FUN = mean
                                 ,na.rm = TRUE
                                 ,fill = NA)
    
    temp_data <- data.frame(date = weather_df_extended$date 
                            ,data = temp_eppet
                            ,var = "eppet.amount.rollAv"
                            ,year = year) 
    
    
    # Combine the rolling average eP/PET with other additional year data
    add_years_df_extended <- rbind(add_years_df_extended, temp_data) 
    
    
    # Add the current selected year's accumulated precip and PPET columns 
    # to the main weather dataframe to write to .csv
    weather_df_extended$accumulatedPrecipitation.amount.year <- 
                     weather_df_mod_extended$accumulatedPrecipitation.amount
    weather_df_extended$ppet.amount.year <- 
                     weather_df_mod_extended$ppet.amount
    weather_df_extended$precipitation.amount.year <- 
                      weather_df_mod_extended$precipitation.amount
    weather_df_extended$pet.amount.year <- 
                      weather_df_mod_extended$pet.amount
    
    # rename the columns to have the current year value appended on the end,
    # such as precipitation.amount.year.2016
    data.table::setnames(weather_df_extended 
                         ,c("accumulatedPrecipitation.amount.year")
                         ,c(paste0("accumulatedPrecipitation.amount.year" 
                                  ,year))) 
    data.table::setnames(weather_df_extended
                         ,c("ppet.amount.year")
                         ,c(paste0("ppet.amount.year", year)))
    data.table::setnames(weather_df_extended
                         ,c("precipitation.amount.year")
                         ,c(paste0("precipitation.amount.year", year)))
    data.table::setnames(weather_df_extended
                         ,c("pet.amount.year")
                         ,c(paste0("pet.amount.year", year)))
  }
  

  # Save weather data to .csv ---------------------------------------------
  
  #Calculate Rolling Effective Precipitation
  weather_df_extended$rolling_precip <- 
    zoo::rollapply(ifelse(weather_df_extended$precipitation.amount > eP
                          ,eP
                          ,weather_df_extended$precipitation.amount)
                   ,width = roll_window 
                   ,align = "right"
                   ,FUN = sum
                   ,na.rm = TRUE
                   ,fill = NA)
  
  #Calculate Rolling PET
  weather_df_extended$rolling_pet <- zoo::rollapply(weather_df_extended$pet.amount
                                                    ,width = roll_window
                                                    ,align = "right"
                                                    ,FUN = sum
                                                    ,na.rm = TRUE
                                                    ,fill = NA)
  
  #Calculate Rolling eP/PET
  weather_df_extended$rollingppet <- weather_df_extended$rolling_precip / 
    weather_df_extended$rolling_pet
  
  weather2_df <- as.data.frame(weather_df)
  
  #Add in Rolling eP/PET
  weather2_df <- merge(weather2_df
                       ,weather_df_extended[,c('date','rollingppet')])
  
  names(weather2_df)[grep("gdd.amount", names(weather2_df))] <- "GDD"
  names(weather2_df)[grep("gdd.average", names(weather2_df))] <- "GDD_avg"
  
  names(weather2_df)[grep("accumulatedPpet.amount", names(weather2_df))] <- "accumPPET"
  names(weather2_df)[grep("accumulatedPpet.average", names(weather2_df))] <- "accumPPETavg"
  names(weather2_df)[grep("accumulatedPet.amount", names(weather2_df))] <- "accumPET"
  names(weather2_df)[grep("accumulatedPet.average", names(weather2_df))] <- "accumPETavg"
  names(weather2_df)[grep("ppet.amount", names(weather2_df))] <- "PPET"
  names(weather2_df)[grep("ppet.average", names(weather2_df))] <- "PPETavg"
  names(weather2_df)[grep("pet.amount", names(weather2_df))] <- "PET"
  names(weather2_df)[grep("pet.average", names(weather2_df))] <- "PET_avg"
  names(weather2_df)[grep("temperatures.max.amount", names(weather2_df))] <- "maxTemp"
  names(weather2_df)[grep("temperatures.min.amount", names(weather2_df))] <- "minTemp"
  names(weather2_df)[grep("temperatures.max.average", names(weather2_df))] <- "maxTemp_avg"
  names(weather2_df)[grep("temperatures.min.average", names(weather2_df))] <- "minTemp_avg"
  names(weather2_df)[grep("precipitation.amount", names(weather2_df))] <- "Precip"
  names(weather2_df)[grep("precipitation.average", names(weather2_df))] <- "Precip_avg"
  names(weather2_df)[grep("relativeHumidity.max.amount", names(weather2_df))] <- "RHmax"
  names(weather2_df)[grep("relativeHumidity.min.amount", names(weather2_df))] <- "RHmin"
  names(weather2_df)[grep("relativeHumidity.max.average", names(weather2_df))] <- "RHmax_avg"
  names(weather2_df)[grep("relativeHumidity.min.average", names(weather2_df))] <- "RHmin_avg"
  names(weather2_df)[grep("wind.average.amount", names(weather2_df))] <- "wind"
  names(weather2_df)[grep("wind.average.average", names(weather2_df))] <- "wind_avg"
  names(weather2_df)[grep("wind.dayMax.amount", names(weather2_df))] <- "max_wind"
  names(weather2_df)[grep("wind.dayMax.average", names(weather2_df))] <- "max_wind_avg"
  names(weather2_df)[grep("wind.dayMax", names(weather2_df))] <- "wind_day_max"
  names(weather2_df)[grep("solar.amount", names(weather2_df))] <- "solar"
  names(weather2_df)[grep("solar.average", names(weather2_df))] <- "solar_avg"
  
  # trim decimal points on weather data values...
  weather2_df$max_wind <- round(weather2_df$max_wind, 1)
  weather2_df$max_wind_avg <- round(weather2_df$max_wind_avg, 1)
  weather2_df$RHmax <- round(weather2_df$RHmax, 0)
  weather2_df$RHmax_avg <- round(weather2_df$RHmax_avg, 0)
  weather2_df$RHmin <- round(weather2_df$RHmin, 0)
  weather2_df$RHmin_avg <- round(weather2_df$RHmin_avg, 0)
  weather2_df$maxTemp <- round(weather2_df$maxTemp, 1)
  weather2_df$maxTemp_avg <- round(weather2_df$maxTemp_avg, 1)
  weather2_df$minTemp <- round(weather2_df$minTemp, 1)
  weather2_df$minTemp_avg <- round(weather2_df$minTemp_avg, 1)
  weather2_df$solar <- round(weather2_df$solar, 0)
  weather2_df$solar_avg <- round(weather2_df$solar_avg, 0)
  weather2_df$Precip <- round(weather2_df$Precip, 1)
  weather2_df$Precip_avg <- round(weather2_df$Precip_avg, 1)
  weather2_df$wind <- round(weather2_df$wind, 1)
  weather2_df$wind_avg <- round(weather2_df$wind_avg, 1)
  weather2_df$GDD <- round(weather2_df$GDD, 1)
  weather2_df$GDD_avg <- round(weather2_df$GDD_avg, 1)
  weather2_df$PET <- round(weather2_df$PET, 2)
  weather2_df$PET_avg <- round(weather2_df$PET_avg, 2)
  
  # Reorder data to make it easier to read
  weather2_df <- 
    weather2_df %>% dplyr::select(latitude, longitude, day, date, Precip 
                                  ,Precip_avg, PET_avg, maxTemp,minTemp 
                                  ,RHmax, RHmin, wind, solar, everything())
  weather_df_extended <- 
    weather_df_extended %>%
    dplyr::select(day, date, latitude, longitude, everything())
  
  # write to .csv file 
  utils::write.csv(weather2_df, 
                   file = paste0(current_csv_path,
                                 paste(place_name
                                       ,paste(date_start, date_end, sep="_")
                                       ,paste(year_start, year_end,sep="_")
                                       ,".csv", sep="_")))
  

  # Create charts ---------------------------------------------------------
  # Create a series of weather variable charts for the current location
  
  # Print a status message to the console 
  cat("   Creating Charts...\n")
  
  # Create a variable containing the lat and lon, for plot titles
  lat_lon <- paste0("(", lat, ", ", lon, ")") 
  
  # Weekly climatology chart ----------------------------------------------
  # Only include date from the first day that is specified, 
  # not the extended window for calculating rolling aggregates
  weekly_chart_title <- paste0("Annual Climate Chart for ", place_name) 
  weekly_chart <- aWhereCharts::generateaWhereChart(data = weather_df
                                          ,variable = "precipitation"
                                          ,title = paste(weekly_chart_title
                                                        ,lat_lon
                                                        ,date_start, "to"
                                                        ,date_end)
                                          ,includeSTD = TRUE
                                          ,variable_rightAxis = "maxTemp"
                                          ,daysToAggregateOver = 7
                                           ,mainGraphType = "bar")  

  # Maximum Temperature with Standard Deviation  --------------------------
  # Construct title
  max_temp_stdev_title <- paste0(place_name, ": Maximum Temperature w StdDev")
  
  # Generate the plot 
  max_temp_stdev <- aWhereCharts::generateaWhereStdDevChart(data = weather_df
                                                    ,variable = "maxTemp" 
                                         ,title = paste(max_temp_stdev_title
                                                        ,lat_lon))
  
  # Minimum Temperature with Standard Deviation ---------------------------
  min_temp_stdev_title <- paste0(place_name, ": Minimum Temperature w StdDev")
  
  min_temp_stdev <- aWhereCharts::generateaWhereStdDevChart(data = weather_df
                                                  ,variable = "minTemp"
                                         ,title = paste(min_temp_stdev_title
                                                        ,lat_lon))
  
  # PET with Standard Deviation -------------------------------------------
  pet_stdev_title <- paste0(place_name, ": PET w StdDev")
  
  pet_stdev <- aWhereCharts::generateaWhereStdDevChart(data = weather_df 
                                                       ,variable = "pet"
                                               ,title = paste(pet_stdev_title
                                                              ,lat_lon))

  # Daily Precip with Standard Deviation ----------------------------------
  precip_stdev_title <- paste0(place_name, ": Daily Precipitation w StdDev")
  
  precip_stdev <- aWhereCharts::generateaWhereStdDevChart(data = weather_df
                                            ,variable = "precipitation"
                                            ,title = paste(precip_stdev_title
                                                           ,lat_lon))

  # Daily Precipitation ---------------------------------------------------
  precip_title <- paste0(place_name, ": Daily Precipitation")
  
  precip <- aWhereCharts::generateaWhereChart(data = weather_df
                                      ,variable = "precipitation"    
                                      ,title = paste(precip_title
                                                     ,lat_lon))

  # Accumulated Precipitation with Standard Deviation ---------------------
  acc_precip_stdev_title <- paste0(place_name, 
                             ": Accumulated Precipitation w StdDev")
  
  acc_precip_stdev <- aWhereCharts::generateaWhereStdDevChart(data = weather_df
                                    ,variable = "accumulatedPrecipitation"
                                        ,title = paste(acc_precip_stdev_title
                                                        ,lat_lon))

  # Accumulated Effective Precipitation with Standard Deviation -----------
  acc_precip_eff_stdev_title <- paste0(place_name, 
      ": Precipitation and Effective Precipitation\n Accumulated w Std Dev")
  
  acc_precip_eff_stdev <- 
    aWhereCharts::generateaWhereStdDevChart(data = weather_df 
                                          ,variable = "accumulatedPrecipitation"
                                          ,title = paste(acc_precip_eff_stdev_title
                                                          ,lat_lon)
                                          ,e_precip = TRUE
                                          ,e_threshold = eP)
  
  # Accumulated Precipitation ---------------------------------------------
  acc_precip_title <- paste0(place_name, ": Accumulated Precipitation")
  
  acc_precip <- aWhereCharts::generateaWhereChart(data = weather_df
                                    ,variable = "accumulatedPrecipitation"
                                    ,title = paste(acc_precip_title
                                                    ,lat_lon)
                                    ,includeSTD = TRUE)
  
  # Accumulated Precipitation with Additional Selected Years --------------
  
  # Filter the add_years data frame for the accumulated precip data
  add_years_acc_precip <- add_years_df %>% 
           dplyr::filter(var == "accumulatedPrecipitation.amount")
  
  # Construct the title
  acc_precip_addyears_title <- paste0(place_name 
                                      ,": Accumulated Precipitation\n"
                                      ,"with additional selected years ")
  
  scale_list <- generateColorScale(acc_precip
                                   ,add_years
                                   ,colors_additional)
  
  # Add the additional selected year lines to the acc precip chart
  acc_precip_addyears <- acc_precip + 
    geom_ribbon(data = add_years_acc_precip
                ,aes(x = as.Date(date)
                     ,ymin = data
                     ,ymax = data
                     ,color = as.factor(year)
                     ,fill = as.factor(year))
                ,size = line_width) +
    ggtitle(paste(acc_precip_addyears_title
                   ,lat_lon)) + 
    scale_list$colorScale +
    scale_list$fillScale + 
    xlim(as.Date(date_start), as.Date(date_end)) +
    guides(fill = guide_legend(ncol =  length(add_years) +2))
  
  # Accumulated PET with Standard Deviation -------------------------------
  
  acc_pet_stdev_title <- paste0(place_name, ": Accumulated PET w StdDev")
  
  acc_pet_stdev <- aWhereCharts::generateaWhereStdDevChart(data = weather_df
                                                ,variable = "accumulatedPet"
                                           ,title = paste(acc_pet_stdev_title
                                                          ,lat_lon))
  
  # Daily P/PET -----------------------------------------------------------
  # P/PET = Precipitation-over-PET ratio 
  # (NOTE!!! --> P/PET is rarely is interpretable on a daily chart) 
  ppet_title <- paste0(place_name,": PPET")
  
  ppet <- aWhereCharts::generateaWhereChart(data = weather_df 
                                            ,variable = "ppet" 
                                            ,title = paste(ppet_title
                                                           ,lat_lon))
  
  # Rolling-Average PET and P/PET ----------------------------------
  
  # No eprecip/PET shows up if all rainfall events are less than 
  # the effective precipitation threshold 
  rolling_avg_ppet_title <- 
    paste0(place_name
          ,": ",roll_window," day rolling average \neP PET and P PET ")
  
  # Create a chart of P/PET without effective precip (set e_precip to FALSE).
  # As for the previous chart, use the weather date with "extended" dates
  # for this rolling average calculation. 
  rolling_avg_ppet <- 
    aWhereCharts::generateaWhereChart(data = weather_df_extended
                                      ,variable = "rollingavgppet"
                                      ,title = paste(rolling_avg_ppet_title
                                                     ,lat_lon)
                                      ,e_precip = FALSE
                                      ,rolling_window = roll_window
                                      ,includeSTD = TRUE)
  
  # Rolling-Average PET and P/PET with Additional Selected Years ---
  rolling_avg_ppet_addyears_title <- 
    paste0(place_name
            ,": ",roll_window," day rolling avg P PET \n"
            ,"with additional selected years")
  
  # Filter the add_years data frame for just the rolling average P/PET data
  add_years_avg_ppet <- add_years_df_extended %>% 
    dplyr::filter(var == "ppet.amount.rollAvg")
  
  scale_list <- generateColorScale(rolling_avg_ppet
                                   ,add_years
                                   ,colors_additional)
  
  # Add P/PET lines to the chart for additional selected years.
  # Set the x-axis limits to reflect the specified start and end dates.
  rolling_avg_ppet_addyears <- rolling_avg_ppet +
    geom_ribbon(data = add_years_avg_ppet
              ,aes(x = as.Date(date)
                   ,ymin = data
                   ,ymax = data
                   ,color = as.factor(year)
                   ,fill = as.factor(year))
              ,size = line_width) +
    ggtitle(paste(rolling_avg_ppet_addyears_title
                  ,lat_lon)) + 
    scale_list$colorScale +
    scale_list$fillScale + 
    xlim(as.Date(date_start), as.Date(date_end)) +
    guides(fill = guide_legend(ncol =  length(add_years) +2))
  

  
  # Rolling-Average PET and P/PET using Effective Precipitation ----
  rolling_avg_eppet_title <- 
    paste0(place_name
            ,": ",roll_window," day rolling average eP PET")
  
  rolling_avg_eppet <- 
    aWhereCharts::generateaWhereChart(data = weather_df_extended
                                      ,variable = "rollingavgppet"
                                      ,title = paste(rolling_avg_eppet_title
                                                      ,lat_lon)
                                      ,e_precip = TRUE
                                      ,e_threshold = eP 
                                      ,rolling_window = roll_window
                                      ,includeSTD = TRUE)
  
  
  # Additional selected years 
  rolling_avg_eppet_addyears_title <- 
    paste0(place_name
           ,": ",roll_window," day rolling avg eP PET \n"
           ,"with additional selected years")
  
  # Filter the add.years data frame for just the rolling average P/PET data
  add_years_rollling_avg_eppet <- add_years_df %>% 
    dplyr::filter(var == "eppet.amount.rollAvg")
  
  scale_list <- generateColorScale(rolling_avg_eppet
                                   ,add_years
                                   ,colors_additional)
  
  # add P/PET lines to the chart for additional selected years 
  rolling_avg_eppet_addyears <- rolling_avg_eppet + 
    geom_ribbon(data = add_years_rollling_avg_eppet
                ,aes(x = as.Date(date)
                     ,ymin = data
                     ,ymax = data
                     ,color = as.factor(year)
                     ,fill = as.factor(year))
              ,size = line_width) +
    ggtitle(rolling_avg_eppet_addyears_title) + 
    scale_list$colorScale +
    scale_list$fillScale + 
    xlim(as.Date(date_start), as.Date(date_end)) +
    guides(fill = guide_legend(ncol =  length(add_years) +2))
  
  
  # SHOW CHARTS ------------------------------------------------------------
  # Show all charts if user specified
  
  if (show_charts == TRUE) {
    
    # Print a status message to the console 
    cat('   Showing Charts...\n')
    
    # Weekly climatology chart 
    print(weekly_chart)
    
    # Maximum temperature with standard deviation
    print(max_temp_stdev) 
    
    # Minimum temperature with standard deviation
    print(min_temp_stdev) 
    
    # Potential evapotranspiration (PET) with standard deviation 
    print(pet_stdev)
    
    # Daily precipitation with standard deviation  
    print(precip_stdev) 
    
    # Daily precipitation without standard deviation  
    print(precip) 
    
    # Accumulated precipitation with standard deviation
    print(acc_precip_stdev) 
    
    # Precipitation and effective precipitation, accumulated, with 
    # standard deviation 
    print(acc_precip_eff_stdev)
    
    # Accumulated precipitation 
    print(acc_precip)
    
    # Accumulated precipitation with additional selected years
    print(acc_precip_addyears)
    
    # Accumulated Potential Evapotranspiration (PET) with standard deviation
    print(acc_pet_stdev)
    
    # Precipitation-over-PET ratio (P/PET)
    print(ppet)
    
    # rolling average eP/PET and P/PET 
    print(rolling_avg_ppet)
    
    # rolling average P/PET with additional selected years
    print(rolling_avg_ppet_addyears)
    
    # rolling average eP/PET
    print(rolling_avg_eppet)
    
    # rolling average eP/PET with additional selected years
    print(rolling_avg_eppet_addyears)
    
  } 
  
  # SAVE CHARTS -------------------------------------------------------------
  # Write the charts to image files if  user specified 
  
  if (save_charts == TRUE) {
    
    # Print a status message to the console 
    cat(paste0('   Saving Charts...\n'))
    
    # Maximum temperature with standard deviation
    WriteJpeg(plt = max_temp_stdev
              ,plt.title = paste0(current_chart_path
                          ,formatGraphTitleForFileName(max_temp_stdev_title)))
    
    # Minimum temperature with standard deviation
    WriteJpeg(plt = min_temp_stdev
              ,plt.title = paste0(current_chart_path
                          ,formatGraphTitleForFileName(min_temp_stdev_title)))
    
    # Potential evapotranspiration (PET) with standard deviation 
    WriteJpeg(plt = pet_stdev
              ,plt.title = paste0(current_chart_path
                               ,formatGraphTitleForFileName(pet_stdev_title)))
    
    # Daily precipitation with standard deviation  
    WriteJpeg(plt = precip_stdev
              ,plt.title = paste0(current_chart_path
                            ,formatGraphTitleForFileName(precip_stdev_title)))
    
    # Daily precipitation without standard deviation  
    WriteJpeg(plt = precip
              ,plt.title = paste0(current_chart_path
                                  ,formatGraphTitleForFileName(precip_title)))
    
    # Accumulated precipitation with standard deviation 
    WriteJpeg(plt = acc_precip_stdev
              ,plt.title = paste0(current_chart_path
                        ,formatGraphTitleForFileName(acc_precip_stdev_title)))
    
    # Precipitation and effective precipitation, accumulated, with 
    # standard deviation 
    WriteJpeg(plt = acc_precip_eff_stdev
              ,plt.title = paste0(current_chart_path
                    ,formatGraphTitleForFileName(acc_precip_eff_stdev_title)))
    
    # Accumulated precipitation 
    WriteJpeg(plt = acc_precip
              ,plt.title = paste0(current_chart_path
                              ,formatGraphTitleForFileName(acc_precip_title)))
    
    # Accumulated precipitation with additional selected years
    WriteJpeg(plt = acc_precip_addyears 
              ,plt.title = paste0(current_chart_path
                     ,formatGraphTitleForFileName(acc_precip_addyears_title)))
    
    
    # Accumulated Potential Evapotranspiration (PET) with standard deviation
    WriteJpeg(plt = acc_pet_stdev
              ,plt.title = paste0(current_chart_path
                           ,formatGraphTitleForFileName(acc_pet_stdev_title)))
    
    
    # Precipitation-over-PET ratio (P/PET)
    WriteJpeg(plt = ppet
              ,plt.title = paste0(current_chart_path
                                    ,formatGraphTitleForFileName(ppet_title)))
    
    
    # rolling average eP/PET and P/PET 
    # VS-NOTE assess this chart make sure all variables are plotted 
    WriteJpeg(plt = rolling_avg_ppet
              ,plt.title = paste0(current_chart_path
                        ,formatGraphTitleForFileName(rolling_avg_ppet_title)))
    
    
    # rolling average P/PET with additional selected years
    WriteJpeg(plt = rolling_avg_ppet_addyears 
              ,plt.title = paste0(current_chart_path
               ,formatGraphTitleForFileName(rolling_avg_ppet_addyears_title)))
    
    # rolling average eP/PET
    WriteJpeg(plt = rolling_avg_eppet
              ,plt.title = paste0(current_chart_path
                                  ,formatGraphTitleForFileName(rolling_avg_eppet_title)))
    
    # rolling average eP/PET with additional selected years
    WriteJpeg(plt = rolling_avg_eppet_addyears 
              ,plt.title = paste0(current_chart_path
              ,formatGraphTitleForFileName(rolling_avg_eppet_addyears_title)))
    
    # Weekly climatology chart comparing current precipitation and maximum 
    # temperature to LTN precip and max temperature
    WriteJpeg(plt = weekly_chart
              ,plt.title = paste0(current_chart_path
                ,formatGraphTitleForFileName(paste0(place_name
                                                    ,"weekly_chart"))))
    
    # Multiplot 
    # First, open a jpeg writer
    jpeg(paste0(current_chart_path,
                formatGraphTitleForFileName(place_name),"_4chart.jpeg"), 
         width = 12, height = 6, 
         units = 'in', res = 500)
  
    # Generate the Multiplot 
    aWhereCharts::generateMultiplot(acc_precip_addyears
                                    ,rolling_avg_ppet_addyears
                                    ,max_temp_stdev 
                                    ,pet_stdev 
                                    ,cols = 2
                                    ,fontsize = 8
                                    ,title = paste("Current vs LTN at "
                                                    ,place_name
                                                    ,lat_lon
                                                    ,"\neP ="
                                                   , eP, "mm"))
    # close the current plot object
    invisible(dev.off())
    
  }

}

# Check the "charts" and "outputCSVs" folders in your working directory 
# to see the outputs of this script!
