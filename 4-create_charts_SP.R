#--------------------------------------------------------------------------
# aWhere R Tutorial: Create Charts
# Tutorial de R de aWhere: Creacion de Graficos
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
# Este codigo le mostrara como crear rapidamente series de graficos para sus
# sitios de interes, para ayudarlo al mismo tiempo a entender las condiciones
# meteorologicas/agronomicas de estas ubicaciones haciendo uso de la base de datos
# agrometeorologica de aWhere (esta incluye variables tales como temperatura,
# precipitacion, evapotranspiracion potencial, comparaciones con valores promedio
# a largo plazo, entre otros). Estos graficos pueden ser agregados a reportes para
# realizar un analisis rapido de las condiciones agrometeorologicas en sus sitios de
# interes. Los datos meteorologicos obtenidos para su sitio y periodo de interes tambien
# pueden ser guardados en un archivo .csv para su analisis posterior.
#
# Antes de ejecutar este script, le solicitamos encontrar el nombre, la latitud y la longitud de su
# sitio de interes, ya sea utilizando Google Maps, QGIS o bien, los archivos geoespaciales de
# aWhere disponibles en apps.awhere.com o bien, utilizando puntos GPS que usted haya obtenido previamente.
#
# Este script utiliza un Archivo de Coordenadas que le permite crear el Grafico de la Climatologia,
# para todos sus sitios de interes mediante un proceso denomidando ciclo ("loop"). Se necesita crear un
# archivo de texto con su(s) ubicacion(es) de interes. Para ello, debe de editar el archivo de texto 
# de ejemplo "locations.txt" en la carpeta RunSet para darle formato a este archivo. Este, debe tener 3 columnas 
# con los nombres place_name, latitude, longitude:
#     place_name, latitude, longitude
#     Nairobi, -1.283, 36.816
#     Addis Ababa, 8.980, 38.757
#
# Antes de continuar, por favor dele el formato requerido a su Archivo de Coordenadas. 
#
# Usted necesita estar conectado a internet para ejecutar este codigo.
#
# Date updated: 2020-04-15
# Fecha de actualización: 2020-04-15
#--------------------------------------------------------------------------

# Install and load packages -----------------------------------------------
# Instalar y cargar paquetes ----------------------------------------------

# Clear your environment and remove all previous variables
# Limpie su entorno y remueva todas las variables previas
rm(list = ls())

# Load the packages needed for this script
# Cargue los paquetes necesarios para este script
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(aWhereAPI)
library(aWhereCharts)


# Load aWhere credentials -------------------------------------------------
# Cargue sus credenciales de aWhere ---------------------------------------

# You will need to load your credentials file which includes your aWhere 
# key and secret, like a username and password. This gives you a token which 
# shows that you have access to the API and all of aWhere's data. Your 
# credentials should be kept in a location where you can easily find them. 
# Copy the pathfile name and paste it below over the phrase, 
# "YOUR CREDENTIALS HERE"

# Es necesario cargar su Archivo de Credenciales que incluye su Key & Secret
# de aWhere, similar a un nombre de usuario y contraseña. Esto le brinda una
# especie de "Token" que muestra que usted tiene acceso al API y a todos los datos de aWhere.
# Debe de mantener sus credenciales en una ubicacion que pueda hallar facilmente.
# Copie la ruta de acceso de este archivo y pegue esta sobre la frase "YOUR CREDENTIALS HERE"

aWhereAPI::load_credentials("YOUR CREDENTIALS HERE")


# Set working & output directories ----------------------------------------
# Establezca sus directorios de trabajo y de salidas ----------------------

# The working directory is the location on your computer where R will 
# automatically save the output files created by this script.

# To set your working directory, find the folder on your computer where you 
# would like the outputs of this script to be saved, copy the pathfile name 
# and paste it over the phrase, "YOUR WD HERE"

# A continuacion, usted necesita establecer su directorio de trabajo. Este es el lugar
# en su computadora en el que R va a guardar los archivos de salida de este script.

# Para establecer su directorio de trabajo, busque la carpeta en su computadora en la cual
# usted desea que las salidas de este script se guarden, copie la ruta de acceso y pegue
# esta sobre la frase "YOUR WD HERE"

working_dir <- "YOUR WD HERE" 
setwd(working_dir) # This sets your working directory to the working_dir path
                   # Esto establece su directorio de trabajo en la ruta de acceso de working_dir

# Now you will create the folders within your working directory where your 
# output files will be saved. These lines of code create 3 folders in your 
# working directory called outputCSVs and charts. You can navigate 
# to your working directory on your computer and see that these folders 
# were created.

# Ahora usted va a crear las carpetas dentro de su directorio de trabajo en la que
# sus archivos de salida se van a guardar. La siguiente linea creara tres carpetas
# en su directorio de trabajo llamada outputCSVs y charts.Puede navegar a su directorio de trabajo
# en su computadora para verificar que estas carpetas fueron creada.

csv_path <- "outputCSVs/"
chart_path <- "charts/"
dir.create(path = csv_path, showWarnings = FALSE, recursive = TRUE) 
dir.create(path = chart_path, showWarnings = FALSE, recursive = TRUE)

# In the following two, you can specify whether you want to show the 
# charts in the bottom right console, and/or save the charts 
# within the output folders created previously. 

# En las siguientes dos lineas, usted puede especificar si usted quiere
# que los graficos se muestren en la consola inferior derecha, y/o guardar
# los graficos dentro de las carpetas de salida que usted creo previamente.
show_charts <- TRUE # TRUE will show charts. FALSE will not show charts. 
save_charts <- TRUE # TRUE will save charts. FALSE will not save charts. 


# Supporting functions ----------------------------------------------------
# Supporting functions ----------------------------------------------------

# This script requires you to load a supporting functions file that is 
# typically saved in the Source folder in your aWhere tutorial folder 
# structure. This step loads additional functions required to create
# the climatology chart. 

# Este script requiere que usted carge el archivo de "supporting functions" que
# comunmente debe de estar guardado en la carpeta Source, a partir del tutorial de
# aWhere referente a la estructura de carpetas y archivos. Este paso, carga funciones
# adicionales para crear el Grafico de la Climatologia.

# Modify the file path and name below to load the supporting functions file. 
# source("YOUR PATHNAME/supporting_functions.R")

# Modifique el nombre y la ruta de acceso del archivo para cargar el archivo de "supporting functions".
source("Source/supporting_functions.R")


# Location(s) of interest -------------------------------------------------
# Sitio(s) de interes -----------------------------------------------------

# In this section, we will pull forecast data for your location of interest. 
# If using a file named "locations.txt" in the RunSet folder: 

# En esta seccion, vamos a obtener los datos del pronostico para su sitio de interes.
# Si usted esta utilizando el archivo "locations.txt" en la carpeta RunSet ejecute lo siguiente:

locations_file <- "RunSet/locations.txt" 
# Change the path and filename above as needed based on you locations file.
# Cambie el nombre y la ruta de acceso en la linea de arriba segun sea el caso de su Archivo de Coordenadas.

# Read the location(s) text file
# Lea su Archivo de Coordenadas
locations <- read.csv(locations_file)


# Time period of interest -------------------------------------------------
# Periodo de interes ------------------------------------------------------

# Specify the starting and ending dates of interest. 
# The starting date year can be as early as 2008. 
# The ending date can include forecast data up to 7 days from now. 
# You may provide these starting and ending dates in multiple formats.  
# You can use the "YYYY-MM-DD" format for a specific YEAR-MONTH-DAY date.
# Or, you can also choose dates relative to the current date recorded 
# by your computer. For example:   
#   day_end <- as.character(Sys.Date())     # Today
#   day_end <- as.character(Sys.Date() + 7) # Forecast 7 days from now


# Especifique la fecha de inicio y fin segun su interes.
# La fecha de inicio puede iniciar a partir del año 2008.
# La fecha final puede incluir hasta el pronostico a 7 dias a partir de hoy.
# Debe proveer ambas fechas en multiples formatos.
# Puede utilizar el formato "AAAA-MM-DD" para una fecha AÑO-MES-DIA específica.
# Tambien, puede escoger fechas relativas a la almacenada como la fecha actual
# en su computadora. Por ejemplo: 
#   day_end <- as.character(Sys.Date())     # Hoy
#   day_end <- as.character(Sys.Date() + 7) # Pronostico a 7 dias a partir de ahora


date_start <- "2018-05-15"  # CHANGE TO THE STARTING DATE OF YOUR CHOICE 
                            # PUEDE CAMBIAR ESTO A LA FECHA DE INICIO DE SU ELECCION 

date_end <- "2019-05-14"    # CHANGE TO THE ENDING DATE OF YOUR CHOICE
                            # PUEDE CAMBIAR ESTO A LA FECHA FINAL DE SU ELECCION

# Long-term normal (LTN) values will be calculated across this range of years.
# Los valores promedio a largo plazo (LTN) se calcularan a partir de un rango de años.

year_start <- 2008  # Starting year can be as early as 2008
                    # El año inicial tiene que ser igual o mayor al 2008.
year_end <- 2018

# Optional: Add additional selected year(s) to charts. 
# This will plot the data from the specified years explicitly. 
#   add_years <- NA             # If you do not want to add additional years. 
#   add_years <- c(2016, 2017)  # To specify multiple additional years. 

# Opcional: Puede añadir año(s) seleccionados previamente a los gráficos
# Estas lineas van a graficar los datos para años especificos de forma explicita.
#   add_years <- NA             # Si usted no quiere agregar años adicionales. 
#   add_years <- c(2016, 2017)  # Si desea agregar varios años especificos. 
add_years <- c(2016, 2017) 



# Additional chart parameters ---------------------------------------------
# (No need to modify these defaults)

# Parametros adicionales de los graficos ----------------------------------
# (No necesita modificar estas configuraciones)

# Do not change: effective precip amount 
# (cap to large precipitation events so runoff is not included)

# No cambie: cantidad efectiva de precipitacion
# (Limita los eventos duraderos de precipitacion de modo que no incluye la escorrentia)
eP <- 30    

# Size of rolling aggregates to calculate when smoothing
# Tamaño de los agregados dinamico para suavizar los calculos
roll_window <- 20

# Adjust the start date to be "roll.avg" days earlier
# Ajuste a la fecha de inicio para venga dada por "roll.avg" - dias previos
date_start_extended <- as.character(as.Date(date_start) - roll_window)

# Set colors for the charts:
# Estableciendo los colores de los graficos:

# List of original colors for current and LTN lines
# Lista de colores originales para las lineas de valores actuales y LTN

    #colors_orig <- c("#1F83B4", "#FF810E") # blue, orange from original charts
    #colors_orig <- c("#1F83B4", "#FF810E")# azul, naranja para los graficos originales
colors_orig <- c("#4575b4", "#fdae61") # blue, orange from new Colorbrewer palette
                                       # azul, naranja para la nueva paleta de colores.

# list of unique colors for additional lines added 
# lista de colores unicos para las lineas adicionales agregadas
    #colors_additional <- c("black", "red", "yellow", "purple")
    #colors_additional <- c("negro", "rojo", "amarillo", "morado")

colors_additional <- c("#000000", "#d73027", "#fee090", "#abd9e9") 
                        #black     #red       #yellow    #light blue
                        #negro     #rojo      #amarillo  #azul claro

# Just take the number of line colors that are needed for the final chart.
# Since additional years will be plotted first, put those colors first.
# Then Current and LTN colors. 

# Solo escoja el numero de los colores de las lineas que son necesarios para el grafico final.
# Como los años adicionales se van a graficar primero, coloque esos colores de primero.
# Posteriormente coloque los colores de las series actuales y LTN.

colors_final <- c(colors_additional[1:length(add_years)],colors_orig)

# Set the width of the lines on the chart
# Establezca el ancho de las lineas del grafico
line_width <- 1


# Create charts  ----------------------------------------------------------
# Cree los graficos -------------------------------------------------------

# The following is called a loop process - you only need to run the line 
# that starts with "for", and the subsequent lines will automatically run 
# until the end of the script. A set of charts will be generated 
# for each location in your locations text file. Weather data will be written
# to .csv files. The outputs will be saved within your working directory.

# Las siguientes lineas se denominan un loop (ciclo) - usted solo necesita ejecutar
# la linea que comienza con un "for" y las lineas siguientes se ejecutaran automaticamente
# hasta el final del script. Se generara una serie de Graficos para cada ubicacion de
# su Archivo de Coordenadas. Los datos climatologicos se escribiran en archivos .csv.
# Estas salidas se guardaran automaticamente en su directorio de trabajo.


for (i in 1:nrow(locations)) {
  
  # Get the latitude, longitude, and name of the current location
  # Obtenga primero la latitud, longitud y nombre de la ubicacion actual
  lat <- locations$latitude[i]
  lon <- locations$longitude[i]
  place_name <- locations$place_name[i]
  
  # Print a status message about the current location
  # Imprima un mensaje acerca del estado de la ubicacion actual
  print(paste0("Creating charts for location ",
               "(", i, "/", nrow(locations), "): ", place_name))
  
  # Create directories to store outputs for the current location 
  # Cree directorios para guardar las salidas para su ubicacion actual
  current_csv_path <- paste0(csv_path
                             ,formatGraphTitleForFileName(place_name), "/")
  current_chart_path <- paste0(chart_path
                               ,formatGraphTitleForFileName(place_name), "/")
  dir.create(path = current_csv_path, showWarnings = FALSE, recursive = TRUE)
  dir.create(path = current_chart_path, showWarnings = FALSE, recursive = TRUE)
  
  # Pull the weather data directly from the aWhere API
  # Obtenga los datos meteorologicos directamente desde la API de aWhere
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
  
  # Obtenga las series de datos meteorologicos para un rango extendido de tiempo,
  # que incluye una fecha de inicio anterior, basada en la cantidad "roll_avg" (utilizando
  # la variable roll_window para determinar su longitud). Es necesario que usted ejecute estas
  # lineas y las anteriores, en lugar de ejecutar solo la siguiente y establecer la corrida en torno
  # a la fecha actual, porque sino la informacion de la Desviacion Estandar para las columnas acumuladas,
  # no se relacionara de forma lineal con cada conjunto de datos.
  weather_df_extended <- aWhereCharts::generateaWhereDataset(lat = lat
                                         ,lon = lon
                                        ,day_start = date_start_extended
                                        ,day_end = date_end
                                        ,year_start = year_start
                                        ,year_end = year_end)
  
  # Additional years --------------------------------------------------------
  # Generate Data for Plotting Specific Years
  
  # Años adicionales
  # Genere datos para graficar años especificos
  
  # create a data frame to store additional selected year data.
  # the "long" format of this data frame is ideal for plotting with ggplot.
  
  # cree un "data frame" para almacenar años adicionales seleccionados.
  # el formato "largo" de este "data frame" es ideal para graficar utilizando la funcion ggplot.
  add_years_df <- data.frame(date = character(),
                             data = double(),
                             var = character(),
                             year = double())
  
  add_years_df_extended <- data.frame(date = character(),
                                      data = double(),
                                      var = character(),
                                      year = double())
  
  # Pull the data sets for additional selected year(s)
  # Obtenga el conjunto de datos para el(los) año(s) adicional(es) seleccionado(s)
  for (year in add_years) {
    
    # If there are no additional selected years to add, break out of loop
    # Si no hay años adicionales que agregar, corte el ciclo
    if(is.na(year)) {
      break
    }
    
    # Print a status message to the console
    # Imprima un mensaje de estado en la consola
    cat(paste0("\n    Pulling weather data for selected year: ", year, "\n"))
    
    # Calculate dates to use for data pull of additional lines to be plotted
    # Calcule las fechas a utilizar para obtener los datos de las lineas adicionales por graficar
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
    
    # Obtenga los datos para la fecha de inicio extendida (para calcular los promedios dinamicos).
    # Como no vamos a utilizar la informacion de la desviacion estandar, solo podremos hacer
    # una solicitud, a diferencia de lo realizado previamente en el codigo.
    weather_df_mod_extended <- aWhereCharts::generateaWhereDataset(lat = lat
                                                                   ,lon = lon
                                          ,day_start = date_start_mod_extended
                                          ,day_end = date_end_mod
                                          ,year_start = year_start
                                          ,year_end = year_end)
    
    
    # Save the accumulated precip data needed for charts in "long" format
    # Guarde los datos de precipitacion acumulada necesarios para los graficos en formato "largo"
    temp_data <- data.frame(date = weather_df[, date] 
                            ,data = weather_df_mod_extended[date >= 
                                                      date_start_mod,
                                          cumsum(precipitation.amount)]
                            ,var = "accumulatedPrecipitation.amount"
                            ,year = year) 
    
    # Combine the accumulated precip data with other additional year data
    # Combine los datos de precipitacion acumulada con algun otro año adicional
    add_years_df <- rbind(add_years_df, temp_data)                       
    
    # Calculate P/PET rolling average values for chart in "long" format
    # Calcule los valores del promedio dinamico del indice P/PET para los graficos en formato "largo"
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
    # Combine los promedios dinamicos de P/PET con algun otro año adicional
    add_years_df_extended <- rbind(add_years_df_extended, temp_data) 
    
    # Calculate eP/PET rolling average. First, use the ClipValues function to
    # calculate effective precipitation, then divide eP by PET for eP/PET. 
    
    # Calcule el promedio dinamico eP/PET. Primero, utilice la funcion ClipValues para
    # calcular la precipitacion efectiva, luego divida eP por PET para obtener eP/PET
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
                            ,var = "eppet.amount.rollAvg"
                            ,year = year) 
    
    
    # Combine the rolling average eP/PET with other additional year data
    # Combine el promedio dinamico del indice eP/PET con algun otro año adicional
    add_years_df_extended <- rbind(add_years_df_extended, temp_data) 
    
    
    # Add the current selected year's accumulated precip and PPET columns 
    # to the main weather dataframe to write to .csv
    
    # Agregue las columnas de los  valores de precipitacion acumulada actual y PPET de los 
    # años seleccionados al "dataframe" principal de datos meteorologicos para escribir un archivo .csv
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
    
    # cambie el nombre de las columnas para tener el valor del año actual agregado al final,
    # de la forma precipitation.amount.year.2016
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
  # Guarde los datos meteorologicos en un archivo .csv --------------------
  
  # Calculate Rolling Effective Precipitation
  # Calcule la Precipitacion Efectiva Dinamica
  weather_df_extended$rolling_precip <- 
    zoo::rollapply(ifelse(weather_df_extended$precipitation.amount > eP
                          ,eP
                          ,weather_df_extended$precipitation.amount)
                   ,width = roll_window 
                   ,align = "right"
                   ,FUN = sum
                   ,na.rm = TRUE
                   ,fill = NA)
  
  # Calculate Rolling PET
  # Calcule la PET Dinamica
  weather_df_extended$rolling_pet <- zoo::rollapply(weather_df_extended$pet.amount
                                                    ,width = roll_window
                                                    ,align = "right"
                                                    ,FUN = sum
                                                    ,na.rm = TRUE
                                                    ,fill = NA)
  
  # Calculate Rolling eP/PET
  # Calcule el Indice Dinamico eP/PET
  weather_df_extended$rollingppet <- weather_df_extended$rolling_precip / 
    weather_df_extended$rolling_pet
  
  weather2_df <- as.data.frame(weather_df)
  
  # Add in Rolling eP/PET
  # Agregue en el Indice Dinamico eP/PET
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
  # Redondee o recorte decimales en los valores de datos meteorologicos
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
  # Reorganice los datos para que sean mas faciles de leer
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
  # Creacion de graficos --------------------------------------------------
  
  # Create a series of weather variable charts for the current location
  # Cree una serie de graficos de las variables meteorologicas para su ubicacion actual
  
  # Print a status message to the console 
  # Imprima un mensaje de estado en la consola
  cat("   Creating Charts...\n")
  
  # Create a variable containing the lat and lon, for plot titles
  # Cree una variable que contenga la lat y lon, para graficar los titulos
  lat_lon <- paste0("(", lat, ", ", lon, ")") 
  
  # Weekly climatology chart ----------------------------------------------
  # Grafico climatologico semanal -----------------------------------------
  
  # Only include date from the first day that is specified, 
  # not the extended window for calculating rolling aggregates
  
  # Solo incluya la fecha del primer dia que se especifica,
  # no la ventana extendida de tiempo para calcular los agregados dinamicos
  weekly_chart_title <- paste0("Weekly Climate Chart for ", place_name) 
  weekly_chart <- aWhereCharts::generateaWhereChart(data = weather_df
                                          ,variable = "precipitation"
                                          ,title = paste(weekly_chart_title
                                                        ,lat_lon
                                                        ,date_start, "to"
                                                        ,date_end)
                                          #,includeSTD = TRUE
                                          ,variable_rightAxis = "maxTemp"
                                          ,daysToAggregateOver = 7
                                           ,mainGraphType = "bar")  

  # Maximum Temperature with Standard Deviation  --------------------------
  # Temperatura Maxima junto con su Desviacion Estandar -------------------
  
  # Construct title
  # Construya el titulo
  max_temp_stdev_title <- paste0(place_name, ": Maximum Temperature w StdDev")
  
  # Generate the plot 
  # Genere el grafico
  max_temp_stdev <- aWhereCharts::generateaWhereStdDevChart(data = weather_df
                                                    ,variable = "maxTemp" 
                                         ,title = paste(max_temp_stdev_title
                                                        ,lat_lon
                                                        ,date_start, "to"
                                                        ,date_end))
  
  # Minimum Temperature with Standard Deviation ---------------------------
  # Temperatura Minima junto con su Desviacion Estandar -------------------
  min_temp_stdev_title <- paste0(place_name, ": Minimum Temperature w StdDev")
  
  min_temp_stdev <- aWhereCharts::generateaWhereStdDevChart(data = weather_df
                                                  ,variable = "minTemp"
                                         ,title = paste(min_temp_stdev_title
                                                        ,lat_lon
                                                        ,date_start, "to"
                                                        ,date_end))
  
  # PET with Standard Deviation -------------------------------------------
  # PET junto con su Desviacion Estandar ----------------------------------
  pet_stdev_title <- paste0(place_name, ": PET w StdDev")
  
  pet_stdev <- aWhereCharts::generateaWhereStdDevChart(data = weather_df 
                                                       ,variable = "pet"
                                               ,title = paste(pet_stdev_title
                                                              ,lat_lon
                                                              ,date_start, "to"
                                                              ,date_end))

  # Daily Precipitation ---------------------------------------------------
  # Precipitacion Diaria --------------------------------------------------
  precip_title <- paste0(place_name, ": Daily Precipitation")
  
  precip <- aWhereCharts::generateaWhereChart(data = weather_df
                                      ,variable = "precipitation"    
                                      ,title = paste(precip_title
                                                     ,lat_lon
                                                     ,date_start, "to"
                                                     ,date_end)
                                      ,mainGraphType = "bar")

  # Accumulated Precipitation with Standard Deviation ---------------------
  # Precipitacion Acumulada junto con su Desviacion Estandar --------------
  acc_precip_stdev_title <- paste0(place_name, 
                             ": Accumulated Precipitation w StdDev")
  
  acc_precip_stdev <- aWhereCharts::generateaWhereStdDevChart(data = weather_df
                                    ,variable = "accumulatedPrecipitation"
                                        ,title = paste(acc_precip_stdev_title
                                                       ,lat_lon
                                                       ,date_start, "to"
                                                       ,date_end))

  # Accumulated Effective Precipitation with Standard Deviation -----------
  # Precipitacion Efectiva Acumulada junto con su Desviacion Estandar -----
  acc_precip_eff_stdev_title <- paste0(place_name, 
      ": Precipitation and Effective Precipitation\nAccumulated w Std Dev")
  
  acc_precip_eff_stdev <- 
    aWhereCharts::generateaWhereStdDevChart(data = weather_df 
                                          ,variable = "accumulatedPrecipitation"
                                          ,title = paste(acc_precip_eff_stdev_title
                                                         ,lat_lon
                                                         ,date_start, "to"
                                                         ,date_end)
                                          ,e_precip = TRUE
                                          ,e_threshold = eP)
  
  # Accumulated Precipitation ---------------------------------------------
  # Precipitacion Acumulada -----------------------------------------------
  acc_precip_title <- paste0(place_name, ": Accumulated Precipitation")
  
  acc_precip <- aWhereCharts::generateaWhereChart(data = weather_df
                                    ,variable = "accumulatedPrecipitation"
                                    ,title = paste(acc_precip_title
                                                    ,lat_lon
                                                    ,date_start, "to"
                                                    ,date_end))
  
  # Accumulated Precipitation with Additional Selected Years --------------
  # Precipitacion Acumulada junto con Años Adicionales Seleccionados ------
  
  # Filter the add_years data frame for the accumulated precip data
  # Filtre el data frame "add_years" para los datos de precipitacion acumulada
  add_years_acc_precip <- add_years_df %>% 
           dplyr::filter(var == "accumulatedPrecipitation.amount")
  
  # Construct the title
  # Construya el titulo
  acc_precip_addyears_title <- paste0(place_name 
                                      ,": Accumulated Precipitation\n"
                                      ,"with additional selected years ")
  
  scale_list <- generateColorScale(acc_precip
                                   ,add_years
                                   ,colors_additional)
  
  # Add the additional selected year lines to the acc precip chart
  # Agregue las lineas para los años adicionales seleccionados al grafico de precipitacion acumulada
  acc_precip_addyears <- acc_precip + 
    geom_ribbon(data = add_years_acc_precip
                ,aes(x = as.Date(date)
                     ,ymin = data
                     ,ymax = data
                     ,color = as.factor(year)
                     ,fill = as.factor(year))
                ,size = line_width) +
    ggtitle(paste(acc_precip_addyears_title
                  ,lat_lon
                  ,date_start, "to"
                  ,date_end)) + 
    scale_list$colorScale +
    scale_list$fillScale + 
    xlim(as.Date(date_start), as.Date(date_end)) +
    guides(fill = guide_legend(ncol =  length(add_years) +2))
  
  # Accumulated PET with Standard Deviation -------------------------------
  # PET Acumulada junto con su Desviacion Estandar ------------------------
  
  acc_pet_stdev_title <- paste0(place_name, ": Accumulated PET w StdDev")
  
  acc_pet_stdev <- aWhereCharts::generateaWhereStdDevChart(data = weather_df
                                                ,variable = "accumulatedPet"
                                           ,title = paste(acc_pet_stdev_title
                                                          ,lat_lon
                                                          ,date_start, "to"
                                                          ,date_end))
  
  # Daily P/PET -----------------------------------------------------------
  # P/PET Diaria ----------------------------------------------------------
  
  # P/PET = Precipitation-over-PET ratio 
  # P/PET = Razon entre la Precipitacion y la PET
  
  # (NOTE!!! --> P/PET is rarely is interpretable on a daily chart) 
  # (NOTA!!! --> El indice P/PET es extraño de interpretar en un grafico de escala diaria)
  
  ppet_title <- paste0(place_name,": P PET")
  
  ppet <- aWhereCharts::generateaWhereChart(data = weather_df 
                                            ,variable = "ppet" 
                                            ,title = paste(ppet_title
                                                           ,lat_lon
                                                           ,date_start, "to"
                                                           ,date_end))
  
  # Rolling-Average PET and P/PET -----------------------------------------
  # Promedio Dinamico de la PET y el P/PET
  
  # No eprecip/PET shows up if all rainfall events are less than 
  # the effective precipitation threshold 
  
  # El indice eprecip/PET NO se muestra si los eventos de lluvia son menores
  # al umbral de precipitacion efectiva
  rolling_avg_ppet_title <- 
    paste0(place_name
          ,": ",roll_window," day rolling average \nP PET ")
  
  # Create a chart of P/PET without effective precip (set e_precip to FALSE).
  # As for the previous chart, use the weather date with "extended" dates
  # for this rolling average calculation. 
  
  # Cree un grafico de la P/PET sin la precipitacion efectiva (establezca el valor de e_precip como FALSE).
  # Asi como lo hizo en el grafico anterior, utilice los datos meteorologicos en fechas "extendidas"
  # para este calculo de los promedios dinamicos.
  
  rolling_avg_ppet <- 
    aWhereCharts::generateaWhereChart(data = weather_df_extended
                                      ,variable = "rollingavgppet"
                                      ,title = paste(rolling_avg_ppet_title
                                                     ,lat_lon
                                                     ,date_start, "to"
                                                     ,date_end)
                                      ,e_precip = FALSE
                                      ,rolling_window = roll_window)
  
  # Rolling-Average P/PET with Additional Selected Years -----------------
  # Promedio Dinamico de la P/PET con Años Adicionales Seleccionados------
  rolling_avg_ppet_addyears_title <- 
    paste0(place_name
            ,": ",roll_window," day rolling avg P PET \n"
            ,"with additional selected years")
  
  # Filter the add_years data frame for just the rolling average P/PET data
  # Filtre el "dataframe" "add_years" para su uso solo en el promedio dinamico de los datos de P/PET
  add_years_avg_ppet <- add_years_df_extended %>% 
    dplyr::filter(var == "ppet.amount.rollAvg")
  
  scale_list <- generateColorScale(rolling_avg_ppet
                                   ,add_years
                                   ,colors_additional)
  
  # Add P/PET lines to the chart for additional selected years.
  # Set the x-axis limits to reflect the specified start and end dates.
  
  # Agregue las lineas de P/PET para los años adicionales seleccionados al grafico.
  # Establezca los limites del eje x de modo que se perciban las fechas de inicio y fin.
  rolling_avg_ppet_addyears <- rolling_avg_ppet +
    geom_ribbon(data = add_years_avg_ppet
              ,aes(x = as.Date(date)
                   ,ymin = data
                   ,ymax = data
                   ,color = as.factor(year)
                   ,fill = as.factor(year))
              ,size = line_width) +
    ggtitle(paste(rolling_avg_ppet_addyears_title
                  ,lat_lon
                  ,date_start, "to"
                  ,date_end)) + 
    scale_list$colorScale +
    scale_list$fillScale + 
    xlim(as.Date(date_start), as.Date(date_end)) +
    guides(fill = guide_legend(ncol =  length(add_years) +2))
  

  # Rolling-Average eP/PET and P/PET w standard deviation ---------------------------
  # Promedio dinamico del indice eP/PET y P/PET junto con su Desviacion Estandar-----
  rolling_avg_eppet_title <- 
    paste0(place_name
           ,": ",roll_window," day rolling average eP PET and P PET \nw Std Dev")
  
  rolling_avg_eppet <- 
    aWhereCharts::generateaWhereChart(data = weather_df_extended
                                      ,variable = "rollingavgppet"
                                      ,title = paste(rolling_avg_eppet_title
                                                     ,lat_lon
                                                     ,date_start, "to"
                                                     ,date_end)
                                      ,e_precip = TRUE
                                      ,e_threshold = eP 
                                      ,rolling_window = roll_window
                                      ,includeSTD = TRUE)
  
  # Rolling-Average P/PET with additional selected years ------------------
  # Promedio dinamico de la P/PETcon años adicionales seleccionados -------
  rolling_avg_ppet_addyears_title <- 
    paste0(place_name
           ,": ",roll_window," day rolling avg P PET \n"
           ,"with additional selected years")
  
  rolling_avg_ppet <- 
    aWhereCharts::generateaWhereChart(data = weather_df_extended
                                      ,variable = "rollingavgppet"
                                      ,title = paste(rolling_avg_ppet_addyears_title
                                                     ,lat_lon
                                                     ,date_start, "to"
                                                     ,date_end)
                                      ,rolling_window = roll_window)
  # Additional selected years
  # Años adicionales seleccionados
  
  # Filter the add.years data frame for just the rolling average P/PET data
  # Filtre el "dataframe" add.years para su uso solo con los datos dinamicos de la P/PET
  add_years_rolling_avg_ppet <- add_years_df_extended %>% 
    dplyr::filter(var == "eppet.amount.rollAvg")
  
  scale_list <- generateColorScale(rolling_avg_ppet
                                   ,add_years
                                   ,colors_additional)
  
  # add P/PET lines to the chart for additional selected years
  # agregue las lineas de P/PET para los años adicionales seleccionados al grafico
  rolling_avg_ppet_addyears <- rolling_avg_ppet + 
    geom_ribbon(data = add_years_rolling_avg_ppet
                ,aes(x = as.Date(date)
                     ,ymin = data
                     ,ymax = data
                     ,color = as.factor(year)
                     ,fill = as.factor(year))
              ,size = line_width) +
    ggtitle(paste(rolling_avg_ppet_addyears_title,
                  lat_lon, date_start, "to",date_end)) + 
    scale_list$colorScale +
    scale_list$fillScale + 
    xlim(as.Date(date_start), as.Date(date_end)) +
    guides(fill = guide_legend(ncol =  length(add_years) +2))
  
  
  # SHOW CHARTS ------------------------------------------------------------
  # VISUALICE LOS GRAFICOS -------------------------------------------------
  
  # Show all charts if user specified
  # Muestre todos los graficos si asi lo especifica el usuario
  
  if (show_charts == TRUE) {
    
    # Print a status message to the console 
    # Imprima un mensaje de estado en la consola
    cat('   Showing Charts...\n')
    
    # Weekly climatology chart
    # Grafico climatologico semanal
    print(weekly_chart)
    
    # Maximum temperature with standard deviation
    # Temperatura maxima con desviacion estandar
    print(max_temp_stdev) 
    
    # Minimum temperature with standard deviation
    # Temperatura minima con desviacion estandar
    print(min_temp_stdev) 
    
    # Potential evapotranspiration (PET) with standard deviation 
    # Evaportranspiracion potencial (PET) con desviacion estandar
    print(pet_stdev)
    
    # Daily precipitation without standard deviation  
    # Precipitacion diaria sin desviacion estandar
    print(precip) 
    
    # Accumulated precipitation with standard deviation
    # Precipitacion acumulada con desviacion estandar
    print(acc_precip_stdev) 
    
    # Precipitation and effective precipitation, accumulated, with 
    # standard deviation
    # Precipitacion y precipitacion efectiva, acumulada, con desivaicion estandar
    print(acc_precip_eff_stdev)
    
    # Accumulated precipitation 
    # Precipitacion acumulada
    print(acc_precip)
    
    # Accumulated precipitation with additional selected years
    # Precipitacion acumulada con años adicionales seleccionados
    print(acc_precip_addyears)
    
    # Accumulated Potential Evapotranspiration (PET) with standard deviation
    # Evapotranspiracion Potencial (PET) acumulada con desviacion estandar
    print(acc_pet_stdev)
    
    # Precipitation-over-PET ratio (P/PET)
    # Razon de la Precipitacion sobre PET (P/PET)
    print(ppet)
    
    # rolling average P/PET 
    # promedio dinamico de la P/PET
    print(rolling_avg_ppet)
    
    # rolling average P/PET with additional selected years
    # promedio dinamico de la P/PET con años adicionales seleccionados
    print(rolling_avg_ppet_addyears)
    
    # rolling average eP/PET and P/PET w std dev
    # promedio dinamico de la eP/PET y P/PET con desviacion estandar
    print(rolling_avg_eppet)
    
  } 
  
  # SAVE CHARTS -------------------------------------------------------------
  # GUARDE LOS GRAFICOS -----------------------------------------------------
  
  # Write the charts to image files if  user specified 
  # Guarde los graficos como archivos de imagen si el usuario lo especifica
  
  if (save_charts == TRUE) {
    
    # Print a status message to the console
    # Imprima un mensaje de stado en la consola
    cat(paste0('   Saving Charts...\n'))
    
    # Maximum temperature with standard deviation
    # Temperatura Maxima con desviacion estandar
    WriteJpeg(plt = max_temp_stdev
              ,plt.title = paste0(current_chart_path
                          ,formatGraphTitleForFileName(max_temp_stdev_title)))
    
    # Minimum temperature with standard deviation
    # Temperatura Minima con desviacion estandar
    WriteJpeg(plt = min_temp_stdev
              ,plt.title = paste0(current_chart_path
                          ,formatGraphTitleForFileName(min_temp_stdev_title)))
    
    # Potential evapotranspiration (PET) with standard deviation 
    # Evapotranspiracion potencial (PET) con desviacion estandar
    WriteJpeg(plt = pet_stdev
              ,plt.title = paste0(current_chart_path
                               ,formatGraphTitleForFileName(pet_stdev_title)))
    
    # Daily precipitation without standard deviation  
    # Precipitacion diaria sin desviacion estandar
    WriteJpeg(plt = precip
              ,plt.title = paste0(current_chart_path
                                  ,formatGraphTitleForFileName(precip_title)))
    
    # Accumulated precipitation with standard deviation 
    # Precipitacion acumulada sin desviacion estandar
    WriteJpeg(plt = acc_precip_stdev
              ,plt.title = paste0(current_chart_path
                        ,formatGraphTitleForFileName(acc_precip_stdev_title)))
    
    # Precipitation and effective precipitation, accumulated, with 
    # standard deviation 
    # Precipitacion y precipitacion efectiva, acumulada, con desviacion estandar
    WriteJpeg(plt = acc_precip_eff_stdev
              ,plt.title = paste0(current_chart_path
                    ,formatGraphTitleForFileName(acc_precip_eff_stdev_title)))
    
    # Accumulated precipitation 
    # Precipitacion Acumulada
    WriteJpeg(plt = acc_precip
              ,plt.title = paste0(current_chart_path
                              ,formatGraphTitleForFileName(acc_precip_title)))
    
    # Accumulated precipitation with additional selected years
    # Precipitacion Acumulada con años adicionales seleccionados
    WriteJpeg(plt = acc_precip_addyears 
              ,plt.title = paste0(current_chart_path
                     ,formatGraphTitleForFileName(acc_precip_addyears_title)))
    
    
    # Accumulated Potential Evapotranspiration (PET) with standard deviation
    # Evapotranspiracion potencial (PET) acumulada con desviacion estandar
    WriteJpeg(plt = acc_pet_stdev
              ,plt.title = paste0(current_chart_path
                           ,formatGraphTitleForFileName(acc_pet_stdev_title)))
    
    
    # Precipitation-over-PET ratio (P/PET)
    # Razon de la Precipitacion sobre la PET (P/PET)
    WriteJpeg(plt = ppet
              ,plt.title = paste0(current_chart_path
                                    ,formatGraphTitleForFileName(ppet_title)))
    
    
    # rolling average P/PET
    # promedio dinamico de la P/PET
    WriteJpeg(plt = rolling_avg_ppet
              ,plt.title = paste0(current_chart_path
                        ,formatGraphTitleForFileName(rolling_avg_ppet_title)))
    
    
    # rolling average eP/PET and P/PET w std dev
    # promedio dinamico de la eP/PET y la P/PET con desviacion estandar
    WriteJpeg(plt = rolling_avg_eppet
              ,plt.title = paste0(current_chart_path
                                  ,formatGraphTitleForFileName(rolling_avg_eppet_title)))
    
    # rolling average P/PET with additional selected years
    # promedio dinamico de la P/PET con años adicionales seleccionados
    WriteJpeg(plt = rolling_avg_ppet_addyears 
              ,plt.title = paste0(current_chart_path
              ,formatGraphTitleForFileName(rolling_avg_ppet_addyears_title)))
    
    # Weekly climatology chart comparing current precipitation and maximum 
    # temperature to LTN precip and max temperature
    
    # grafico climatologico semanal comparando la precipitacion actual y la temperatura maxima
    # con el valor LTN de la precipitacion y la temperatura maxima
    WriteJpeg(plt = weekly_chart
              ,plt.title = paste0(current_chart_path
                ,formatGraphTitleForFileName(paste0(place_name
                                                    ,"weekly_chart"))))
    
    # Multiplot
    # Multiplot
    
    # First, open a jpeg writer
    # Primero, abra un editor de jpeg
    jpeg(paste0(current_chart_path,
                formatGraphTitleForFileName(place_name),"_4chart.jpeg"), 
         width = 12, height = 6, 
         units = 'in', res = 500)
  
    # Generate the Multiplot
    # Genere un Multiplot
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
    # Cierre el objeto con el grafico actual
    invisible(dev.off())
    
  }

}

# Check the "charts" and "outputCSVs" folders in your working directory 
# to see the outputs of this script!

# ¡Revise las carpetas "charts" y "outputCSVs" en su directorio de trabajo
# para ver las salidas de este script!
