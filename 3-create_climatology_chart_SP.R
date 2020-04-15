#--------------------------------------------------------------------------
# aWhere R Tutorial: Create Climatology Chart
# Tutorial de R de aWhere: Cree un Grafico Climatologico
#
# Purpose of script: 
# This code will show you how to access aWhere's API to create a chart that 
# includes current maximum temperature, long-term maximum temperature, 
# current precipitation, and long-term precipitation for the time period 
# and location(s) of interest that you specify. 
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
# The outputs of this script include a chart for each location you added 
# in your text file as well as a corresponding csv for further analysis 
# (this is the raw data). The charts can be embedded in reports or visuals 
# for quick interpretation.
# 
# You will need to be connected to the internet to run this script.
#
# Proposito del Script
# Este codigo le mostrara como acceder al API (Interfaz de Programacion de Aplicaciones) de aWhere
# para crear un grafico que incluya la Temperatura Maxima Actual, Temperatura Maxima a largo plazo,
# Precipitacion Actual y Precipitacion a largo plazo para el periodo y ubicacion(es) de interes que
# usted especifique.
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
# Las salidas de este script incluyen un grafico para cada ubicacion que usted agrego
# a su archivo de texto, asi como tambien el archivo .csv respectivo para su posterior analisis
# (datos sin procesar). Estos graficos pueden ser incluidos en reportes o distintas visualizaciones
# para una rapida interpretacion.
#
# Usted necesita estar conectado a internet para ejecutar este codigo.
#
# Date updated: 2020-04-15
# Fecha de actualizacion: 2020-04-15
#--------------------------------------------------------------------------


# Install and load packages -----------------------------------------------
# Instalar y cargar paquetes ----------------------------------------------

# Clear your environment and remove all previous variables
# Limpie su entorno y remueva todas las variables previas
rm(list = ls())

# Install the aWhere R Charts package, if you have not already
# Instale el paquete aWhere R Charts, si usted aun no lo ha hecho
devtools::install_github("awhereAPI/aWhere-R-Charts")

# Load the packages needed for this script.
# If they have not been installed yet on your computer, 
# using this code to install them: install.packages("NAME OF PACKAGE")

# Cargue los paquetes necesarios para este script.
# Si estos no han sido instalados aun en su computadora,
# use este comando para instalarlos: install.packages("NOMBRE DEL PAQUETE")

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(data.table)
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

# Next, you need to set your working directory. This is the location on your 
# computer where R will automatically save the output files created by this 
# script.

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
# en su computadora para verificar que esta carpetas fueron creadas.


dir.create(path = "outputCSVs/", showWarnings = FALSE, recursive = TRUE) 
dir.create(path = "charts/", showWarnings = FALSE, recursive = TRUE)


# Supporting functions ----------------------------------------------------
# Supporting functions ----------------------------------------------------

# This script requires you to load a supporting functions file that is 
# typically saved in the Source folder in your aWhere tutorial folder 
# structure. This step loads additional functions required to create
# the climatology chart.   

# Este script requiere que usted carge el archivo de "supporting functions" que
# comunmente debe de estar guardado en la carpeta Source, a partir del tutorial
# aWhere referente a la estructura de carpetas y archivos. Este paso, carga funciones
# adicionales para crear el Grafico de la Climatologia.

# Modify the file name and path as needed below to load the supporting
# functions file. 

# Modifique el nombre y la ruta de acceso del archivo segun sea el caso debajo de esta linea
# para cargar el archivo de "supporting functions".
source("Source/supporting_functions.R")

# Location(s) of interest -------------------------------------------------
# Sitio(s) de interes -----------------------------------------------------

# In this section, we will pull forecast data for your location of interest. 
# CHANGE THIS to the path of your locations text file.

# En esta seccion obtendremos los datos del pronostico para su sitio de interes.
# CAMBIE ESTO por la ruta de acceso y nombre de su Archivo de Coordenada
locations_file <- "RunSet/locations.txt"

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


date_start <- "2018-01-01"  # CHANGE TO THE STARTING DATE OF YOUR CHOICE
                            # PUEDE CAMBIAR ESTO A LA FECHA DE INICIO DE SU ELECCION              

date_end <- "2019-06-16"    # CHANGE TO THE ENDING DATE OF YOUR CHOICE 
                            # PUEDE CAMBIAR ESTO A LA FECHA FINAL DE SU ELECCION

# Long-term normal (LTN) values will be calculated across this range of years
# Los valores promedio a largo plazo (LTN) se calcularan a partir de un rango de años.

year_start <- 2011  # Starting year can be as early as 2008
                    # El año inicial tiene que ser igual o mayor al 2008.
year_end <- 2018 

# Climatology chart -------------------------------------------------------

# The following is called a loop process - you only need to run the line 
# that starts with "for", and the subsequent lines will automatically run 
# until the end of the script. A climatology chart will be generated 
# for each location in your locations text file. 
# The outputs will be saved within your working directory.

# Las siguientes lineas se denominan un loop (ciclo) - usted solo necesita ejecutar
# la linea que comienza con un "for" y las lineas siguientes se ejecutaran automaticamente
# hasta el final del script. Se generara un Grafico de la Climatologia para cada ubicacion de
# su Archivo de Coordenadas y estos se guardaran automaticamente en su directorio de trabajo.



for (i in 1:nrow(locations)) {

  # Get the latitude, longitude, and name of the current location
  # Obtenga primero la latitud, longitud y nombre de la ubicacion actual
  lat <- locations$latitude[i]
  lon <- locations$longitude[i]
  place_name <- locations$place_name[i]
  
  # Print a status message about the current location being processed
  # Imprima un mensaje con el estado que indica la ubicacion actual que esta siendo procesada
  print(paste0("Creating climatology chart ",
               "(", i, "/", nrow(locations), "): ", place_name))
  
  # Pull the weather data directly from the aWhere API
  # Obtenga los datos meteorologicos directamente desde la API de aWhere
  weather_df <- aWhereCharts::generateaWhereDataset(lat = lat 
                                                    ,lon = lon
                                                    ,day_start = date_start
                                                    ,day_end = date_end
                                                    ,year_start = year_start
                                                    ,year_end = year_end)
  
  # Round the latitude and longitude values for chart title
  # Redeondee los valores de latitud y longitud para el titulo del grafico
  lat_title <- round(lat, 2)
  lon_title <- round(lon, 2)
  
  chart_title <- paste0("Weekly Climate Chart for ", place_name, 
                        "\nLatitude: ", lat_title,",  Longitude: ",lon_title
                        , ", Date range: ",date_start, " to ", date_end)
 
   weekly_chart <- generateaWhereChart(data = weather_df,
                                       variable = "precipitation",
                                       title = chart_title,
                                       includeSTD = TRUE,
                                       variable_rightAxis = 'maxTemp',
                                       daysToAggregateOver = 7,
                                       mainGraphType = 'bar')
  # Write weekly chart to jpg
  # Convierta el grafico semanal en una imagen con formato jpg
  WriteJpeg(plt = weekly_chart
            ,plt.title = paste0("charts/WeeklyChart_", 
                                date_end, "_", place_name),
            w=12, h=6, r=500)

  #------------------------------------------------------------------------
  
  # Clean up the weather data frame for clear output file
  # Depure el "weather data frame" para dar claridad al archivo de salida
  many_data <- weather_df %>% dplyr::select(1:3,17,18,20,21,26,27,29
                                    ,30,32,33,35,36,38,39,41
                                    ,42,44,45,47,48) # Adjust columns as needed
                                                     # Ajuste las columnas segun sea necesario
  
  names(many_data)[grep("gdd.amount", names(many_data))] <- "GDD"
  names(many_data)[grep("gdd.average", names(many_data))] <- "GDD_avg"
  names(many_data)[grep("pet.amount", names(many_data))] <- "PET"
  names(many_data)[grep("pet.average", names(many_data))] <- "PET_avg"
  names(many_data)[grep("temperatures.max.amount", names(many_data))] <- "maxTemp"
  names(many_data)[grep("temperatures.min.amount", names(many_data))] <- "minTemp"
  names(many_data)[grep("temperatures.max.average", names(many_data))] <- "maxTemp_avg"
  names(many_data)[grep("temperatures.min.average", names(many_data))] <- "minTemp_avg"
  names(many_data)[grep("precipitation.amount", names(many_data))] <- "Precip"
  names(many_data)[grep("precipitation.average", names(many_data))] <- "Precip_avg"
  names(many_data)[grep("relativeHumidity.max.amount", names(many_data))] <- "RHmax"
  names(many_data)[grep("relativeHumidity.min.amount", names(many_data))] <- "RHmin"
  names(many_data)[grep("relativeHumidity.max.average", names(many_data))] <- "RHmax_avg"
  names(many_data)[grep("relativeHumidity.min.average", names(many_data))] <- "RHmin_avg"
  names(many_data)[grep("wind.average.amount", names(many_data))] <- "wind"
  names(many_data)[grep("wind.average.average", names(many_data))] <- "wind_avg"
  names(many_data)[grep("wind.dayMax.amount", names(many_data))] <- "max_wind"
  names(many_data)[grep("wind.dayMax.average", names(many_data))] <- "max_wind_avg"
  names(many_data)[grep("wind.dayMax", names(many_data))] <- "wind_day_max"
  names(many_data)[grep("solar.amount", names(many_data))] <- "solar"
  names(many_data)[grep("solar.average", names(many_data))] <- "solar_avg"
  
  # to trim decimal points...
  # Las siguientes lineas son para redondear/recortar decimales
  many_data$max_wind <- round(many_data$max_wind, 1)
  many_data$max_wind_avg <- round(many_data$max_wind_avg, 1)
  many_data$RHmax <- round(many_data$RHmax, 0)
  many_data$RHmax_avg <- round(many_data$RHmax_avg, 0)
  many_data$RHmin <- round(many_data$RHmin, 0)
  many_data$RHmin_avg <- round(many_data$RHmin_avg, 0)
  many_data$maxTemp <- round(many_data$maxTemp, 1)
  many_data$maxTemp_avg <- round(many_data$maxTemp_avg, 1)
  many_data$minTemp <- round(many_data$minTemp, 1)
  many_data$minTemp_avg <- round(many_data$minTemp_avg, 1)
  many_data$solar <- round(many_data$solar, 0)
  many_data$solar_avg <- round(many_data$solar_avg, 0)
  many_data$Precip <- round(many_data$Precip, 1)
  many_data$Precip_avg <- round(many_data$Precip_avg, 1)
  many_data$wind <- round(many_data$wind, 1)
  many_data$wind_avg <- round(many_data$wind_avg, 1)
  many_data$GDD <- round(many_data$GDD, 1)
  many_data$GDD_avg <- round(many_data$GDD_avg, 1)
  many_data$PET <- round(many_data$PET, 2)
  many_data$PET_avg <- round(many_data$PET_avg, 2)
  
  # Write the weather data to .csv file
  # A continuacion, se van a escribir los datos meteorologicos en un archivo .csv
  write.csv(many_data, 
            file = paste0("outputCSVs/", "Weekly_Chart_", 
                          date_end, "_", place_name, ".csv"), 
            row.names = F )
}

# Check the charts and outputCSVs folders in your working directory 
# to see the outputs of this script!

# ¡Revise las carpetas charts y outputCSVs en su directorio de trabajo
# para ver las salidas de este script!