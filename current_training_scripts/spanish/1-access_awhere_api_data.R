#--------------------------------------------------------------------------
 # Proposito del Script:
 # Este codigo le mostrara como acceder a las bases agrometeorologicas de aWhere
 # a partir de nuestro API (Interfaz de Programacion de Aplicaciones) para su area de interes.
 # Antes de ejecutar este script, le solicitamos encontrar la latitud y la longitud de su
 # sitio de interes, ya sea utilizando Google Maps, QGIS o bien, los archivos geoespaciales de
 # aWhere disponibles en apps.awhere.com o, utilizando puntos GPS que usted haya obtenido previamente.
 #
 # Este script le provee a usted las siguientes bases de datos para su sitio de interes:
 # 1. Un archivo csv con el pronostico del tiempo (Horario, a 6 horas, a 12 horas o en franjas diarias de tiempo).
 # 2. Datos observados para cualquier periodo de tiempo entre el 2008 y el presente.
 # 3. Valores promedio a largo plazo (LTN) de las variables meteorologicas para un periodo escogido entre el 2008 y
 # el presente.
 # 4. Un archivo csv llamado "aWhere Weather Dataset" que incluye todas las variables observadas y todas las
 # variables LTN incluyendo las diferencias con respecto a los valores normales.
 #
 # Usted necesita estar conectado a internet para ejecutar este codigo.
 #
 # Fecha de actualizacion: 2020-04-14

#--------------------------------------------------------------------------
 # Instalar y cargar paquetes ----------------------------------------------
 #
 # Limpie su entorno y remueva todas las variables previas
rm(list = ls())

 # Instale los paquetes de R de aWhere, si no lo ha hecho previamente
devtools::install_github("aWhereAPI/aWhere-R-Library")
devtools::install_github("aWhereAPI/aWhere-R-Charts")

 # Cargue los paquetes necesarios para este script.
 # Si estos no han sido instalados aun en su computadora,
 # use este comando para instalarlo: install.packages("NOMBRE DEL PAQUETE")
library(devtools)
library(rgeos)
library(raster)
library(foreach)
library(aWhereAPI)
library(aWhereCharts)

 # Cargue sus credenciales de aWhere ---------------------------------------
 #
 # Es necesario cargar su Archivo de Credenciales que incluye su Key & Secret
 # de aWhere, similar a un nombre de usuario y contrase?a. Esto le brinda una
 # especie de "Token" que muestra que usted tiene acceso al API y a todos los datos de aWhere.
 # Debe de mantener sus credenciales en una ubicacion que pueda hallar facilmente.
 # Copie la ruta de acceso de este archivo y pegue esta sobre la frase "YOUR CREDENTIALS HERE"
aWhereAPI::load_credentials("YOUR CREDENTIALS HERE")

 # Establezca sus directorios de trabajo y de salidas ----------------------
 #
 # A continuacion, usted necesita establecer su directorio de trabajo. Este es el lugar
 # en su computadora en el que R va a guardar los archivos de salida de este script.
 #
 # Para establecer su directorio de trabajo, busque la carpeta en su computadora en la cual
 # usted desea que las salidas de este script se guarden, copie la ruta de acceso y pegue
 # esta sobre la frase "YOUR WD HERE"
working_dir <- "YOUR WD HERE" 

 # Esto establece su directorio de trabajo en la ruta de acceso de working_dir
setwd(working_dir) 
                  
 # Ahora usted va a crear la carpeta dentro de su directorio de trabajo en la que
 # sus archivos csv de salida se van a guardar. La siguiente linea crea una carpeta
 # en su directorio de trabajo llamada outputCSVs.Puede navegar a su directorio de trabajo
 # en su computadora para verificar que esta carpeta fue creada.
dir.create(path = "outputCSVs/", showWarnings = FALSE, recursive = TRUE) 

 # Una vez establecidos sus parametros para este script, usted esta listo
 # para empezar a solicitar datos de la API e investigar mas a fondo para su area de interes.
 #
 # Pronostico --------------------------------------------------------------
 # En esta seccion, vamos a obtener los datos del pronostico para su sitio de interes.
 # Primero, determine el nombre la ubicacion, su latitud y longitud.
 # Puede usar QGIS, Google Maps, o sus propios datos para encontrar esta informacion.
 # Seguidamente, cree el siguiente archivo de texto con la informacion de la ubicacion.
 # Refierase al archivo de texto de ejemplo "locations.txt" en la carpeta RunSet para darle
 # formato a este archivo. Debe de tener 3 columnas con los nombres place_name, latitude, longitude.
 # Un ejemplo de una fila con la informacion de la ubicacion seria:
 #     place_name, latitude, longitude
 #     Nairobi, -1.283, 36.816
 #
 # CAMBIE ESTO por la ruta de acceso de su archivo de texto "locations.txt"
locations_file <- "YOUR LOCATION FILE.txt" 

 # Lea su archivo de texto de ubicacion(es)
locations <- read.csv(locations_file)

for (i in(1:nrow(locations))) { 
  
   # Obtenga las primeras latitudes, longitudes y nombre de su(s) sitio(s) de interes
  lat <- locations$latitude[i]
  lon <- locations$longitude[i]
  place_name <- locations$place_name[i]
  
   # Obtenga el pronostico del tiempo directamente desde nuestra API  
  forecast <- aWhereAPI::forecasts_latlng(lat
                                           ,lon 
                                           ,day_start = as.character(Sys.Date()) 
                                           ,day_end = as.character(Sys.Date()+7)
                                           ,block_size = 6) 
  
   # Los parametros por defecto del pronostico del codigo anterior son:
   # La fecha de inicio es hoy, Sys.Date()
   # La fecha final es siete dias a partir de ahora, Sys.Date() +7
   # Block size se refiere al numero de horas que se representara en cada punto. Este valor es de 6,
   # pues obtiene los datos del pronostico para bloques de 6 horas. Un block size de 1 brindara
   # el pronostico en bloques de una hora.
   #
   # Guarde los datos del pronostico en un archivo .csv dentro de la carpeta outputCSVs
   # que usted creo dentro de su directorio de trabajo.
  write.csv(forecast, file = paste0("outputCSVs/Forecast-6hour-",place_name,".csv"), row.names=F) 
  
   # Puede tambien hacer clic en el dataframe del pronostico en la pesta?a "Environment"
   # en la consola superior derecha para ver los datos en RStudio.
   #
   # Datos Observados --------------------------------------------------------
   #
   # Establezca sus fechas de inicio y fin para un periodo de interes
   #
   # Aqui vamos a agregar los datos historicos para su area de interes.
   #
   # 1ero de enero, 2016
  starting_date <- "2018-01-01" 
   
   # hace dos dias
  ending_date <- as.character(Sys.Date() - 2) 
                                  
   # Obtenga los datos meteorologicos observados a partir del API de aWhere
  observed <- aWhereAPI::daily_observed_latlng(latitude = lat,
                                               longitude = lon,
                                               day_start = starting_date,
                                               day_end = ending_date)
  
  write.csv(observed, file = paste0("outputCSVs/observedData-",place_name,".csv"), row.names=F) 
  
   # Los parametros para esta funciion pueden tener muchos formatos.
   # Usted puede cambiar las fechas de inicio/fin para un periodo de interes.
   # Puede utilizar el formato "AAAA-MM-DD" para una fecha especifica.
   # Tambien pude utilizar Sys.Date() para que su fecha final sea hoy,
   # o de forma similar, puede usar Sys.Date() - 1 para que su fecha final sea ayer.
   # NOTE que los datos observados SOLO pueden pertenecer al pasado. Si selecciona 
   # una fecha futura el sistema le indicara un error.
   #
   # Haga clic en el dataframe "observed" en la pesta?a "environment" en la consola 
   # superior derecha para ver los datos.
   #
   # Datos Agronomicos -------------------------------------------------------
   #
   # Aqui usted va a obtener los datos agronomicos para su lugar y periodo de interes.
   # Si no cambia las variables de "starting_date" y "ending_date", entonces
   # el periodo tiempo de los datos va a ser el mismo de los Datos Observados que se obtuvieron previamente.
   #
   # Obtener los datos agronomicos a partir del API de aWhere
  ag <- aWhereAPI::agronomic_values_latlng(lat
                                            ,lon 
                                            ,day_start = starting_date 
                                            ,day_end = ending_date)
  
   # Haga clice en el dataframe "ag" de la pesta?a "environment" en la consola superior derecha
   # para ver los datos.

  write.csv(ag, file = paste0("outputCSVs/agronomicsData-",place_name,".csv"), row.names=F) 
  
   # Valores promedio a largo plazo (LTN) ------------------------------------
   #
   # Aqui usted podra obtener los valores promedio a largo plazo (LTN) de las variables meteorologicas
   # para su sitio y periodo de interes.
   #
   # Las variables LTN se calculan a partir de un rango de a?os.
  year_start <- 2011
  year_end <- 2018
  
   # Por favor especifique su mes-dia de inicio y de fin,
   # asi como tambien su "temporada de crecimiento" de cultivos
   #
   # Enero 1
  monthday_start <- "01-01" 
                            
   # Junio 16
  monthday_end <- "06-16"   
                            
   # Obtenga los datos meteorologicos LTN a partir del API de aWhere
   #
   # puede tambien excluir ciertos a?os de las variables LTN
  ltn <- weather_norms_latlng(lat, lon,
                               monthday_start = monthday_start,
                               monthday_end = monthday_end,
                               year_start = year_start,
                               year_end = year_end,
                               # puede tambien excluir ciertos a?os de las variables LTN
                               exclude_years = c("2011", "2016")) 

   # Haga clic en el dataframe "ltn" en la pesta?a "environment" en la consola superior derecha
   # para ver los datos.
  write.csv(ltn, file = paste0("outputCSVs/ltnData-",place_name,".csv"), row.names=F) 
  
   # Base de datos aWhere Ag-Weather Dataset completa ------------------------
   #
   # Esta seccion combina todas las bases de datos anteriores de una forma cohesiva
   # en un archivo .csv para su posterior analisis. Puede cambiar la ubicacion y periodo de interes,
   # segun sea el caso en las lineas siguientes del codigo.
  starting_date <- "2018-01-01"
  ending_date <- "2019-06-16"
  year_start <- 2008
  year_end <- 2018
  
   # La siguiente funcion crea un conjunto de datos depurados con los datos observados, 
   # los datos agronomicos y los valores promedio a largo plazo.
  weather_df <- generateaWhereDataset(lat = lat, 
                                      lon = lon, 
                                      day_start = starting_date, 
                                      day_end = ending_date, 
                                      year_start = year_start, 
                                      year_end = year_end)
  
   # Guarde el archivo .csv del conjunto de datos en la carpeta outputCSVs que fue creada
   # dentro de su directorio de trabajo.
  
  write.csv(weather_df, 
            file = paste0("outputCSVs/aWhereWeatherDataset-",place_name,".csv"), 
            row.names=F) 
}


