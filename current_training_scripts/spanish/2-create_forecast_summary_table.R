#--------------------------------------------------------------------------
 # Tutorial de R de aWhere: Tabla resumen del pronostico
 #
 # Proposito del script:
 # Este codigo le mostrara como acceder a los datos del pronostico de aWhere
 # a partir de nuestro API (Interfaz de Programacion de Aplicaciones) para su area de interes.
 # Antes de ejecutar este script, le solicitamos encontrar la latitud y la longitud de su
 # sitio de interes, ya sea utilizando Google Maps, QGIS o bien, los archivos geoespaciales de
 # aWhere disponibles en apps.awhere.com o, utilizando puntos GPS que usted haya obtenido previamente.
 #
 # Las salidas de este script incluyen un pronostico (7-dias, 168 horas) en csv para su zona de interes
 # y una tabla del pronostico que puede ser incluida en distintos reportes o visualizaciones para una
 # rapida interprestacion.
 #
 # Usted necesita estar conectado a internet para ejecutar este codigo.

#--------------------------------------------------------------------------
 # Instalar y cargar paquetes ----------------------------------------------
 #
 # Limpie su entorno y remueva todas las variables previas
rm(list = ls())

 # Instale el paquete aWhere R Charts, si usted aun no lo ha hecho
devtools::install_github("awhereAPI/aWhere-R-Charts")

 # Cargue los paquetes necesarios para este script.
 # Si estos no han sido instalados aun en su computadora,
 # use este comando para instalarlo: install.packages("NOMBRE DEL PAQUETE")
library(aWhereAPI)
library(data.table)
library(gridExtra)
library(grid)
library(gtable)
library(grDevices)
library(dplyr)

 # Cargue sus credenciales de aWhere ---------------------------------------
 #
 # Es necesario cargar su Archivo de Credenciales que incluye su Key & Secret
 # de aWhere, similar a un nombre de usuario y contraseña. Esto le brinda una
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
 #
 # Esto establece su directorio de trabajo en la ruta de acceso de working_dir 
working_dir <- "YOUR WD HERE" 
setwd(working_dir) 
             
 # Ahora usted va a crear la carpeta dentro de su directorio de trabajo en la que
 # sus archivos csv de salida se van a guardar. La siguiente linea crea una carpeta
 # en su directorio de trabajo llamada outputCSVs.Puede navegar a su directorio de trabajo
 # en su computadora para verificar que esta carpeta fue creada.
dir.create(path = 'outputCSVs/', showWarnings = FALSE, recursive = TRUE) 

 # Sitio(s) de interes -----------------------------------------------------
 #
 # En esta seccion, vamos a obtener los datos del pronostico para su sitio de interes.
 # Primero, determine el nombre la ubicacion, su latitud y longitud.
 # Puede usar QGIS, Google Maps, o sus propios datos para encontrar esta informacion.
 # Seguidamente, un archivo de texto con la informacion de la ubicacion. 
 # Refierase al archivo de texto de ejemplo "locations.txt" en la carpeta RunSet para darle
 # formato a este archivo. Debe de tener 3 columnas con los nombres place_name, latitude, longitude.
 # Un ejemplo de una fila con la informacion de la ubicacion seria:
 #     place_name, latitude, longitude
 #     Nairobi, -1.283, 36.816
 #
 # CAMBIE ESTO por la ruta de acceso y nombre de su Archivo de Coordenadas
locations_file <- "RunSet/locations.txt"

 # Lea su Archivo de Coordenadas
locations <- read.csv(locations_file)

 # Tabla resumen del pronostico --------------------------------------------
 #
 # Las siguientes lineas se denominan un loop (ciclo) - usted solo necesita ejecutar
 # la linea que comienza con un "for" y las lineas siguientes se ejecutaran automaticamente
 # hasta el final del script. Las salidas correspondientes a los datos y la tabla del pronostico
 # se guardaran automaticamente en su directorio de trabajo.

for (i in(1:nrow(locations))) { 

   # Obtenga primero la latitud, longitud y nombre de la ubicacion actual
  lat <- locations$latitude[i]
  lon <- locations$longitude[i]
  place_name <- locations$place_name[i]
  
   # Obtenga el pronostico del tiempo directamente desde la API de aWhere
  forecast<- aWhereAPI::forecasts_latlng(lat
                                        ,lon 
                                        ,day_start = as.character(Sys.Date()) 
                                        ,day_end = as.character(Sys.Date()+3)
                                        ,block_size = 6)  # MUST use 6 hours
  
   # Guarde el archivo .csv con el conjunto de datos en la carpeta outputCSVs 
   # que fue creada dentro de su directorio de trabajo
  write.csv(forecast, 
            file = paste0("outputCSVs/",place_name,"_Forecast-6hour.csv"), 
            row.names=F) 
   # Dar formato a sus datos pronosticados para la tabla-resumen -----------
   #
   # Seleccione columnas especificas para el resumen del pronostico
  df_sub <- subset(forecast, select = c(1,2,3,6,7,8,9,10, 18))
  
   # A continuacion, is.num() tiene el valor TRUE para columnas numericas, y FALSE en el caso contrario.
   # Seguidamente, vamos a aplicar la funcion "round" a las columnas numericas:
  is.num <- sapply(df_sub, is.numeric)
  df_sub[is.num] <- lapply(df_sub[is.num], round, 2)
  
   # Acortando los nombres de las columnas
  colnames(df_sub) <- c("Lat"
                        ,"Lon"
                        ,"dateTime"
                        ,"Conditions"
                        ,"Max_Temp"
                        ,"Min_Temp"
                        ,"Chance_Precip"
                        ,"Amount_Precip"
                        ,"Max_Wind")

   # Convirtiendo el data.frame del pronostico en un objeto del tipo data.table
  df_sub <- data.table::as.data.table(df_sub)
  
   # Acortando el "string" a la naturaleza de los datos - Ajustando la zona horaria (dejando de lado el formato GMT)
  df_sub[, c('date','startTime') := tstrsplit(x = dateTime
                                             ,split = 'T'
                                             ,fixed = TRUE)]
  
  df_sub[, c('startTime', 'timeZone') := tstrsplit(x = startTime
                                                 ,split = '\\+|-')]
  
  df_sub[, c('startHour', 'startMin', 'startSec') := tstrsplit(x = startTime
                                                            ,split = ':'
                                                            ,fixed = TRUE
                                                        ,type.convert = TRUE)]
  
   # Cambiar el formato de la media noche (24:00) para que sean las 00:00 horas
  df_sub[, endHour := startHour + 6]
  df_sub[endHour == 24, endHour := 0]
  
   # Crear un string de tiempo (timeString) con base en los bloques horarios de tiempo
  df_sub[startHour == 0,  timeString := 'Midnight-0600']
  df_sub[startHour == 6,  timeString := '0600-Noon']
  df_sub[startHour == 12, timeString := 'Noon-1800']
  df_sub[startHour == 18, timeString := '1800-Midnight']
  
   # Removiendo columnas para que no sean incluidas en el archivo de salida
  df_sub[, c('dateTime'
            ,'timeZone'
            ,'startTime'
            ,'startMin'
            ,'startSec'
            ,'startHour'
            ,'endHour') := NULL]
  
   # Reordenando las columnas
  df_sub = df_sub %>% dplyr::select(date
                                    ,timeString
                                    ,Max_Temp
                                    ,Min_Temp
                                    ,Chance_Precip
                                    ,Amount_Precip
                                    ,Max_Wind
                                    ,Conditions)          
  
   # Convertir la Tabla a un nuevo formato para reformatearla posteriormente
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

   # Guardar la Tabla Resumen del Pronostico como una imagen
  out_file = paste0(place_name,"_table.png")
  grDevices::png(filename = out_file, 
                 width = 980,
                 height = 780,
                 bg = "white")
  
  grid::grid.draw(table)
  dev.off()
  
}

