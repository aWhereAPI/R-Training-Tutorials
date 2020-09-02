#--------------------------------------------------------------------------
 # Tutorial de R de aWhere: Mapas, Histogramas y Estadisticas
 #
 # Proposito del Script:
 # Este script examina una region y tiene como salidas los mapas, histogramas
 # y graficos para las regiones o subregiones dentro de esta.
 #
 # Usted necesita estar conectado a internet para ejecutar este codigo.
 #
 #--------------------------------------------------------------------------
 #
 # Instalar y cargar paquetes ----------------------------------------------
 #
 # Limpie su entorno y remueva todas las variables previas
rm(list = ls())

 # Cargue los paquetes necesarios para este script
library(ggmap)
library(ggplot2)
library(dplyr)  
library(wicket)
library(aWhereAPI)
library(aWhereCharts)
library(data.table)

 # Establezca su directorio de trabajo -------------------------------------
 #
 # A continuacion, usted necesita establecer su directorio de trabajo. Este es el lugar
 # en su computadora en el que R va a guardar los archivos de salida de este script.
 #
 # Para establecer su directorio de trabajo, busque la carpeta en su computadora en la cual
 # usted desea que las salidas de este script se guarden, copie la ruta de acceso y pegue
 # esta sobre la frase "YOUR WD HERE"
working_dir <- "YOUR WD HERE" 

 Esto establece su directorio de trabajo en la ruta de acceso de working_dir
setwd(working_dir) 
                   
 # Supporting functions ----------------------------------------------------
 #
 # Este script requiere que usted carge el archivo de "supporting functions" que
 # comunmente debe de estar guardado en la carpeta Source, a partir del tutorial de
 # aWhere referente a la estructura de carpetas y archivos. Este paso, carga funciones
 # adicionales para crear el Grafico de la Climatologia. Copie la ruta de acceso respectiva
 # y peguela sobre la frase "YOUR PATHNAME".
source("YOUR PATHNAME/supporting_functions.R")

 # cargue los datos externos: plantilla ("template") y archivos agrometeorologicos ------
 #
 # Cargue su(s) archivo(s) de datos METEOROLOGICOS.
 # Estos archivos le seran brindados; sin embargo, tambien puede descargarlos
 # a partir del portal de mapas interactivo disponible en apps.awhere.com. Ademas,
 # ahi podra encontrar ejemplos de datos en la carpeta BaseData. Ahi puede encontrar
 # cualquier cantidad de archivos o archivos individuales que  puden ser ingresados en un ciclo.
 #
 # CAMBIE LO SIGUIENTE por su ruta de acceso del archivo de datos meteorologicos
weather_file_list <- c("YOUR PATHFILE HERE/190613_past7.csv") 

 # Cargue el archivo TEMPLATE
 # En la carpeta RunSet, hay un archivo cuyo nombre es similar a "Template___.csv".
 # Este archivo incluye la columna shapewkt (poligono) asi como tambien los datos de poblacion,
 # tipo de suelo, y otros conjuntos de datos para cada celda de la grilla de aWhere.
 # Póngase en contacto con aWhere para obtener una plantilla para su área de interés
 #
 # CAMBIE ESTO por su ruta de acceso del archivo de plantilla ("template")
template_file <- "RunSet/Template____.csv" 

 # Region(es) de interes ---------------------------------------------------
 #
 # Defina la region(es) para las cuales desea generar mapas, histogramas y
 # estadisticas con base en las columnas de su archivo de plantilla ("template").
 # Cada asociacion entre operadores booleanos se va a considerar de forma independiente
 # (por ejemplo: es NOT condition1 AND condition 2 AND condition3).
 # Si se ingresa una lista en formato separado por comas, se hara un subconjunto de todas las unidades en esa lista.
 # Si se escoje "ENTIRE_REGION" el codigo se ejecutara para toda la region.
 # Utilice la siguiente sintaxis para hacer un subconjunto de las subregiones especificas con base
 # en cualquier atributo disponible. Mas adelante, se profundizara en como realizar subconjuntos de 
 # multiples atributos.
 #
 # subarea_select <- c("admin0_name: Kenya"
 #                    ,"admin1_name: Wajir, Kitui, Makueni"
 #                    ,"soiltype: MEDIUM")
subarea_select <- c("admin0_name: Zambia"
                    ,"admin1_name: Luapula, Lusaka, Western, Nyamira")

#--------------------------------------------------------------------------
 # NO MODIFIQUE estas lineas - ajuste adicional de parametros
 #
 # Escriba los histogramas como archivos de imagen (TRUE o FALSE)
write_hist <-  TRUE

 # Establezca las dimensiones para cualquier grafico o histograma que vaya a ser una salida del codigo
fig_dim_graph <- c(height = 3.38, width = 6.02)
fig_dim_hist  <- c(height = 6, width = 10)

 # Bandejas ("Bins") para los resumenes de los datos tabulados de los histogramas
 #
 # Precipitacion
bins_precip <- c(seq(from = 0, to = 300, by = 12.5), Inf)

 # P/PET
bins_ppet <- c(0, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
               , 1.0, 1.1, 1.2, 1.4, 1.6, 2.0, Inf) 
#--------------------------------------------------------------------------

 # Comienzo del Analisis
 #
 # Comience un ciclo para cada archivo de datos meteorologicos especificado.
 # Cada archivo contiene datos meteorologicos para dias especificos.
for (j in 1:length(weather_file_list))  {    
  
   # Cargue y procese los archivos de datos meteorologicos por estudiar ----
   #
   # Grabe el nombre del archivo actual
  weather_file <- weather_file_list[j]
  
   # Como el archivo debe de especificarse mediante su ruta de acceso, obtenga el nombre del archivo
  weather_name  <- tail(strsplit(x = weather_file
                                 ,split = "/"
                                 ,fixed = TRUE)[[1]]
                        ,n = 1)
  
   # lea los datos meteorologicos
  weather_df <- read.csv(weather_file)
  
   # Realice un ciclo a traves de cada region y/o subregion para que sea analizada de forma independiente----
  for (x in 1:length(subarea_select)) {
    
     # Procese el archivo "template" para la ejecucion ("corrida") actual
     #
     # lea los datos del archivo "template". Remueva las columnas que estan duplicadas
     # en el archivo de datos meteorologicos
    template_df <- 
      read.csv(template_file) %>% 
      dplyr::select( -c(shapewkt, longitude, latitude))
    
     # Analice la informacion del subconjunto de modo que sepa cuales datos observar
    subarea_select_list <- 
      strsplit(x = subarea_select[x]
               ,split = ":"
               ,fixed = TRUE) %>%
      lapply(.,trimws)
    
     # En el caso en que el usuario brinda una lista separada por comas, analice eso
    subarea_select_list[[2]] <- 
      strsplit(x = subarea_select_list[[1]][2]
               ,split = ","
               ,fixed = TRUE)[[1]] %>%
      lapply(.,trimws) %>%
      unlist(.) %>%
      as.vector(.)
    
     # Si se selecciono la region entera ("ENTIRE REGION"), asigne palabras a las siguientes variables
     # para darle formato a sus nombres de forma adecuada
    if (any(is.na(subarea_select_list[[2]])) == TRUE) {
      subarea_select_list[[1]][1] <- "ENTIRE"
      subarea_select_list[[2]] <- "REGION"
    }
    
     # Combine lo anterior mediante un nombre util que pueda ser empleado para guardar los archivos
    subarea_select_name <- paste(subarea_select_list[[1]][1]
                                 ,paste0(subarea_select_list[[2]]
                                         ,collapse = "_"),sep = "_")
    
     # Filtre el conjunto de datos para la(s) subregion(es) de interes
     # y escriba este conjunto de datos filtrados en un archivo. Este archivo sera
     # un archivo de plantilla ("template") para el pronostico.
    if (subarea_select_name != "ENTIRE_REGION"){ 
      template_df <- eval(parse(text = paste0("template_df[template_df$"
                                              ,subarea_select_list[[1]][1]
                                    ," %in% subarea_select_list[[2]],]")))
    } 
    
     # Determine la ruta de acceso que se usara para guardar los archivos electronicos
    csv_path <- paste0("outputCSVs/", subarea_select_name)
    figures_path <- paste0('figures/', subarea_select_name)
    
     # Cree los directorios de salida necesarios - si se requiere 
    dir.create(path = csv_path
               ,showWarnings = FALSE
               ,recursive = TRUE)   
    dir.create(path = figures_path
               ,showWarnings = FALSE
               , recursive = TRUE)
    
     # Filtre los datos meteorologicos solo para las ubicaciones de la grilla que estan dentro del archivo "template"
    get_country_area <- 
      weather_df %>% 
      dplyr::filter(locationid %in% template_df$locationid)
    
     # Fusione los datos meteorologicos con los datos del tipo "template"
    weather_template_df <- 
      merge(get_country_area, template_df, by = "locationid")
    
     # Calcule las estadisticas a traves de admin0, o sea el mas alto nivel administrativo
    admin0_stats <- 
      weather_template_df %>%
      dplyr::group_by(admin0_name) %>%
      dplyr::summarise(tot_POP = sum(population_2020),          
                       avg_CSUMPRE = mean(CSUMPRE),
                       max_CSUMPRE = max(CSUMPRE),
                       sd_CSUMPRE = sd(CSUMPRE),
                       avg_LTNsumPre = mean(LTNSUMP),
                       max_LTNsumPre = max(LTNSUMP),
                       sd_LTNsumPre = sd(LTNSUMP),
                       avg_D_CLTNSUMPRE = mean(DFLTSUM),
                       max_D_CLTNSUMPRE = max(DFLTSUM),
                       sd_D_CLTNSUMPRE = sd(DFLTSUM),
                       avg_CP_PET = mean(CPOVRPR),
                       max_CP_PET = max(CPOVRPR),
                       sd_CP_PET = sd(CPOVRPR),
                       avg_LTNP_PET = mean(LTNASPO),
                       max_LTNP_PET = max(LTNASPO),
                       sd_LTNPPET = sd(LTNASPO),
                       avg_D_CLTNP_PET = mean(DFLTPVP),
                       max_D_CLTNP_PET = max(DFLTPVP),
                       sd_D_CLTNP_PET = sd(DFLTPVP),
                       avg_CAvgMinT = mean(CAvgMinT),
                       max_CAvgMinT = max(CAvgMinT),
                       sd_CAvgMinT = sd(CAvgMinT),
                       avg_CAvgMaxT = mean(CAvgMaxT),
                       max_CAvgMaxT = max(CAvgMaxT),
                       sd_CAvgMaxT = sd(CAvgMaxT),
                       avg_LTAvgMxT = mean(LTAvgMxT),
                       max_LTAvgMxT = max(LTAvgMxT),
                       sd_LTAvgMxT = sd(LTAvgMxT),
                       avg_LTAvgMnT = mean(LTAvgMnT),
                       max_LTAvgMnT = max(LTAvgMnT),
                       sd_LTAvgMnT = sd(LTAvgMnT),
                       n_grids = n())
    
     # Calcule las estadisticas atravez de los admin1S dentro de admin0
    admin1_stats <- 
      weather_template_df %>%
      dplyr::group_by(admin0_name, admin1_name) %>% 
      dplyr::summarise(tot_POP = sum(population_2020),   
                       avg_CSUMPRE = mean(CSUMPRE),
                       max_CSUMPRE = max(CSUMPRE),
                       sd_CSUMPRE = sd(CSUMPRE),
                       avg_LTNsumPre = mean(LTNSUMP),
                       max_LTNsumPre = max(LTNSUMP),
                       sd_LTNsumPre = sd(LTNSUMP),
                       avg_D_CLTNSUMPRE = mean(DFLTSUM),
                       max_D_CLTNSUMPRE = max(DFLTSUM),
                       sd_D_CLTNSUMPRE = sd(DFLTSUM),
                       avg_CP_PET = mean(CPOVRPR),
                       max_CP_PET = max(CPOVRPR),
                       sd_CP_PET = sd(CPOVRPR),
                       avg_LTNP_PET = mean(LTNASPO),
                       max_LTNP_PET = max(LTNASPO),
                       sd_LTNPPET = sd(LTNASPO),
                       avg_D_CLTNP_PET = mean(DFLTPVP),
                       max_D_CLTNP_PET = max(DFLTPVP),
                       sd_D_CLTNP_PET = sd(DFLTPVP),
                       avg_CAvgMinT = mean(CAvgMinT),
                       max_CAvgMinT = max(CAvgMinT),
                       sd_CAvgMinT = sd(CAvgMinT),
                       avg_CAvgMaxT = mean(CAvgMaxT),
                       max_CAvgMaxT = max(CAvgMaxT),
                       sd_CAvgMaxT = sd(CAvgMaxT),
                       avg_LTAvgMxT = mean(LTAvgMxT),
                       max_LTAvgMxT = max(LTAvgMxT),
                       sd_LTAvgMxT = sd(LTAvgMxT),
                       avg_LTAvgMnT = mean(LTAvgMnT),
                       max_LTAvgMnT = max(LTAvgMnT),
                       sd_LTAvgMnT = sd(LTAvgMnT),
                       n_grids = n())
    
     # Calcule las estadisticas a traves de los admin2S dentro de los admin1s dentro de admin0
    admin2_stats <- 
      weather_template_df %>%
      dplyr::group_by(admin0_name,admin1_name,admin2_name) %>% 
      dplyr::summarise(tot_POP = sum(population_2020),   
                       avg_CSUMPRE = mean(CSUMPRE),
                       max_CSUMPRE = max(CSUMPRE),
                       sd_CSUMPRE = sd(CSUMPRE),
                       avg_LTNsumPre = mean(LTNSUMP),
                       max_LTNsumPre = max(LTNSUMP),
                       sd_LTNsumPre = sd(LTNSUMP),
                       avg_D_CLTNSUMPRE = mean(DFLTSUM),
                       max_D_CLTNSUMPRE = max(DFLTSUM),
                       sd_D_CLTNSUMPRE = sd(DFLTSUM),
                       avg_CP_PET = mean(CPOVRPR),
                       max_CP_PET = max(CPOVRPR),
                       sd_CP_PET = sd(CPOVRPR),
                       avg_LTNP_PET = mean(LTNASPO),
                       max_LTNP_PET = max(LTNASPO),
                       sd_LTNPPET = sd(LTNASPO),
                       avg_D_CLTNP_PET = mean(DFLTPVP),
                       max_D_CLTNP_PET = max(DFLTPVP),
                       sd_D_CLTNP_PET = sd(DFLTPVP),
                       avg_CAvgMinT = mean(CAvgMinT),
                       max_CAvgMinT = max(CAvgMinT),
                       sd_CAvgMinT = sd(CAvgMinT),
                       avg_CAvgMaxT = mean(CAvgMaxT),
                       max_CAvgMaxT = max(CAvgMaxT),
                       sd_CAvgMaxT = sd(CAvgMaxT),
                       avg_LTAvgMxT = mean(LTAvgMxT),
                       max_LTAvgMxT = max(LTAvgMxT),
                       sd_LTAvgMxT = sd(LTAvgMxT),
                       avg_LTAvgMnT = mean(LTAvgMnT),
                       max_LTAvgMnT = max(LTAvgMnT),
                       sd_LTAvgMnT = sd(LTAvgMnT),
                       n_grids = n())
    
     # Combine las estadisticas especificas para cada nivel "admin-specific stats" 
     # con los calculos para toda la region
    admin1_stats <- admin1_stats %>% dplyr::arrange(avg_LTNP_PET)
    admin2_stats <- admin2_stats %>% dplyr::arrange(avg_LTNP_PET)
    
    
    stats_out <- data.table::rbindlist(list(admin0_stats
                                            ,admin1_stats
                                            ,admin2_stats)
                                       ,use.names = TRUE
                                       ,fill = TRUE)
    
     # Lo siguiente reorganiza la informacion calculada - no es un paso necesario pero,
     # uno puede realizar una salida que, al ser leida en Excel,
     # sera facil de interpretar.
    stats_out <- 
      stats_out %>% 
      dplyr::select(1, admin0_name, admin1_name, admin2_name, n_grids
                    ,avg_CSUMPRE, avg_LTNsumPre
                    ,avg_D_CLTNSUMPRE, avg_CP_PET 
                    ,avg_LTNP_PET, avg_D_CLTNP_PET
                    ,avg_CAvgMinT, avg_CAvgMaxT
                    ,avg_LTAvgMnT, avg_LTAvgMxT
                    ,everything())
    
     # Redondee los numeros de las salidas de modo que sean razonables y significativas
    stats_out$avg_CSUMPRE <- round(stats_out$avg_CSUMPRE, 1)
    stats_out$avg_LTNsumPre <- round(stats_out$avg_LTNsumPre, 1)
    stats_out$avg_D_CLTNSUMPRE <- round(stats_out$avg_D_CLTNSUMPRE, 1)
    stats_out$avg_CP_PET <- round(stats_out$avg_CP_PET, 2)
    stats_out$avg_LTNP_PET <- round(stats_out$avg_LTNP_PET, 2)
    stats_out$avg_D_CLTNP_PET <- round(stats_out$avg_D_CLTNP_PET, 2)
    stats_out$avg_CAvgMinT <- round(stats_out$avg_CAvgMinT, 1)
    stats_out$avg_CAvgMaxT <- round(stats_out$avg_CAvgMaxT, 1)
    stats_out$avg_LTAvgMxT <- round(stats_out$avg_LTAvgMxT, 1)
    stats_out$avg_LTAvgMnT <- round(stats_out$avg_LTAvgMnT, 1)
    
     # Cambie el nombre de las columnas de modo que estas sean descriptivas
    names(stats_out)[grep("n_grids",          
           names(stats_out))] <- "No. aWhere Virtual Weather Stations"   
    names(stats_out)[grep("avg_CSUMPRE",      
          names(stats_out))] <- "Total current Precipitation (mm) "
    names(stats_out)[grep("avg_LTNsumPre",    
          names(stats_out))] <- "Average LTN Precipitation (mm)"
    names(stats_out)[grep("avg_D_CLTNSUMPRE", 
          names(stats_out))] <- paste("Difference: Current"
                , "Precipitation vs. LTN Precipitation (mm)")
    names(stats_out)[grep("avg_CP_PET",       
          names(stats_out))] <- "Current P/PET"
    names(stats_out)[grep("avg_LTNP_PET",     
          names(stats_out))] <- "LTN P/PET"
    names(stats_out)[grep("avg_D_CLTNP_PET",  
          names(stats_out))] <- "Difference: Current P/PET vs LTN P/PET"
    names(stats_out)[grep("avg_CAvgMinT",     
          names(stats_out))] <- "Average Current Minimum Temperature"
    names(stats_out)[grep("avg_CAvgMaxT",     
          names(stats_out))] <- "Average Current Maximum Temperature"
    names(stats_out)[grep("avg_LTAvgMxT",     
          names(stats_out))] <- "LTN Maximum Temperature"
    names(stats_out)[grep("avg_LTAvgMnT",     
          names(stats_out))] <- "LTN Minimum Temperature"
    
     # Escriba estadisticas administrativas en el archivo
    write.csv(stats_out,
              paste0(csv_path, "/stats_", subarea_select_name
                     ,"_", weather_name))
    
     # Guarde el archivo combinado de los datos meteorologicos y datos "template" ("weather-template file")
    write.csv(weather_template_df,
              paste0(csv_path, "/weatherTemplate_", subarea_select_name
                     ,"_", weather_name))
    
     # Guarde esta plantilla del subconjunto, que puede ser utilizada como archivo "template"
     # mas adelante.
    write.csv(template_df,
              paste0(csv_path, "/template_", subarea_select_name, ".csv"))
    
     # Filtre/Recorte Valores Extremos -------------------------------------
     #
     # Use las funciones de apoyo ("support functions") para establecer los valores minimos/maximos de cada variable.
     # Todos los valores anteriores/posteriores estan limitados a estos valores
    weather_template_df$cPovPET  <- ClipValues(weather_template_df$CPOVRPR, 
                                               max.thresh = 2)
    weather_template_df$cLTNPPET <- ClipValues(weather_template_df$LTNASPO, 
                                               max.thresh = 2)
    weather_template_df$aPre     <- ClipValues(weather_template_df$CSUMPRE, 
                                               max.thresh = 300)
    weather_template_df$aLTNPRE  <- ClipValues(weather_template_df$LTNSUMP, 
                                               max.thresh = 400)
    weather_template_df$aDinPre  <- ClipValues(weather_template_df$DFLTSUM, 
                                               max.thresh = 250,
                                               min.thresh = -250)
    weather_template_df$DFLTPVP  <- ClipValues(weather_template_df$DFLTPVP, 
                                               max.thresh = 2,
                                               min.thresh = -2)
    weather_template_df$ct1      <- ClipValues(weather_template_df$CSUMPRE, 
                                               max.thresh = 700)
    weather_template_df$LTNt1    <- ClipValues(weather_template_df$LTNSUMP, 
                                               max.thresh = 700)
    weather_template_df$ctemp    <- ClipValues(weather_template_df$CPOVRPR, 
                                               max.thresh = 2)
    weather_template_df$LTNtemp  <- ClipValues(weather_template_df$LTNASPO, 
                                               max.thresh = 2)
     # ---------------------------------------------------------------------
     # MAPAS 
     # ---------------------------------------------------------------------
     #
     # Primero, descargue las imagenes para la base o fondo de los mapas.
     # Expanda los valores de la columna "wkt" en un formato que pueda ser utilizado por ggplot
    polygon_df <- tibble::as_tibble(
                  wicket::wkt_coords(weather_template_df$shapewkt))
    
     # Obtenga el mapa que sera utilizado como base para todas las imagenes con mapas.
     # Defina un area limita con base en la extension de los datos.
    bounding_box <- make_bbox(lon = polygon_df$lng
                             ,lat = polygon_df$lat
                             ,f = .075)
    
    region_base_map <- get_map(location = bounding_box
                               ,maptype = "toner"
                               ,source = "stamen")
    
     # Configure los datos para que puedan ser mapeados en un formato apropiado
     #
     # Las lineas a continuacion utilizan el indice de cada fila ("row index") que se expresa en la 
     # columna-objeto para combinar los datos con el objeto polygon_df
    polygon_df$aPre     <- weather_template_df$aPre[polygon_df$object]
    polygon_df$cPovPET  <- weather_template_df$cPovPET[polygon_df$object]
    polygon_df$aDinPre  <- weather_template_df$aDinPre[polygon_df$object]
    polygon_df$CAvgMaxT <- weather_template_df$CAvgMaxT[polygon_df$object]
    polygon_df$DFLTMaxT <- weather_template_df$DFLTMaxT[polygon_df$object]
    polygon_df$DFLTPVP  <- weather_template_df$DFLTPVP[polygon_df$object]
    
     # Cree un mapa para cada una de las variables especificadas
     #
     # Temperatura Maxima, actual ------------------------------------------
    max_temp_title <- paste0("Maximum Temperature\n"
                             ,subarea_select_name, "\n"
                             ,tools::file_path_sans_ext(weather_name))
    
    max_temp_map <- ggmap(region_base_map) +
      geom_polygon(aes(x = lng
                       ,y = lat
                       ,group = object
                       ,fill = CAvgMaxT)
                   ,data = polygon_df
                   ,alpha = 0.7) +
      scale_fill_gradient2(breaks = seq(10, 42, by = 4)
                           ,low = "blue"
                           ,mid = "green"
                           ,high = "red"
                           ,midpoint = 26
                           ,limits = c(10,42)
                           ,name="Max Temperature \n(C)") +
      ggtitle(max_temp_title)
    
    print(max_temp_map)
    
     # Guarde el mapa en un archivo
    ggsave(max_temp_map
           ,filename = paste0(figures_path, "/"
                ,formatGraphTitleForFileName(max_temp_title), "_map.png")
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # Temperatura Maxima, actual vs. LTN ----------------------------------
    max_temp_LTN_title <- paste0("Current Max Temperature compared to LTN\n"
                            ,subarea_select_name,"\n"
                            ,tools::file_path_sans_ext(weather_name))
    
    max_temp_LTN_map <- ggmap(region_base_map) +
      geom_polygon(aes(x = lng
                       ,y = lat
                       ,group = object
                       ,fill = DFLTMaxT)
                   ,data = polygon_df
                   ,alpha = 0.7) +
      scale_fill_gradient2(breaks = seq(-5, 5, by = 1)
                           ,low = "blue"
                           ,mid = "gray"
                           ,high = "red" 
                           ,midpoint = 0
                           ,limits = c(-5,5)
                           ,name="Max Temperature  \n vs. LTN Max T Deg C") +
      ggtitle(max_temp_LTN_title)
    
    print(max_temp_LTN_map)
    
     # Guarde el mapa en un archivo
    ggsave(max_temp_LTN_map
           ,filename = paste0(figures_path, "/"
              ,formatGraphTitleForFileName(max_temp_LTN_title), "_map.png") 
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # Precipitacion, actual -----------------------------------------------
    precip_title <- paste0("Precipitation\n"
                           ,subarea_select_name, "\n"
                           ,tools::file_path_sans_ext(weather_name))
    
    precip_map <- ggmap(region_base_map) +
      geom_polygon(aes(x = lng
                       ,y = lat
                       ,group = object
                       ,fill = aPre)
                   ,data = polygon_df
                   ,alpha = 0.7) +
      scale_fill_gradient2(breaks = seq(0,300, by = 50)
                           ,low = "red"
                           ,mid = "green"
                           ,high = "blue"
                           ,midpoint = 150
                           ,limits = c(0,300)
                           ,name="Precipitation \n(mm)") +
      ggtitle(precip_title)
    
    print(precip_map)
    
     # Guarde el mapa en un archivo
    ggsave(precip_map
           ,filename = paste0(figures_path, "/"
                ,formatGraphTitleForFileName(precip_title), "_map.png")
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # Precipitacion, actual vs. LTN ---------------------------------------
    precip_ltn_title <- paste0("Current Precipitation vs LTN\n"
                         ,subarea_select_name, "\n"
                         ,tools::file_path_sans_ext(weather_name))
    
    precip_ltn_map <- ggmap(region_base_map) +
      geom_polygon(aes(x = lng
                       ,y = lat
                       ,group = object
                       ,fill = aDinPre)
                   ,data = polygon_df
                   ,alpha = 0.9) +
      scale_fill_gradient2(breaks = seq(-250,250, by = 50)
                           ,low = "red"
                           ,mid = "white"
                           ,high = "blue"
                           ,midpoint = 0
                           ,limits = c(-250,250)
                           ,name= "Difference \nP vs LTN P (mm)") +
      ggtitle(precip_ltn_title)
    
    print(precip_ltn_map)
    
     # Guarde el mapa en un archivo
    ggsave(precip_ltn_map
           ,filename = paste0(figures_path, "/"
                ,formatGraphTitleForFileName(precip_ltn_title), "_map.png")
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # P/PET, actual -------------------------------------------------------
     #
     # (P / PET = Razon de la precipitacion sobre la PET)
    ppet_title <- paste0("P/PET\n"
                         ,subarea_select_name, "\n"
                         ,tools::file_path_sans_ext(weather_name), "\n")
    
    ppet_map <- ggmap(region_base_map) +
      geom_polygon(aes(x = lng
                       ,y = lat
                       ,group = object
                       ,fill = cPovPET)
                   ,data = polygon_df
                   ,alpha = 0.7) +
      scale_fill_gradient2(breaks = seq(0,2, by = 0.2)
                           ,low = "red"
                           ,mid = "green"
                           ,high = "blue"
                           ,midpoint = 1.0
                           ,limits = c(0,2.0)
                           ,name="P/PET") +
      ggtitle(ppet_title)
    
    print(ppet_map)
    
     # Guarde el mapa en un archivo
    ggsave(ppet_map
           ,filename = paste0(figures_path, "/"
                    ,formatGraphTitleForFileName(ppet_title), "_map.png")
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # P/PET, actual vs. LTN -----------------------------------------------
    ppet_vs_ltn_title <- paste0("Current P/PET vs LTN\n"
                               ,subarea_select_name, "\n"
                               ,tools::file_path_sans_ext(weather_name))
    ppet_vs_ltn_map <- 
      ggmap(region_base_map) +
      geom_polygon(aes(x = lng
                       ,y = lat
                       ,group = object
                       ,fill = DFLTPVP)
                   ,data = polygon_df
                   ,alpha = 0.7) +
      scale_fill_gradient2(breaks = seq(-2, 2, by = 0.5)
                           ,low = "red"
                           ,mid = "green"
                           ,high = "blue" 
                           ,midpoint = 0
                           ,limits = c(-2,2),
                           name="Current PPET \nvs. LTN PPET") +
      ggtitle(ppet_vs_ltn_title)
    
    print(ppet_vs_ltn_map)
    
     # Guarde el mapa en un archivo
    ggsave(ppet_vs_ltn_map
           ,filename = paste0(figures_path, "/"
                ,formatGraphTitleForFileName(ppet_vs_ltn_title), "_map.png")
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # ---------------------------------------------------------------------
     # HISTOGRAMAS 
     # ---------------------------------------------------------------------
     #
     # Histograma de Temperatura Minima, actual vs. LTN --------------------
    hist_title <- paste0("Min Temp: Current vs LTN\n"
                         ,subarea_select_name, "\n"
                         ,tools::file_path_sans_ext(weather_name))
    
    aWhereCharts::generateaWhereHistogram(data = weather_template_df
                                          ,variable = "CAvgMinT"
                                          ,title = hist_title
                                          ,xlabel = "Deg C"
                                          ,compare = TRUE
                                          ,compare_var = "LTAvgMnT")
    
     # Escriba el histograma en un archivo
    if (write_hist == TRUE) {
      ggplot2::ggsave(filename = paste0(figures_path, "/"
                      ,formatGraphTitleForFileName(hist_title), ".png")
                      ,width = fig_dim_hist["width"]
                      ,height = fig_dim_hist["height"]
                      ,units = "in"
                      ,device = "png")
    }
    
     # Histograma de Temperatura Maxima, actual vs. LTN --------------------
    hist_title <- paste0("Max Temp: Current vs LTN\n"
                         ,subarea_select_name, "\n"
                         ,tools::file_path_sans_ext(weather_name))
    
    aWhereCharts::generateaWhereHistogram(data = weather_template_df
                                          ,variable = "CAvgMaxT"
                                          ,title = hist_title
                                          ,xlabel = "Deg C"
                                          ,compare = TRUE
                                          ,compare_var = "LTAvgMxT")
    
     # Escriba el histograma en un archivo
    if (write_hist == TRUE) {
      ggplot2::ggsave(filename = paste0(figures_path, "/"
                      ,formatGraphTitleForFileName(hist_title), ".png")
                      ,width = fig_dim_hist["width"]
                      ,height = fig_dim_hist["height"]
                      ,units = "in"
                      ,device = "png")
    }
    
     # Histograma de Precipitacion, actual vs. LTN -------------------------
    hist_title <- paste0("Precipitation: Current vs LTN\n"
                         ,subarea_select_name 
                         ,tools::file_path_sans_ext(weather_name))
    
    aWhereCharts::generateaWhereHistogram(data = weather_template_df
                                          ,variable = "ct1"
                                          ,compare = TRUE
                                          ,compare_var = "LTNt1"
                                          ,title = hist_title
                                          ,xlabel = "mm")
    
     # Escriba el histograma en un archivo
    if (write_hist == TRUE) {
      ggplot2::ggsave(filename = paste0(figures_path, "/"
                      ,formatGraphTitleForFileName(hist_title), ".png")
                      ,width = fig_dim_hist["width"]
                      ,height = fig_dim_hist["height"]
                      ,units = "in"
                      ,device = "png")
    }
    
     # Histograma del indice P/PET, actual vs. LTN -------------------------
    hist_title <- paste0("PPET: Current vs LTN\n"
                         ,subarea_select_name, "\n" 
                         ,tools::file_path_sans_ext(weather_name))
    
    aWhereCharts::generateaWhereHistogram(data = weather_template_df
                                          ,variable = "ctemp" 
                                          ,title = hist_title 
                                          ,xlabel = "P/PET" 
                                          ,compare = TRUE 
                                          ,compare_var = "LTNtemp") 
    
     # Escriba el histograma en un archivo
    if (write_hist == TRUE) {
      ggplot2::ggsave(filename = paste0(figures_path, "/"
                      ,formatGraphTitleForFileName(hist_title), ".png")
                      ,width = fig_dim_hist["width"]
                      ,height = fig_dim_hist["height"]
                      ,units = "in"
                      ,device = "png")
    }
    
     # ---------------------------------------------------------------------
     # ESTADISTICAS (Tablas-resumen de los Histogramas)
     # ---------------------------------------------------------------------
     #
     # Tabla resumen de la Precipitacion -----------------------------------
     #
     # Añada una columna que exprese en que contendor/rango de contenedores se incluira cada grilla
    weather_template_df$bin.precip <- NA
    weather_template_df$bin.ltn.precip <- NA
    weather_template_df$bin.range.precip <- NA
    
     # Cree una estructura que albergue los nombres de cada contenedor
    bin_names_precip <- data.frame(bins = seq(1, length(bins_precip) - 1, 1)
                                  ,range = ""
                                  ,stringsAsFactors = FALSE)
    
     # Ejecute un ciclo en cada contenedor y agreguele los valores apropiados.
     # Por ejemplo, calculando de forma manual el histograma. Al hacerlo de esta forma,
     # los nombres que se generan para cada contenedor se organizan de una mejor manera.
    for(b in 1:(length(bins_precip) - 1)) {
      
      bin_names_precip[b, "range"] <- paste(as.character(bins_precip[b])
                                          ,as.character(bins_precip[b + 1])
                                          ,sep = " - ")
      
       # Indices de entradas que caen/se incluyen en el contenedor actual
      idx <- weather_template_df$CSUMPRE >= bins_precip[b] & 
             weather_template_df$CSUMPRE < bins_precip[b + 1]
      
      # Añada el numero de contenedor a cada fila
      weather_template_df$bin.precip[idx] <- b
      
       # Indices de entradas que caen/se incluyen en el contenedor actual
      idx <- weather_template_df$LTNSUMP >= bins_precip[b] & 
             weather_template_df$LTNSUMP < bins_precip[b + 1]           
      
       # Añada el numero de contenedor a cada fila
      weather_template_df$bin.ltn.precip[idx] <- b
    }
    
     # Agregue columnas para el numero de grillas por nivel de precipitacion, para el 
     # porcentaje de todas las grillas por nivel de precipitacion y para la poblacion afectada
     # por nivel de precipitacion
    grids_per_precip_level <- 
      weather_template_df %>% 
      dplyr::group_by(bin.precip) %>%
      dplyr::mutate(grid.count.precip = n()
                  ,grid.percent.precip = 100 * n() / nrow(weather_template_df)
                  ,population.count.precip = round(sum(population_2020))) %>%
      dplyr::group_by(bin.ltn.precip) %>% 
      dplyr::mutate(grid.count.ltn.precip = n()
              ,grid.percent.ltn.precip = 100 * n() / nrow(weather_template_df)
              ,population.count.ltn.precip = round(sum(population_2020))) %>%
      dplyr::select(bin.precip
                    ,bin.ltn.precip
                    ,grid.count.precip
                    ,grid.percent.precip
                    ,grid.count.ltn.precip
                    ,grid.percent.ltn.precip
                    ,population.count.precip
                    ,population.count.ltn.precip) %>%
      dplyr::arrange(bin.precip)
    
     # Combine los datos de la precipitacion actual y LTN de modo que la clave principal en
     # la tabla sea una unica columna que contiene los datos del contenedor de precipitacion
    current_and_ltn_precip <- 
      dplyr::full_join(unique(grids_per_precip_level[c("bin.precip"
        ,"grid.count.precip", "grid.percent.precip"
        ,"population.count.precip")])
        ,unique(grids_per_precip_level[c("bin.ltn.precip"
        ,"grid.count.ltn.precip", "grid.percent.ltn.precip"
        ,"population.count.ltn.precip")])
                       ,by = c("bin.precip" = "bin.ltn.precip")) %>%
      dplyr::arrange(bin.precip)
    
     # reemplace los valores NA por 0
    current_and_ltn_precip[is.na(current_and_ltn_precip)] <- 0
    
     # Agregue los nombres de cada contenedor
    current_and_ltn_precip <- 
      dplyr::inner_join(current_and_ltn_precip
                        ,bin_names_precip
                        ,by = c("bin.precip" = "bins")) %>%
      dplyr::arrange(bin.precip)
    
     # redondee los valores de los porcentajes a tres valores decimales
    current_and_ltn_precip$grid.percent.precip <- 
      round(current_and_ltn_precip$grid.percent.precip, digits = 2)
    current_and_ltn_precip$grid.percent.ltn.precip <- 
      round(current_and_ltn_precip$grid.percent.ltn.precip, digits = 2)
    
     # escriba el numero de celdas de grilla del nivel de precipitacion actual y LTN en un archivo .csv
    write.csv(current_and_ltn_precip, 
              file = paste0("outputCSVs/",
                            tools::file_path_sans_ext(weather_name),
                            "_grids_per_current_and_ltn_precip_level.csv"))
    
     # Tabla-resumen del indice P/PET --------------------------------------
     #
     # Añada una columna que exprese en que contendor se incluira cada grilla
    weather_template_df$bin.ppet <- NA
    weather_template_df$bin.ltn.ppet <- NA
    weather_template_df$bin.range.ppet <- NA
    
     # Haga una estructura de datos que albergue los nombres de los contenedores
    bin_names_ppet <- data.frame(bins = seq(1,length(bins_ppet) - 1, 1)
                                ,range = ""
                                ,stringsAsFactors = FALSE)
    
     # Haga un ciclo a traves de cada contenedor para incluir en el los valores apropiados
    for(b in 1:(length(bins_ppet) - 1)){
      
      bin_names_ppet[b, "range"] <- paste(as.character(bins_ppet[b])
                                        ,as.character(bins_ppet[b + 1])
                                        ,sep = " - ")
      
       # Indices de entradas que caen/se incluyen en el contenedor actual
      idx <- weather_template_df$CPOVRPR >= bins_ppet[b] & 
        weather_template_df$CPOVRPR < bins_ppet[b + 1]           
      
       # Agregue el numero de contenedor a cada fila
      weather_template_df$bin.ppet[idx] <- b
      
       # Indices de entradas que caen/se incluyen en el contenedor actual
      idx <- weather_template_df$LTNASPO >= bins_ppet[b] & 
        weather_template_df$LTNASPO < bins_ppet[b + 1]           
      
       # Agregue el numero de contenedor a cada fila
      weather_template_df$bin.ltn.ppet[idx] <- b
    }
    
     # Agregue columnas para el numero de grillas por nivel de la ppet, el porcentaje de todas las grillas
     # por nivel de ppet y la poblacion afectada por nivel de ppet
    grids_per_ppet_level <- 
      weather_template_df %>% 
      dplyr::group_by(bin.ppet) %>%
      dplyr::mutate(grid.count.ppet = n()
                    ,grid.percent.ppet = 100 * n() / nrow(weather_template_df)
                    ,population.count.ppet = round(sum(population_2020))) %>%
      dplyr::group_by(bin.ltn.ppet) %>% 
      dplyr::mutate(grid.count.ltn.ppet = n()
                ,grid.percent.ltn.ppet = 100 * n() / nrow(weather_template_df)
                ,population.count.ltn.ppet = round(sum(population_2020))) %>%
      dplyr::select(bin.ppet
                    ,bin.ltn.ppet
                    ,grid.count.ppet
                    ,grid.percent.ppet
                    ,grid.count.ltn.ppet
                    ,grid.percent.ltn.ppet
                    ,population.count.ppet
                    ,population.count.ltn.ppet) %>%
      dplyr::arrange(bin.ppet)
    
     # Combine los datos de la ppet actual y LTN de modo que la clave principal en
     # la tabla sea una unica columna que contiene los datos del contenedor de la ppet
    current_and_ltn_ppet <- 
      dplyr::full_join(unique(grids_per_ppet_level[c("bin.ppet"
                      ,"grid.count.ppet", "grid.percent.ppet"
                      ,"population.count.ppet")])
                       ,unique(grids_per_ppet_level[c("bin.ltn.ppet"
                       ,"grid.count.ltn.ppet", "grid.percent.ltn.ppet"
                       ,"population.count.ltn.ppet")])
                       ,by = c("bin.ppet" = "bin.ltn.ppet")) %>%
      dplyr::arrange(bin.ppet)
    
     # Reemplace los valores NA por 0
    current_and_ltn_ppet[is.na(current_and_ltn_ppet)] = 0
    
     # Agregue los nombres de cada contenedor
    current_and_ltn_ppet <- 
      dplyr::inner_join(current_and_ltn_ppet
                        ,bin_names_ppet
                        ,by = c("bin.ppet" = "bins")) %>%
      dplyr::arrange(bin.ppet)
    
     # Redondee los valores de los porcentajes a tres valores decimales
    current_and_ltn_ppet$grid.percent.ppet <- 
      round(current_and_ltn_ppet$grid.percent.ppet, digits = 2)
    current_and_ltn_ppet$grid.percent.ltn.ppet <- 
      round(current_and_ltn_ppet$grid.percent.ltn.ppet, digits = 2)
    
     # Escriba el numero de grillas por nivel de la ppet actual y LTN en un archivo .csv
    write.csv(current_and_ltn_ppet, 
              file = paste0("outputCSVs/"
                            ,tools::file_path_sans_ext(weather_name)
                            ,"_grids_per_current_and_ltn_ppet_level.csv"))
    
     # Genere el histograma de la Poblacion por contenedor de Precipitacion -----
     #
     # Esto ajusta los histogramas anteriores para que representen el numero de personas
     # afectadas por cada nivel de precipitacion
     #
     # Establezca el titulo de la figura
    hist_title <- paste0("Precip vs Population\n"
                         ,subarea_select_name , "\n"
                         ,tools::file_path_sans_ext(weather_name))
    
     # Obtenga las columnas de interes para el histograma de poblacion por contenedor de precipitacion
    chart_data <- current_and_ltn_precip[, c("bin.precip" 
                                             ,"population.count.precip" 
                                             ,"population.count.ltn.precip")]
    
     # Cambie el nombre de las columnas que describen las variables de la poblacion a "Current" y "LTN"
     # para las siguientes etiquetas del histograma
    colnames(chart_data) <- c("bin.precip", "Current", "LTN")
    
     # Establezca el formato de datos como "long"
    chart_data <- tidyr::gather(chart_data
                                ,key = Variable 
                                ,value = population 
                                ,2:ncol(chart_data))
    
     # Establezca la escala del eje x tomando como base el rango del contenedor de precipitacion.
     # Al utilizar esta continuidad en la escala del eje x, se estableceran las divisiones
     # del eje x en el histograma (contenedor 1, contenedor 2, .... contenedor n).
     # Las etiquetas seran los valores actuales de la precipitacion (en mm) y seran escritas en el eje x.
    breaks_precip <- sort(unique(chart_data$bin.precip))
    x_scale <- scale_x_continuous(breaks = unique(breaks_precip),
        labels = sapply(strsplit(bin_names_precip[
          which(bin_names_precip$bins %in% breaks_precip), ]$range
          , " - "), "[", 1))
    
     # Establezca el formato y la estetica del grafico tomando como base el comando aWhereCharts::generateaWhereHistogram()
     # Variables para el titulo y tamaño de la fuente de las etiquetas
    size_font_main_title <- 16
    size_font_axis_titles <- 14
    size_font_axis_labels <- 14
    size_font_legend_entries <- 14
    
     # escala de colores
    colorScaleToUse <- scale_colour_manual(values = c("#1696AC", "#FF810E")) 
    colorFillToUse <- scale_fill_manual(values = c("#1696AC", "#FF810E")) 
    
     # Haga el histograma

     # use los datos del grafico en formato "long"
    ggplot(data = chart_data, 
            # contenedor de precipitacion en el eje x. Agregue 0.5 para cambiar las barras.
           aes(x = bin.precip + 0.5 
                # poblacion en el eje y. 
               ,y = population 
                # coloree las barras de forma diferente para las variables Actuales y LTN.  
               ,col = Variable 
                # coloree las barras de forma diferente para las variables Actuales y LTN.
               ,fill = Variable)) + 
       # establezca la opacidad de las barras
      geom_col(alpha = 0.4, 
                # apile las bardas una encima de otra
               position = "identity") + 
      ggplot2::theme_bw() + 
       # establezca el formato del eje x utilizando los rangos y etiquetas definidos arriba
      x_scale +
       # remueva la notacion cientifica del eje y.  
      scale_y_continuous(labels = scales::comma) + 
       # coloque una barra de colores de leyenda debajo del eje x
      theme(legend.position="bottom", 
            legend.direction="horizontal",
            legend.title = element_blank(),
            legend.spacing.x = unit(0.3,"cm")
             #color de la fuente 
            ,axis.text.x = element_text(color = "grey20", 
                                        
                                         # tamaño de la fuente 
                                        size = size_font_axis_labels,  
                                        angle = 45,
                                         # ajuste horizontal
                                        hjust = 1,      
                                         # tipo de fuente "plain", "negrita"
                                        face = "plain")  
            ,axis.text.y = element_text(color = "grey20", 
                                        size = size_font_axis_labels, 
                                        face = "plain")
            ,axis.title.y = element_text(color = "grey20", 
                                         size = size_font_axis_titles, 
                                         face = "bold")
            ,axis.title.x = element_text(color = "grey20", 
                                         size = size_font_axis_titles, 
                                         face = "bold")
            ,legend.text=element_text(size=size_font_legend_entries)) +
      labs(x = "Precipitation [level mm]", 
           y = "Population (# of people)") +
       # establezca el titulo principal
      ggtitle(hist_title) + 
      colorScaleToUse + 
      colorFillToUse + 
       # tamaño del titulo principal
      theme(plot.title = element_text(size=size_font_main_title)) 
    
     # Escriba el histograma en un archivo
    ggplot2::ggsave(filename = paste0(figures_path, "/"
                    ,formatGraphTitleForFileName(hist_title), ".png")
                    ,width = fig_dim_hist["width"]
                    ,height = fig_dim_hist["height"]
                    ,units = "in"
                    ,device = "png")
    
     # Poblacion por contenedor P/PET --------------------------------------
     #
     # Establezca el titulo de la figura
    hist_title <- paste0("PPET vs Population\n"
                         ,subarea_select_name, "\n" 
                         ,tools::file_path_sans_ext(weather_name))
    
     # Establezca las columnas de interes para el histograma de poblacion por contenedor de ppet
    chart_data <- current_and_ltn_ppet[, c("bin.ppet" 
                                           ,"population.count.ppet" 
                                           ,"population.count.ltn.ppet")]
    
     # Cambie el nombre de las columnas que describen las variables de la poblacion a "Current" y "LTN"
     # para las siguientes etiquetas del histograma 
    colnames(chart_data) <- c("bin.ppet", "Current", "LTN")
    
     # Cambie el formato de los datos por "long"
    chart_data <- tidyr::gather(chart_data 
                                ,key = Variable 
                                ,value = population 
                                ,2:ncol(chart_data))
    
     # Establezca la escala del eje x tomando como base el rango del contenedor ppet.
     # Al utilizar esta continuidad en la escala del eje x, se estableceran las divisiones
     # del eje x en el histograma (contenedor 1, contenedor 2, .... contenedor n).
     # Las etiquetas seran los valores actuales de la P/PET y seran escritas en el eje x.
    breaks_ppet <- sort(unique(chart_data$bin.ppet))
    
    x_scale <- scale_x_continuous(breaks = unique(chart_data$bin.ppet)
              ,labels = sapply(strsplit(bin_names_ppet[
               which(bin_names_ppet$bins %in% breaks_ppet),]$range
              , " - "), "[", 1))
    
     # haga el histograma

     # utilice los datos del grafico en formato "long"
    ggplot(data = chart_data, 
            # contenedor de ppet en el eje x. Agregue 0.5 para cambiar las barras.
           aes(x = bin.ppet + 0.5 
                # poblacion en el eje y
               ,y = population 
                # coloree las barras de forma distinta para las variables Actuales y LTN.
               ,col = Variable 
                # coloree las barras de forma distinta para las variables Actuales y LTN.
               ,fill = Variable)) + 
       # establezca la opacidad de las barras.
      geom_col(alpha = 0.4,  
                # apile las barras una encima de otra.
               position = "identity") + 
      ggplot2::theme_bw() + 
       # formatee el eje x utilizando los rangos y etiquetas anteriores.
      x_scale + 
       # remueva la notacion cientifica del eje y
      scale_y_continuous(labels = scales::comma) + 
       # coloque una barra de colores de leyenda debajo del eje x
      theme(legend.position="bottom", 
            legend.direction="horizontal",
            legend.title = element_blank(),
            legend.spacing.x = unit(0.3,"cm")
             # color de la fuente
            ,axis.text.x = element_text(color = "grey20", 
                                         # tamaño de la fuente
                                        size = size_font_axis_labels, 
                                        angle = 45,
                                         # ajuste horizontal
                                        hjust = 1,        
                                         # tipo de fuente "plain", "negrita"
                                        face = "plain") 
            ,axis.text.y = element_text(color = "grey20", 
                                        size = size_font_axis_labels, 
                                        face = "plain")
            ,axis.title.y = element_text(color = "grey20", 
                                         size = size_font_axis_titles, 
                                         face = "bold")
            ,axis.title.x = element_text(color = "grey20", 
                                         size = size_font_axis_titles, 
                                         face = "bold")
            ,legend.text=element_text(size=size_font_legend_entries)) +
      labs(x = "Precipitation [level mm]", 
           y = "Population (# of people)") +
       # establezca el titulo principal
      ggtitle(hist_title) +
      colorScaleToUse + 
      colorFillToUse + 
       # tamaño del titulo principal
      theme(plot.title = element_text(size=size_font_main_title))  
    
     # Escriba el histograma como un archivo
    ggplot2::ggsave(filename = paste0(figures_path,"/",formatGraphTitleForFileName(hist_title), ".png")
                    ,width = fig_dim_hist["width"]
                    ,height = fig_dim_hist["height"]
                    ,units = "in"
                    ,device = "png")
  }
}

 # Despliegue un mensaje de estado en la consola

cat("Completed Processing all files and all Regions")

 # ¡Revise las carpetas "figures" y "outputCSVs" en su directorio de trabajo
 # para ver las salidas de este script!

