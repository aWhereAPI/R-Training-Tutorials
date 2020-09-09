#--------------------------------------------------------------------------
 # aWhere R Tutorial: Maps, Histograms and Statistics
 #
 # Purpose of script:
 # This script examines a region and returns maps, histograms and charts 
 # for the region and the sub-regions within it. 
 #
 # You will need to be connected to the internet to run this script.
 #
 #--------------------------------------------------------------------------
 # Install and load packages -----------------------------------------------
 #
 # Clear your environment and remove all previous variables
rm(list = ls())

 # Load the packages needed for this script
library(ggmap)
library(ggplot2)
library(dplyr)  
library(wicket)
library(aWhereAPI)
library(aWhereCharts)
library(data.table)

 # Set working directory ---------------------------------------------------
 #
 # The working directory is the location on your computer where R will 
 # automatically save the output files created by this script.
 #
 # To set your working directory, find the folder on your computer where you 
 # would like the outputs of this script to be saved, copy the pathfile name 
 # and paste it over the phrase, "YOUR WD HERE"
working_dir <- "YOUR WD HERE" 

 # This sets your working directory to the working_dir path
setwd(working_dir) 
                   
 # Supporting functions ----------------------------------------------------
 #
 # This script requires you to load a supporting functions file that is 
 # typically saved in the Source folder in your aWhere tutorial folder 
 # structure. This step loads additional functions required to create
 # the climatology chart. Copy the path name and paste it over the 
 # phrase, "YOUR PATHNAME".  
source("YOUR PATHNAME/supporting_functions.R")

 # Load external data: template and ag-weather files ------------------------------------
 #
 # Load WEATHER data file(s).
 # These may be provided to you but you can also download them through the
 # interactive maps portal at apps.awhere.com. There are exmaples of data 
 # in the BaseData folder too. This can be any number of files, individual 
 # files will be looped over.
 #
 # CHANGE THIS to be your path to a weather file
weather_file_list <- c("YOUR PATHFILE HERE/190613_past7.csv") 

 # Load TEMPLATE data file.
 # In the RunSet folder, there is a file named similarly to "Template___.csv"
 # This file includes shapewkt (polygon) column as well as population, 
 # soil type, and other data sets for each aWhere grid cell. Please contact aWhere for a 
 # template for your area of interes
 #
 # CHANGE THIS to be your path to the template file
template_file <- "RunSet/Template____.csv" 

 # Region(s) of interest ---------------------------------------------------
 #
 # Define the region(s) for which you want to generate maps, histograms, and 
 # statistics based on columns in the template file.
 # Each pairing will be independtly considered (i.e. it is NOT condition1 AND 
 # condition2 AND condition3).
 # If a comma-separated list is given, it will subset on all units in that list.
 # If set to "ENTIRE_REGION" the entire region will be run.  
 # Use the following syntax to subset to specific subregions based on any 
 # available attribute. Ability to subset on multiple attributes will be added 
 # later.
 #
 # subarea_select <- c("admin0_name: Kenya"
 #                    ,"admin1_name: Wajir, Kitui, Makueni"
 #                    ,"soiltype: MEDIUM")
 #
subarea_select <- c("admin0_name: Zambia"
                    ,"admin1_name: Luapula, Lusaka, Western, Nyamira")

#--------------------------------------------------------------------------
 # DO NOT MODIFY these lines - further parameter setting
 #
 # Write histograms to image files (TRUE or FALSE)
write_hist <-  TRUE

 # Set the dimensions for any graphs and histograms that are output
fig_dim_graph <- c(height = 3.38, width = 6.02)
fig_dim_hist  <- c(height = 6, width = 10)

 # Bins for tabular summaries of histogram data
 #
 # Precipitation
bins_precip <- c(seq(from = 0, to = 300, by = 12.5), Inf)

 # P/PET
bins_ppet <- c(0, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
               , 1.0, 1.1, 1.2, 1.4, 1.6, 2.0, Inf) 
#--------------------------------------------------------------------------

 # Beginning of Analysis
 #
 # Begin looping over the specified weather data files. 
 # Each file contains weather data for specific days. 
for (j in 1:length(weather_file_list))  {    
  
   # Load and process the weather file to be studied -----------------------
   #
   # Record the name of the current file
  weather_file <- weather_file_list[j]
  
   # Since the file might be specified using the path, get the filename
  weather_name  <- tail(strsplit(x = weather_file
                                 ,split = "/"
                                 ,fixed = TRUE)[[1]]
                        ,n = 1)
  
   # read the weather data
  weather_df <- read.csv(weather_file)
  
   # Loop through each region and/or subregion to be indepently analyzed ------------------------------------
  for (x in 1:length(subarea_select)) {
    
     # Process the template file for the current run
     #
     # read the template data. remove columns that are duplicated in the 
     # weather file
    template_df <- 
      read.csv(template_file) %>% 
      dplyr::select( -c(shapewkt, longitude, latitude))
    
     # Parse the subsetting info so we know which data to look at
    subarea_select_list <- 
      strsplit(x = subarea_select[x]
               ,split = ":"
               ,fixed = TRUE) %>%
      lapply(.,trimws)
    
     # In the event the user provided a comma separated list, parse that
    subarea_select_list[[2]] <- 
      strsplit(x = subarea_select_list[[1]][2]
               ,split = ","
               ,fixed = TRUE)[[1]] %>%
      lapply(.,trimws) %>%
      unlist(.) %>%
      as.vector(.)
    
     # If ENTIRE REGION was selected, assign words to the following variables
     # to the name formats properly
    if (any(is.na(subarea_select_list[[2]])) == TRUE) {
      subarea_select_list[[1]][1] <- "ENTIRE"
      subarea_select_list[[2]] <- "REGION"
    }
    
     # Combine the above into a name that can be used for saving files
    subarea_select_name <- paste(subarea_select_list[[1]][1]
                                 ,paste0(subarea_select_list[[2]]
                                         ,collapse = "_"),sep = "_")
    
     # Filter the data set for subregion(s) of interest
     # and write this clipped data set to file. It can become a template
     # for a forecast. 
    if (subarea_select_name != "ENTIRE_REGION"){ 
      template_df <- eval(parse(text = paste0("template_df[template_df$"
                                              ,subarea_select_list[[1]][1]
                                    ," %in% subarea_select_list[[2]],]")))
    } 
    
     # Determine the path that will be used for savin th efiles
    csv_path <- paste0("outputCSVs/", subarea_select_name)
    figures_path <- paste0('figures/', subarea_select_name)
    
     # Create necessary output directories - if needed
    dir.create(path = csv_path
               ,showWarnings = FALSE
               ,recursive = TRUE)   
    dir.create(path = figures_path
               ,showWarnings = FALSE
               , recursive = TRUE)
    
     # Filter weather data for only the grid locations within the template data
    get_country_area <- 
      weather_df %>% 
      dplyr::filter(locationid %in% template_df$locationid)
    
     # Merge the weather data with and template data
    weather_template_df <- 
      merge(get_country_area, template_df, by = "locationid")
    
     # Calculate stats across admin0, highest administrative boundary level
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
    
     # Calculate stats across admin1s within admin0
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
    
     # Calculate stats across admin2s within admin1s within admin0
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
    
     # Combine the admin-specific stats with the overall region calculation
    admin1_stats <- admin1_stats %>% dplyr::arrange(avg_LTNP_PET)
    admin2_stats <- admin2_stats %>% dplyr::arrange(avg_LTNP_PET)
    
    
    stats_out <- data.table::rbindlist(list(admin0_stats
                                            ,admin1_stats
                                            ,admin2_stats)
                                       ,use.names = TRUE
                                       ,fill = TRUE)
    
     # This re-orders the information we have calculated - not a necessary 
     # step, but one that makes the output, when read in Excel, easier 
     # to interpret.
    stats_out <- 
      stats_out %>% 
      dplyr::select(1, admin0_name, admin1_name, admin2_name, n_grids
                    ,avg_CSUMPRE, avg_LTNsumPre
                    ,avg_D_CLTNSUMPRE, avg_CP_PET 
                    ,avg_LTNP_PET, avg_D_CLTNP_PET
                    ,avg_CAvgMinT, avg_CAvgMaxT
                    ,avg_LTAvgMnT, avg_LTAvgMxT
                    ,everything())
    
     # Round output numbers to reasonable significant figures
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
    
     # Rename columns to something descriptive
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
    
     # Write administrative statistics to file
    write.csv(stats_out,
              paste0(csv_path, "/stats_", subarea_select_name
                     ,"_", weather_name))
    
     # Save the combined weather-template
    write.csv(weather_template_df,
              paste0(csv_path, "/weatherTemplate_", subarea_select_name
                     ,"_", weather_name))
    
     # Save the subsetted template, which can be used as its own template
     # moving forward. 
    write.csv(template_df,
              paste0(csv_path, "/template_", subarea_select_name, ".csv"))
    
     # Clip Extreme Values -------------------------------------------------
     #
     # Use the support function to set the min/max of each variable.  
     # All values below/above are capped at these values. 
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
     # MAPS 
     # ---------------------------------------------------------------------
     #
     # First, download images for the base or background of the maps.  
     # Expand the "wkt" column values to a format usable by ggplot.
    polygon_df <- tibble::as_tibble(
                  wicket::wkt_coords(weather_template_df$shapewkt))
    
     # Pull the map that will be used as the base for all of the mapping 
     # images. Define a bounding box based on the extent of the data. 
    bounding_box <- make_bbox(lon = polygon_df$lng
                             ,lat = polygon_df$lat
                             ,f = .075)
    
    region_base_map <- get_map(location = bounding_box
                               ,maptype = "toner"
                               ,source = "stamen")
    
     # Set up data to mapped in appropriate format
     #
     # This is using the row index that is expressed in the object column to 
     # combine the data to the polygon_df object
    polygon_df$aPre     <- weather_template_df$aPre[polygon_df$object]
    polygon_df$cPovPET  <- weather_template_df$cPovPET[polygon_df$object]
    polygon_df$aDinPre  <- weather_template_df$aDinPre[polygon_df$object]
    polygon_df$CAvgMaxT <- weather_template_df$CAvgMaxT[polygon_df$object]
    polygon_df$DFLTMaxT <- weather_template_df$DFLTMaxT[polygon_df$object]
    polygon_df$DFLTPVP  <- weather_template_df$DFLTPVP[polygon_df$object]
    
     # Create a map for each of the specified variables
     #
     # Maximum Temperature, current ----------------------------------------
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
    
     # Save the map to file
    ggsave(max_temp_map
           ,filename = paste0(figures_path, "/"
                ,formatGraphTitleForFileName(max_temp_title), "_map.png")
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # Maximum temperature, current vs. LTN --------------------------------
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
    
     # Save the map to file
    ggsave(max_temp_LTN_map
           ,filename = paste0(figures_path, "/"
              ,formatGraphTitleForFileName(max_temp_LTN_title), "_map.png") 
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # Precipitation, current ----------------------------------------------
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
    
     # Save the map to file
    ggsave(precip_map
           ,filename = paste0(figures_path, "/"
                ,formatGraphTitleForFileName(precip_title), "_map.png")
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # Precipitation, current vs. LTN --------------------------------------
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
    
     # Save the map to file
    ggsave(precip_ltn_map
           ,filename = paste0(figures_path, "/"
                ,formatGraphTitleForFileName(precip_ltn_title), "_map.png")
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # P/PET, current ------------------------------------------------------
     #
     # (P / PET = Precipitation-over-PET ratio) 
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
    
     # Save the map to file
    ggsave(ppet_map
           ,filename = paste0(figures_path, "/"
                    ,formatGraphTitleForFileName(ppet_title), "_map.png")
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # P/PET, current vs. LTN ----------------------------------------------
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
    
     # Save the map to file
    ggsave(ppet_vs_ltn_map
           ,filename = paste0(figures_path, "/"
                ,formatGraphTitleForFileName(ppet_vs_ltn_title), "_map.png")
           ,width = fig_dim_graph["width"]
           ,height = fig_dim_graph["height"]
           ,units = "in")
    
     # ---------------------------------------------------------------------
     # HISTOGRAMS 
     # ---------------------------------------------------------------------
     #
     # Histogram of Minimum Temperature, current vs. LTN -------------------
    hist_title <- paste0("Min Temp: Current vs LTN\n"
                         ,subarea_select_name, "\n"
                         ,tools::file_path_sans_ext(weather_name))
    
    aWhereCharts::generateaWhereHistogram(data = weather_template_df
                                          ,variable = "CAvgMinT"
                                          ,title = hist_title
                                          ,xlabel = "Deg C"
                                          ,compare = TRUE
                                          ,compare_var = "LTAvgMnT")
    
     # Write histogram to file
    if (write_hist == TRUE) {
      ggplot2::ggsave(filename = paste0(figures_path, "/"
                      ,formatGraphTitleForFileName(hist_title), ".png")
                      ,width = fig_dim_hist["width"]
                      ,height = fig_dim_hist["height"]
                      ,units = "in"
                      ,device = "png")
    }
    
     # Histogram of Maximum Temperature, current vs. LTN -------------------
    hist_title <- paste0("Max Temp: Current vs LTN\n"
                         ,subarea_select_name, "\n"
                         ,tools::file_path_sans_ext(weather_name))
    
    aWhereCharts::generateaWhereHistogram(data = weather_template_df
                                          ,variable = "CAvgMaxT"
                                          ,title = hist_title
                                          ,xlabel = "Deg C"
                                          ,compare = TRUE
                                          ,compare_var = "LTAvgMxT")
    
     # Write histogram to file
    if (write_hist == TRUE) {
      ggplot2::ggsave(filename = paste0(figures_path, "/"
                      ,formatGraphTitleForFileName(hist_title), ".png")
                      ,width = fig_dim_hist["width"]
                      ,height = fig_dim_hist["height"]
                      ,units = "in"
                      ,device = "png")
    }
    
     # Histogram of Precipitation, current vs. LTN -------------------------
    hist_title <- paste0("Precipitation: Current vs LTN\n"
                         ,subarea_select_name 
                         ,tools::file_path_sans_ext(weather_name))
    
    aWhereCharts::generateaWhereHistogram(data = weather_template_df
                                          ,variable = "ct1"
                                          ,compare = TRUE
                                          ,compare_var = "LTNt1"
                                          ,title = hist_title
                                          ,xlabel = "mm")
    
     # Write histogram to file
    if (write_hist == TRUE) {
      ggplot2::ggsave(filename = paste0(figures_path, "/"
                      ,formatGraphTitleForFileName(hist_title), ".png")
                      ,width = fig_dim_hist["width"]
                      ,height = fig_dim_hist["height"]
                      ,units = "in"
                      ,device = "png")
    }
    
     # Histogram of P/PET, current vs. LTN ---------------------------------
    hist_title <- paste0("PPET: Current vs LTN\n"
                         ,subarea_select_name, "\n" 
                         ,tools::file_path_sans_ext(weather_name))
    
    aWhereCharts::generateaWhereHistogram(data = weather_template_df
                                          ,variable = "ctemp" 
                                          ,title = hist_title 
                                          ,xlabel = "P/PET" 
                                          ,compare = TRUE 
                                          ,compare_var = "LTNtemp") 
    
     # Write histogram to file
    if (write_hist == TRUE) {
      ggplot2::ggsave(filename = paste0(figures_path, "/"
                      ,formatGraphTitleForFileName(hist_title), ".png")
                      ,width = fig_dim_hist["width"]
                      ,height = fig_dim_hist["height"]
                      ,units = "in"
                      ,device = "png")
    }
    
     # ---------------------------------------------------------------------
     # STATISTICS (Tabular summaries of the Histograms)
     # ---------------------------------------------------------------------
     #
     # Precipitation Tabular Summary ---------------------------------------
     #
     # Add a column for which bin/ bin range each grid falls into
    weather_template_df$bin.precip <- NA
    weather_template_df$bin.ltn.precip <- NA
    weather_template_df$bin.range.precip <- NA
    
     # Make structure to hold the names of each bin
    bin_names_precip <- data.frame(bins = seq(1, length(bins_precip) - 1, 1)
                                  ,range = ""
                                  ,stringsAsFactors = FALSE)
    
     # Loop through each bin and populate the appropriate values i.e. manually
     # calculating histogram.  Done this way to accommodate generating the 
     # names for each bin. 
    for(b in 1:(length(bins_precip) - 1)) {
      
      bin_names_precip[b, "range"] <- paste(as.character(bins_precip[b])
                                          ,as.character(bins_precip[b + 1])
                                          ,sep = " - ")
      
       # Indices of entries that fall in the current bin
      idx <- weather_template_df$CSUMPRE >= bins_precip[b] & 
             weather_template_df$CSUMPRE < bins_precip[b + 1]
      
       # Add the bin number to each row
      weather_template_df$bin.precip[idx] <- b
      
       # Indices of entries that fall in the current bin
      idx <- weather_template_df$LTNSUMP >= bins_precip[b] & 
             weather_template_df$LTNSUMP < bins_precip[b + 1]           
      
       # Add the bin number to each row
      weather_template_df$bin.ltn.precip[idx] <- b
    }
    
     # Add columns for number of grids per precip level, the percentage of all
     # grids per precip level, and the affected population per precip level
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
    
     # Combine the data for both the current and LTN such that the primary key on
     # the table is a single column of which precip bin
    current_and_ltn_precip <- 
      dplyr::full_join(unique(grids_per_precip_level[c("bin.precip"
        ,"grid.count.precip", "grid.percent.precip"
        ,"population.count.precip")])
        ,unique(grids_per_precip_level[c("bin.ltn.precip"
        ,"grid.count.ltn.precip", "grid.percent.ltn.precip"
        ,"population.count.ltn.precip")])
                       ,by = c("bin.precip" = "bin.ltn.precip")) %>%
      dplyr::arrange(bin.precip)
    
     # replace NA values with 0
    current_and_ltn_precip[is.na(current_and_ltn_precip)] <- 0
    
     # Add in the names of each bin
    current_and_ltn_precip <- 
      dplyr::inner_join(current_and_ltn_precip
                        ,bin_names_precip
                        ,by = c("bin.precip" = "bins")) %>%
      dplyr::arrange(bin.precip)
    
     # round the percentage values to three decimal places
    current_and_ltn_precip$grid.percent.precip <- 
      round(current_and_ltn_precip$grid.percent.precip, digits = 2)
    current_and_ltn_precip$grid.percent.ltn.precip <- 
      round(current_and_ltn_precip$grid.percent.ltn.precip, digits = 2)
    
     # write number of grids per current and LTN precip level to .csv
    write.csv(current_and_ltn_precip, 
              file = paste0("outputCSVs/",
                            tools::file_path_sans_ext(weather_name),
                            "_grids_per_current_and_ltn_precip_level.csv"))
    
     # P/PET Table Summary -------------------------------------------------
     #
     # Add a column for which bin the grid falls into
    weather_template_df$bin.ppet <- NA
    weather_template_df$bin.ltn.ppet <- NA
    weather_template_df$bin.range.ppet <- NA
    
     # Make datastructure to hold the bin names
    bin_names_ppet <- data.frame(bins = seq(1,length(bins_ppet) - 1, 1)
                                ,range = ""
                                ,stringsAsFactors = FALSE)
    
     # Loop through each bin and populate the appropriate values
    for(b in 1:(length(bins_ppet) - 1)){
      
      bin_names_ppet[b, "range"] <- paste(as.character(bins_ppet[b])
                                        ,as.character(bins_ppet[b + 1])
                                        ,sep = " - ")
      
       # Indices of entries that fall in the current bin
      idx <- weather_template_df$CPOVRPR >= bins_ppet[b] & 
        weather_template_df$CPOVRPR < bins_ppet[b + 1]           
      
       # Add the bin number to each row
      weather_template_df$bin.ppet[idx] <- b
      
       # Indices of entries that fall in the current bin
      idx <- weather_template_df$LTNASPO >= bins_ppet[b] & 
        weather_template_df$LTNASPO < bins_ppet[b + 1]           
      
       # Add the bin number to each row
      weather_template_df$bin.ltn.ppet[idx] <- b
    }
    
     # Add columns for number of grids per ppet level, the percentage of all
     # grids per ppet level, and the affected population per ppet level
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
    
     # Combine the data for both the current and LTN such that the primary 
     # key on the table is a single column of which ppet bin
    current_and_ltn_ppet <- 
      dplyr::full_join(unique(grids_per_ppet_level[c("bin.ppet"
                      ,"grid.count.ppet", "grid.percent.ppet"
                      ,"population.count.ppet")])
                       ,unique(grids_per_ppet_level[c("bin.ltn.ppet"
                       ,"grid.count.ltn.ppet", "grid.percent.ltn.ppet"
                       ,"population.count.ltn.ppet")])
                       ,by = c("bin.ppet" = "bin.ltn.ppet")) %>%
      dplyr::arrange(bin.ppet)
    
     # Replace NA values with 0
    current_and_ltn_ppet[is.na(current_and_ltn_ppet)] = 0
    
     # Add in the names of each bin
    current_and_ltn_ppet <- 
      dplyr::inner_join(current_and_ltn_ppet
                        ,bin_names_ppet
                        ,by = c("bin.ppet" = "bins")) %>%
      dplyr::arrange(bin.ppet)
    
     # Round the percentage values to three decimal places
    current_and_ltn_ppet$grid.percent.ppet <- 
      round(current_and_ltn_ppet$grid.percent.ppet, digits = 2)
    current_and_ltn_ppet$grid.percent.ltn.ppet <- 
      round(current_and_ltn_ppet$grid.percent.ltn.ppet, digits = 2)
    
     # Write number of grids per current and LTN precip level to .csv
    write.csv(current_and_ltn_ppet, 
              file = paste0("outputCSVs/"
                            ,tools::file_path_sans_ext(weather_name)
                            ,"_grids_per_current_and_ltn_ppet_level.csv"))
    
     # Generate Population per Precip bin histogram ------------------------------
     #
     # This adjust the above histograms to represent the number of people 
     # affected by each precip level.
     #
     # Set the title of the figure
    hist_title <- paste0("Precip vs Population\n"
                         ,subarea_select_name , "\n"
                         ,tools::file_path_sans_ext(weather_name))
    
     # Get the columns of interest for the population-per-precip-bin histogram
    chart_data <- current_and_ltn_precip[, c("bin.precip" 
                                             ,"population.count.precip" 
                                             ,"population.count.ltn.precip")]
    
     # Rename the population count columns to simply be "Current" and "LTN"
     # for straightfoward labels in the histogram
    colnames(chart_data) <- c("bin.precip", "Current", "LTN")
    
     # Set data format as long
    chart_data <- tidyr::gather(chart_data
                                ,key = Variable 
                                ,value = population 
                                ,2:ncol(chart_data))
    
     # Set the x scale based on the precip bin range.
     # Using this scale_x_continuous function, the breaks are where tick marks 
     # will be drawn along the x-axis (bin 1, bin 2, ... bin n)
     # The labels are the actual precip values (in units of mm) that will be 
     # written on the x-axis. 
    breaks_precip <- sort(unique(chart_data$bin.precip))
    x_scale <- scale_x_continuous(breaks = unique(breaks_precip),
        labels = sapply(strsplit(bin_names_precip[
          which(bin_names_precip$bins %in% breaks_precip), ]$range
          , " - "), "[", 1))
    
     # Set the chart aesthetics based on aWhereCharts::generateaWhereHistogram()
     # variables for title and label font sizes 
    size_font_main_title <- 16
    size_font_axis_titles <- 14
    size_font_axis_labels <- 14
    size_font_legend_entries <- 14
    
     # scale colors
    colorScaleToUse <- scale_colour_manual(values = c("#1696AC", "#FF810E")) 
    colorFillToUse <- scale_fill_manual(values = c("#1696AC", "#FF810E")) 
    
     # Make the histogram

     # use the "long" formatted chart data
    ggplot(data = chart_data, 
            # precip bin on the x-axis. add 0.5 to shift bars over.
           aes(x = bin.precip + 0.5 
                # population on the y-axis
               ,y = population 
                # color the bars differently for Current vs. LTN
               ,col = Variable 
                # color the bars differently for Current vs. LTN
               ,fill = Variable)) + 
       # set the opacity of the bars
      geom_col(alpha = 0.4, 
                # stack the bard on top of each other 
               position = "identity") + 
      ggplot2::theme_bw() + 
       # format the x-axis using the breaks and labels from above
      x_scale +
       # remove scientific notation from y-axis
      scale_y_continuous(labels = scales::comma) + 
       # put the color bar legend below the x-axis
      theme(legend.position="bottom", 
            legend.direction="horizontal",
            legend.title = element_blank(),
            legend.spacing.x = unit(0.3,"cm")
             # font color 
            ,axis.text.x = element_text(color = "grey20", 
                                        
                                         # font size 
                                        size = size_font_axis_labels,  
                                        angle = 45,
                                         # horizontal adjustment
                                        hjust = 1,      
                                         # font type "plain", "bold"
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
       # set the main title 
      ggtitle(hist_title) + 
      colorScaleToUse + 
      colorFillToUse + 
       # main title size 
      theme(plot.title = element_text(size=size_font_main_title)) 
    
     # Write histogram to file
    ggplot2::ggsave(filename = paste0(figures_path, "/"
                    ,formatGraphTitleForFileName(hist_title), ".png")
                    ,width = fig_dim_hist["width"]
                    ,height = fig_dim_hist["height"]
                    ,units = "in"
                    ,device = "png")
    
     # Population per P/PET bin --------------------------------------------
     #
     # Set the title of the figure
    hist_title <- paste0("PPET vs Population\n"
                         ,subarea_select_name, "\n" 
                         ,tools::file_path_sans_ext(weather_name))
    
     # Get the columns of interest for the population-per-ppet-bin histogram
    chart_data <- current_and_ltn_ppet[, c("bin.ppet" 
                                           ,"population.count.ppet" 
                                           ,"population.count.ltn.ppet")]
    
     # Rename the population count columns to simply be "Current" and "LTN"
     # for straightfoward labels in the histogram. 
    colnames(chart_data) <- c("bin.ppet", "Current", "LTN")
    
     # Set data format as long 
    chart_data <- tidyr::gather(chart_data 
                                ,key = Variable 
                                ,value = population 
                                ,2:ncol(chart_data))
    
     # Set the x scale based on the ppet bin range.
     # Using this scale_x_continuous function, the breaks are where tick marks 
     # will be drawn along the x-axis (bin 1, bin 2, ... bin n).
     # The labels are the actual P/PET values that will be written on the x-axis. 
    breaks_ppet <- sort(unique(chart_data$bin.ppet))
    
    x_scale <- scale_x_continuous(breaks = unique(chart_data$bin.ppet)
              ,labels = sapply(strsplit(bin_names_ppet[
               which(bin_names_ppet$bins %in% breaks_ppet),]$range
              , " - "), "[", 1))
    
     # make the histogram

     # use the "long" formatted chart data 
    ggplot(data = chart_data, 
            # ppet bin on the x-axis. add 0.5 to shift bars over.
           aes(x = bin.ppet + 0.5 
                # population on the y-axis
               ,y = population 
                # color the bars differently for Current vs. LTN
               ,col = Variable 
                # color the bars differently for Current vs. LTN
               ,fill = Variable)) + 
       # set the opacity of the bars
      geom_col(alpha = 0.4,  
                # stack the bard on top of each other+ 
               position = "identity") + 
      ggplot2::theme_bw() + 
       # format the x-axis using the breaks and labels from above
      x_scale + 
       # remove scientific notation from y-axis
      scale_y_continuous(labels = scales::comma) + 
       # put the color bar legend below the x-axis
      theme(legend.position="bottom", 
            legend.direction="horizontal",
            legend.title = element_blank(),
            legend.spacing.x = unit(0.3,"cm")
             # font color
            ,axis.text.x = element_text(color = "grey20", 
                                         # font size 
                                        size = size_font_axis_labels, 
                                        angle = 45,
                                         # horizontal adjustment
                                        hjust = 1,        
                                         # font type "plain", "bold" 
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
      labs(x = "P/PET", 
           y = "Population (# of people)") +
       # set the main title
      ggtitle(hist_title) +
      colorScaleToUse + 
      colorFillToUse + 
       # main title size
      theme(plot.title = element_text(size=size_font_main_title))  
    
     # Write histogram to file 
    ggplot2::ggsave(filename = paste0(figures_path,"/",formatGraphTitleForFileName(hist_title), ".png")
                    ,width = fig_dim_hist["width"]
                    ,height = fig_dim_hist["height"]
                    ,units = "in"
                    ,device = "png")
  }
}

 # Display a status message to the console

cat("Completed Processing all files and all Regions")

 # Check the "figures" and "outputCSVs" folders in your working directory 
 # to see the outputs of this script! 

