#This function will run a fullly automated install of all dependencies for the
#aWhere package.  It has been tested on windows, OSX, and linux on completely clean installs. 
#THe user will be asked for NO input in this process


completeInstall_aWhereDepencies <- function(whichPackage_aWhere = 'aWhereAPI') {
  
  #Number of times to attempt install of each package
  maxInstallAttempts <- 2
  
  #In order to make the package install process go as smoothly as possible we will
  #default to using binaries if the user's system allows
  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  
  if(get_os() != 'linux') {
    options(pkgType="binary")
  }
  
  
  #Specify CRAN packages that need to be installed
  dependencies.CRAN.aWhereAPI <- c('httr'
                                  ,'base64enc'
                                  ,'jsonlite'
                                  ,'lubridate'
                                  ,'data.table'
                                  ,'curl'
                                  ,'knitr'
                                  ,'devtools'
                                  ,'foreach'
                                  ,'raster'
                                  ,'rgeos')
  
  depencies.CRAN.aWhereCharts <- c('dplyr'
                                   ,'ggplot2'
                                   ,'ggthemes'
                                   ,'grid'
                                   ,'rworldmap'
                                   ,'sp'
                                   ,'tidyr'
                                   ,'zoo'
                                   ,'knitr'
                                   ,'devtools')
  
  
  if (whichPackage_aWhere == 'aWhereAPI') {
    
    list.of.packages.CRAN = dependencies.CRAN.aWhereAPI
    list.of.packages.Github = c("aWhereAPI")
    
  } else if (whichPackage_aWhere %in% c('aWhereCharts','all')) {
    list.of.packages.CRAN = unique(c(dependencies.CRAN.aWhereAPI,depencies.CRAN.aWhereCharts))
    list.of.packages.Github = c("aWhereAPI", "aWhereCharts")
  }
  
  #Determine which CRAN packages are not installed
  new.packages.CRAN = list.of.packages.CRAN[!(list.of.packages.CRAN %in% 
                                                installed.packages()[,"Package"])]
  
  #Determine the dependencies of CRAN packages that must be installed
  dependencies.packages.CRAN <- tools::package_dependencies(new.packages.CRAN,recursive = TRUE)
  
  #If dependencies need to be installed, install each one individually
  if(length(dependencies.packages.CRAN) > 0) {
    
    for (x in 1:length(dependencies.packages.CRAN)) {
      
      dependenciesToInstall.packages.CRAN <- unique(dependencies.packages.CRAN[[x]][!(dependencies.packages.CRAN[[x]] %in%
                                                                                        installed.packages()[,"Package"])])
      
      #The recursive options returns the lowest level depencies last so we need to reverse to install them first                                 
      dependenciesToInstall.packages.CRAN <- rev(dependenciesToInstall.packages.CRAN)
      
      if(length(dependenciesToInstall.packages.CRAN) > 0) {
        for (y in 1:length(dependenciesToInstall.packages.CRAN)) {
          
          currentAttemptNumber <- 0
          
          #Check to make sure the package didn't already get installed
          if (dependenciesToInstall.packages.CRAN[y] %in% installed.packages()[,"Package"]) {
            next
          }
          
          while(currentAttemptNumber <= maxInstallAttempts) {
            currentAttemptNumber <- currentAttemptNumber + 1
            tryCatch({
              install.packages(dependenciesToInstall.packages.CRAN[y])
              currentAttemptNumber <- currentAttemptNumber + maxInstallAttempts
            }, error = function(e) {
              if (currentAttemptNumber == maxInstallAttempts) {
                stop('Problem with installing ',dependenciesToInstall.packages.CRAN[y],' which is a necessary dependency 
                     \nPlease followup manually')
              }
              })
            }
          }
        }
      }
    }
  
  #Once all CRAN dependencies are installed, install necessary CRAN packages
  if(length(new.packages.CRAN) > 0) {
    for (x in 1:length(new.packages.CRAN)) {
      
      currentAttemptNumber <- 0
      
      #Check to make sure the package didn't already get installed
      if (new.packages.CRAN[x] %in% installed.packages()[,"Package"]) {
        next
      }
      
      while(currentAttemptNumber <= maxInstallAttempts) {
        currentAttemptNumber <- currentAttemptNumber + 1
        tryCatch({
          install.packages(new.packages.CRAN[x])
          currentAttemptNumber <- currentAttemptNumber + maxInstallAttempts
        }, error = function(e) {
          if (currentAttemptNumber == maxInstallAttempts) {
            stop('Problem with installing ',new.packages.CRAN[x],' which is a necessary dependency 
                 \nPlease followup manually')
          }
          })
        }
      }
    } 
  
  
  new.packages.Github = list.of.packages.Github[!(list.of.packages.Github %in% 
                                                    installed.packages()[,"Package"])]
  if(length(new.packages.Github) > 0) {
    for (x in 1:length(new.packages.Github)) {
      if (new.packages.Github[x] == 'aWhereAPI') {
        devtools::install_github("aWhereAPI/aWhere-R-Library")
      } else if (new.packages.Github[x] == 'aWhereCharts') {
        devtools::install_github("aWhereAPI/aWhere-R-Charts")
      }
    }
  } 

cat('Process Complete')
}