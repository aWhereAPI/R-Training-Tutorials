 #--------------------------------------------------------------------------
 #
 # aWhere R Tutorial: Installation and Setup
 #
 # Purpose of script:
 # This code will show you how to Install R, RStudio, and aWhere R package.
 #
 # Date updated: 2020-04-13
 #--------------------------------------------------------------------------
 #
 # Installing R and RStudio ------------------------------------------------
 #
 # In order to download and use the aWhere API (Application Program Interface) 
 # R package, the R programming language will need to be installed on your 
 # computer. You can download and install R here: 
 #   <https://cloud.r-project.org>.
 #
 # We also recommend installing RStudio for writing and developing your code. 
 # RStudio is a free program that includes many useful features and functions 
 # including code syntax highlighting, a GUI interface for writing and 
 # maintaining code, files, plots, etc. It is not necessary to use RStudio to 
 # use R, but it will make it easier. 
 # RStudio can be downloaded here: 
 #   <https://www.rstudio.com/products/rstudio/download/#download>.
 #
 # Installing the aWhere API R package -------------------------------------
 #
 # aWhere's API package for querying our API is hosted on GitHub and not 
 # R's central hub for installing packages, CRAN. In order to install the 
 # aWhere package directly from GitHub, we need to use a function from the 
 # devtools package. If you do not already have the devtools package 
 # installed on your computer, or are unsure, you can install it by running 
 # the install.packages() function with the name of the package, "devtools": 

 # Install the "devtools" R package:
install.packages("devtools")

 # Load the "devtools" R package after it has been installed: 
library(devtools)

 # Once the devtools package is installed, install the aWhere API package by 
 # running the following:
devtools::install_github("aWhereAPI/aWhere-R-Library")

 # The aWhere package references several other R packages in its inner 
 # functions. Therefore those packages must also be installed in order for 
 # the aWhere package to function as expected. If you run into issues when 
 # first installing the aWhere package, try installing these R packages, 
 # and then re-install the aWhere package.
 #
 # Run if packages are not already installed:
install.packages(c("chron", "magrittr", "btcops", "DBI", "assertthat", 
                   "Rcpp", "tibble")) 

 # Upon re-install, if you are still presented with an error message 
 # stating that a certain package is not installed, use the install.packages() 
 # function to install the specified package. Make sure that the package name 
 # is placed with quotes. For instance, install.packages("tibble") 

