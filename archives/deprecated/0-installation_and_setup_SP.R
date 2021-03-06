#--------------------------------------------------------------------------
#
# aWhere R Tutorial: Installation and Setup
# Tutorial de R de aWhere: Instalacion y configuracion
#
# Purpose of script:
# This code will show you how to Install R, RStudio, and aWhere R package.
#
# Proposito del script: 
# Este codigo le mostrara como instalar R, RStudio y el paquete (package) de R 
#
# Date updated: 2020-04-13
# Fecha de actualizacion: 2020-04-13
#--------------------------------------------------------------------------

# Installing R and RStudio ------------------------------------------------
#
# In order to download and use the aWhere API (Application Program Interface) 
# R package, the R programming language will need to be installed on your 
# computer. You can download and install R here: 
#   <https://cloud.r-project.org>.

# We also recommend installing RStudio for writing and developing your code. 
# RStudio is a free program that includes many useful features and functions 
# including code syntax highlighting, a GUI interface for writing and 
# maintaining code, files, plots, etc. It is not necessary to use RStudio to 
# use R, but it will make it easier. 
# RStudio can be downloaded here: 
#   <https://www.rstudio.com/products/rstudio/download/#download>.


# Instalacion de R y RStudio ------------------------------------------------

# Para descargar el paquete de R y utilizar la API (Interfaz de Programacion de Aplicaciones, ingles)  
# de aWhere, debe de tener instalado el lenguage de programacion R en su computadora. 
# Puede descargar e instalar R en el siguiente enlace: 
#   <https://cloud.r-project.org>.

# Tambien, le recomendamos instalar RStudio para escribir y desarrollar su codigo. 
# RStudio es un programa de acceso libre que incluye diversas caracteristicas y funciones
# como por ejemplo el resalte de la sintaxis del codigo, una interfaZ grafica de usuario (GUI, en ingles)
# para escribir y mantener el codigo, archivos, graficos, etc. No es necesario utilizar RStudio para
# hacer uso de R; sin embargo, el uso de RStudio lo hara mas sencillo. 
# Puede descargar RStudio en el siguiente enlace: 
#   <https://www.rstudio.com/products/rstudio/download/#download>.


# Installing the aWhere API R package -------------------------------------

# aWhere's API package for querying our API is hosted on GitHub and not 
# R's central hub for installing packages, CRAN. In order to install the 
# aWhere package directly from GitHub, we need to use a function from the 
# devtools package. If you do not already have the devtools package 
# installed on your computer, or are unsure, you can install it by running 
# the install.packages() function with the name of the package, "devtools": 

# Instalacion del paquete de R del API de aWhere -------------------------------------

# El paquete del API de aWhere para consultar nuestra API, se encuentra almacenado en GitHub
# y no en el repositorio central de R, CRAN. Para instalar el
# paquete de aWhere directamente desde GitHub, es necesario utilizar una funcion
# del paquete devtools. Si usted todavia no tiene el paquete devtools
# instalado en su computadora o no esta seguro(a), puede instalarlo ejecutando 
# la funcion install.packages() con el nombre del paquete, "devtools" entre los parentesis: 

# Install the "devtools" R package:
# Instalacion del paquete de R "devtools": 
install.packages("devtools")

# Load the "devtools" R package after it has been installed: 
# Linea para cargar el paquete "devtools" una vez que este fue instalado:
library(devtools)


# Once the devtools package is installed, install the aWhere API package by 
# running the following:
# Una vez que el paquete "devtools" esta instalado, instale el paquete del API de aWhere 
# ejecutando la siguiente linea:
devtools::install_github("aWhereAPI/aWhere-R-Library")


# The aWhere package references several other R packages in its inner 
# functions. Therefore those packages must also be installed in order for 
# the aWhere package to function as expected. If you run into issues when 
# first installing the aWhere package, try installing these R packages, 
# and then re-install the aWhere package. 


# Dentro de las funciones del paquete de aWhere, se hace referencia a
# otros paquetes de R. Por lo tanto, estos paquetes deben estar previamente instalados
# para que el paquete de aWhere funcione correctamente. Si se presenta algun problema
# al instalar por primera vez el paquete de aWhere, trate de instalar los siguientes paquetes de R, 
# y posteriormente, vuelva a instalar el paquete de aWhere. 

# Run if packages are not already installed:
# Ejecute la siguiente linea si alguno de los paquetes dentro del parentesis no han sido instalados:
install.packages(c("chron", "magrittr", "btcops", "DBI", "assertthat", 
                   "Rcpp", "tibble")) 

# Upon re-install, if you are still presented with an error message 
# stating that a certain package is not installed, use the install.packages() 
# function to install the specified package. Make sure that the package name 
# is placed with quotes. For instance, install.packages("tibble") 


# Si al reinstalar el paquete de aWhere, aun se muestra un mensaje de error 
# indicando que determinado(s) paquete(s) no se encuentra(n) instalado(s), utilice la funcion 
# install.packages() para instalar un paquete en especifico. Asegurese que el nombre del 
# paquete se encuentre dentro de los parentesis. Por ejemplo, install.packages("tibble") 
