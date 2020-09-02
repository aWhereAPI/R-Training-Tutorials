 #--------------------------------------------------------------------------
 # Tutorial de R de aWhere: Instalacion y configuracion
 #
 # Proposito del script: 
 # Este codigo le mostrara como instalar R, RStudio y el paquete (package) de R 
 #
 # Fecha de actualizacion: 2020-04-13
 #
 #--------------------------------------------------------------------------
 # Instalacion de R y RStudio ------------------------------------------------
 #
 # Para descargar el paquete de R y utilizar la API (Interfaz de Programacion de Aplicaciones, ingles)  
 # de aWhere, debe de tener instalado el lenguage de programacion R en su computadora. 
 # Puede descargar e instalar R en el siguiente enlace: 
 #   <https://cloud.r-project.org>.
 #
 # Tambien, le recomendamos instalar RStudio para escribir y desarrollar su codigo. 
 # RStudio es un programa de acceso libre que incluye diversas caracteristicas y funciones
 # como por ejemplo el resalte de la sintaxis del codigo, una interfaZ grafica de usuario (GUI, en ingles)
 # para escribir y mantener el codigo, archivos, graficos, etc. No es necesario utilizar RStudio para
 # hacer uso de R; sin embargo, el uso de RStudio lo hara mas sencillo. 
 # Puede descargar RStudio en el siguiente enlace: 
 #   <https://www.rstudio.com/products/rstudio/download/#download>.
 #
 # Instalacion del paquete de R del API de aWhere -------------------------------------
 #
 # El paquete del API de aWhere para consultar nuestra API, se encuentra almacenado en GitHub
 # y no en el repositorio central de R, CRAN. Para instalar el
 # paquete de aWhere directamente desde GitHub, es necesario utilizar una funcion
 # del paquete devtools. Si usted todavia no tiene el paquete devtools
 # instalado en su computadora o no esta seguro(a), puede instalarlo ejecutando 
 # la funcion install.packages() con el nombre del paquete, "devtools" entre los parentesis: 

 # Instalacion del paquete de R "devtools": 
install.packages("devtools")

 # Linea para cargar el paquete "devtools" una vez que este fue instalado:
library(devtools)

 # Una vez que el paquete "devtools" esta instalado, instale el paquete del API de aWhere 
 # ejecutando la siguiente linea:
devtools::install_github("aWhereAPI/aWhere-R-Library")

 # Dentro de las funciones del paquete de aWhere, se hace referencia a
 # otros paquetes de R. Por lo tanto, estos paquetes deben estar previamente instalados
 # para que el paquete de aWhere funcione correctamente. Si se presenta algun problema
 # al instalar por primera vez el paquete de aWhere, trate de instalar los siguientes paquetes de R, 
 # y posteriormente, vuelva a instalar el paquete de aWhere. 
 #
 # Ejecute la siguiente linea si alguno de los paquetes dentro del parentesis no han sido instalados:
install.packages(c("chron", "magrittr", "btcops", "DBI", "assertthat", 
                   "Rcpp", "tibble")) 

 # Si al reinstalar el paquete de aWhere, aun se muestra un mensaje de error 
 # indicando que determinado(s) paquete(s) no se encuentra(n) instalado(s), utilice la funcion 
 # install.packages() para instalar un paquete en especifico. Asegurese que el nombre del 
 # paquete se encuentre dentro de los parentesis. Por ejemplo, install.packages("tibble") 

