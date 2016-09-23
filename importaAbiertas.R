#'Importa un catálogo (generado con "exportaAbiertas") para limpiar columnas de preguntas abiertas y regresa un data frame
#'con las variables limpias y todas otras variables indicadas en la función.
#'Principalmente esta función fue creada por que había muchas columnas de una misma pregunta abierta. Lo correcto es juntar
#'todas las respuestas en un vector gigante, limpiar ese vector, y despues clonar las columnas pero limpias.
#'@param misDatos La tabla principal
#'@param misVaria El nombre de las variables de la pregunta abierta
#'@param micatalog El data frame (un .csv leído) que tiene como primer columna los fraseos en sucio y en columnas subsecuentes las respuestas limpias
#'@param misVariablesFinales Vector con los nombres de variables que vendrán en el data.frame que regresa la función
#'@export
#'@keywords abiertas
#'@examples 
#'importarAbiertas(misDatos = datos,misVaria = nombresR(datos,"P5"),micatalog = read.csv("./abiertas/finalP5.csv"),misVariablesFinales = "Total")

function(misDatos,misVaria,micatalog,misVariablesFinales=NULL){
  # misDatos <- datos
  # misVaria <- "P5.HSBC.2"
  # micatalog <- read.csv("./abiertas/finalP5VF.csv")
  # misVariablesFinales <- bandera1
  
  fcatalogo <- NULL
  finalV <- NULL
  # Hago que todas las columnas de micatalogo sean el mismo factor....
  for(ppi in 2:length(micatalog)){
    # ppi <- 2
    vector <- micatalog[,ppi]
    vector <- as.character(vector)
    finalV <- c(finalV,vector)
  }
  finalV <- unique(finalV)
  finalV <- factor(finalV)
  for(ppi in 2:length(micatalog)){
    # ppi <- 2
    micatalog[,ppi] <- as.character(micatalog[,ppi])
    micatalog[,ppi] <- factor(x = micatalog[,ppi],levels = finalV)
  }
  for(pp in 1:length(misVaria)){
    # Para cada variable tantas codificaciones hayan salido...
    # pp <- 1
    minimisVaria <- misVaria[pp]
    for(ll in 2:length(micatalog)){
      # Para cada codificación dentro de cada variable...
      # ll <- 2
      minimisVariaR <- paste("CL",minimisVaria,"_",(ll-1),sep="")
      misDatos[,minimisVariaR] <- micatalog[match(misDatos[,minimisVaria],micatalog[,1]),ll]
      fcatalogo <- c(fcatalogo,minimisVariaR)
    }
  }
  return(misDatos[,c(misVariablesFinales,fcatalogo)])
}

