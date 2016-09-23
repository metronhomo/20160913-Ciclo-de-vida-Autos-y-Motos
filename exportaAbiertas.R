#'Exporta columnas en formato amigable. El propósito es exportar (para posteriormente limpiar) preguntas abiertas
#'y que sea mucho más fácil la limpieza de los datos.
#'Principalmente esta función fue creada por que había muchas columnas de una misma pregunta abierta. Lo correcto es juntar
#'todas las respuestas en un vector gigante, limpiar ese vector, y despues clonar las columnas pero limpias.
#'@param xpa La tabla principal
#'@param xpb El nombre de las variables de la pregunta abierta
#'@param xpc El nombre del .csv (codificado en "latin1" para que pueda ser leído por windows) 
#'@export
#'@keywords abiertas
#'@examples 
#'exportarAbiertas(nombresR(datos,"P5"),"./abiertas/finalP5.csv")


exportarAbiertas <- function(xpa, xpb, xpc){
  # xpa <- datos
  # xpb <- nombresR(datos,"P5")
  # xpc <- "./abiertas/finalP5.csv"
  
  subdatos <- xpa[,xpb]
  
  finalV <- NULL
  for(i in 1:length(subdatos)){
    vector <- subdatos[,i]
    vector <- as.character(vector)
    finalV <- c(finalV,vector)
  }
  finalV <- unique(finalV)
  
  xpa <- finalV
  write.csv(
    xpa,
    xpc,
    row.names = F,
    fileEncoding = "latin1"
  )
}