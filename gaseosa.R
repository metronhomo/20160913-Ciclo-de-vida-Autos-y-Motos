
library(sjmisc)

gaseosa <- function(xfile, yfile1){
  # Donde
  # xfile Es el .csv
  # yfile Es el excel que baja de soda
  xfile <- "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS 13-09-2016 18-23-09.csv"
  yfile <- "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS_DataMap_13-09-2016 18_24_04.xlsx"
  
  #########################
  # Previos...
  
  # La base no se puede descargar en spss, voy a etiquetar a mano...
  
  datos <- read.csv(
    xfile,stringsAsFactors = F
  )
  etiquetasPregunta <- readxl::read_excel(path = yfile,sheet = 1)
  etiquetasVariable <- readxl::read_excel(path = yfile,sheet = 2)
  
  for(i in 1:length(datos)){
    # i <- 37
    miDato <- names(datos)[i]
    # Uso subset porque quiero respetar la estructura de mis datos i.e. un data frame
    subDatos <- subset(datos, select = miDato)
    misEtiquetas <- etiquetasPregunta[etiquetasPregunta$Variable==miDato,]
    misEtiquetasVariable <- etiquetasVariable[etiquetasVariable$Variable==miDato,]
    
    if(nrow(misEtiquetas)>0){
      subDatos[,1] <- set_label(subDatos[,1],misEtiquetas$Label)
    }
    if(nrow(misEtiquetasVariable)>0){
      miVector <- as.character(unlist(misEtiquetasVariable[,3]))
      names(miVector) <- as.numeric(unlist(misEtiquetasVariable[,2]))
      
      subDatos[,1] <- factor(x = subDatos[,1],levels = as.numeric(unlist(misEtiquetasVariable[,2])),labels = as.character(unlist(misEtiquetasVariable[,3])))
    }
    datos[,i] <- subDatos
  }
  return(datos)
  ##########################

}

