
library(plyr)

exportator <- function(reporteF, nombreFinal){
  # exportator(resultados, "resultados.csv")
  # reporteF <- resultados
  # nombreFinal <- "resultados.csv"
  reporteFINAL <- data.frame()
  for(finali in 1:length(reporteF)){
    # finali <- 1
    elTemporal <- reporteF[[finali]]
    
    elTemporal <- rbind(names(elTemporal),elTemporal)
    
    names(elTemporal) <- paste(LETTERS[1:length(elTemporal)],1:length(elTemporal),sep="")
    
    elTemporal[1,1] <- names(reporteF)[finali]
    
    salto <- elTemporal[0,]
    
    elTemporal <- rbind(elTemporal,NA)
    
    reporteFINAL <- plyr::rbind.fill(reporteFINAL,elTemporal)
    
  }
  
  write.csv(reporteFINAL, nombreFinal,fileEncoding = "Latin1",na = "")
}