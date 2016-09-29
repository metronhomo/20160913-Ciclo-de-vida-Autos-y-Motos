#hola mundo------------
devtools::install_github("metronhomo/brostatistics")
library(dplyr)
library(brostatistics)
library(sjmisc)
library(stringr)
library(mlogit)

datos <- gaseosa(xfile = "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS 13-09-2016 18-23-09.csv",yfile = "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS_DataMap_13-09-2016 18_24_04.xlsx")
# 
 names(datos)[188] <- "P15_12_1"
# 
 saveRDS(datos,"datos.rds")

datos <- readRDS(
  "datos.rds"
)
###########################Agrego el año de los autos

año<-readRDS("Abiertaaño.rds")
datos<-cbind(datos,año)
###Vamos a botar los casos que tienen Na en edo civil y nos quedamos con 60 para completar la cuota de autos

datos<-datos[!(datos$P15=='Moto' & is.na(datos$A4)),]
datos<-datos[!(datos$P15=='Ambos' & is.na(datos$A4)),]
set.seed(9)
vector<-datos[(datos$P15=='Auto' & is.na(datos$A4)),"ResponseID"][sample(x = 1:86,size = 26,replace = F)]
datos<-datos[!(datos$ResponseID %in% vector),]

# table(datos$P15)
# summary(datos$A4)

##########################################################################################
##########################################################################################
# Modificaciones necesarias
datos$A7<-droplevels(datos$A7)
# Saco variable "total
datos$Total <- factor(1,levels = 1,labels = "Total")

datos$A4<- as.character(datos$A4)
datos[is.na(datos$A4), "A4"]<-"NsNc"
datos$A4<-as.factor(datos$A4)

datos$A6<- as.character(datos$A6)
datos[is.na(datos$A6), "A6"]<-"NsNc"
datos$A6<-as.factor(datos$A6)

#Creo la variable para el Banner
datos$EdocivilHijos <-5
datos[(datos$A4=="Casado(a)"| datos$A4=='Unión libre') & datos$A6=='Sí',"EdocivilHijos"] <-1
datos[(datos$A4=="Casado(a)"| datos$A4=='Unión libre') & datos$A6=='No',"EdocivilHijos"] <-2 
datos[(datos$A4=="Soltero(a)"|datos$A4=="Divorciado(a)"| datos$A4=="Viudo(a)") & datos$A6=='Sí',"EdocivilHijos"] <-3
datos[(datos$A4=="Soltero(a)"|datos$A4=="Divorciado(a)"| datos$A4=="Viudo(a)") & datos$A6=='No',"EdocivilHijos"] <-4

datos$EdocivilHijos <- factor(datos$EdocivilHijos,levels = c(1,2,3,4,5),labels = c("Casado con hijos","Casado sin hijos","Soltero con Hijos","Soltero sin HIjos","No contestó"))

###Creo la variable de tiempo casados

datos$TiempoCasados = datos$A5*12 + datos$A5_MESES
cortes<-list(
  "4 años o menos"= c(0:48),
  "De 5 a 9 años"= c(49:108),
  "De 10 a 14 años"= c(109:168),
  "De 15 a 19 años"= c(169:228),
  "20 años o más"= c(228:600)
)
datos$TiempoJuntos<-NA
for(i in 1:length(cortes)){
  misCN<- names(cortes)[i]
  misCCn<- cortes[[i]]
  datos[datos$TiempoCasados %in% misCCn,"TiempoJuntos"]<-i
}
datos$TiempoJuntos<- factor(datos$TiempoJuntos,levels = c(1,2,3,4,5), labels = names(cortes))

############# Creo la variable de edad de hijos

hijitos<-list(
  "A9Primero"=c("A9_Hijo1_A_os","A9_Hijo1_Meses"),
  "A9Segundo"=c("A9_Hijo2_A_os","A9_Hijo2_Meses"),
  "A9Tercero"=c("A9_Hijo3_A_os","A9_Hijo3_Meses"),
  "A9Cuarto"=c("A9_Hijo4_A_os","A9_Hijo4_Meses"),
  "A9Quinto"=c("A9_Hijo5_A_os","A9_Hijo5_Meses"),
  "A9Sexto"=c("A9_Hijo6_A_os","A9_Hijo6_Meses")
)

for(i in 1:length(hijitos)){
   # i <- 1
  hijitosSub <- hijitos[[i]]
  nhijitosSub <- names(hijitos)[i]
  edadhijo<-as.numeric(datos[,hijitosSub[1]])
  edadhijo<- edadhijo*12
  edadhijo<-rowSums(cbind(edadhijo,as.numeric(datos[,hijitosSub[2]])),na.rm = T)
  edadhijo[is.na(datos[,hijitosSub[1]]) & is.na(datos[,hijitosSub[2]])]<-NA
  edadhijo<- round(edadhijo,0)
  cortes<-list(
    "menos de 4 años"= c(0:48),
    "entre 5 y 9 años"= c(49:108),
    "entre 10 y 14 años"= c(109:168),
    "más de 15 años"= c(169:800)
  )
  datos[,nhijitosSub]<-NA
  for(i in 1:length(cortes)){
    misCN<- names(cortes)[i]
    misCCn<- cortes[[i]]
    datos[edadhijo %in% misCCn,nhijitosSub]<-i
  }
  datos[,nhijitosSub]<- factor(datos[,nhijitosSub], levels = 1:4,labels = names(cortes))
}
hijitosEdad <- c("A9Primero","A9Segundo","A9Tercero","A9Cuarto","A9Quinto","A9Sexto")

###########Limpieza de variables
#exportarAbiertas(xpa = datos,xpb = "P17_B_BANCO",xpc = "Pregunta Bancos.csv" )
P17_b_bancor<-importaAbiertas(misDatos = datos, misVaria ="P17_B_BANCO",micatalog = read.csv("Pregunta Bancos1.csv"))
datos<- cbind(datos, P17_b_bancor)

#exportarAbiertas(xpa = datos,xpb = "P18_D_BANCO",xpc = "Pregunta BancosD.csv" )
P18_d_bancor<-importaAbiertas(misDatos = datos, misVaria ="P18_D_BANCO",micatalog = read.csv("Pregunta BancosDR.csv"))
datos<- cbind(datos, P18_d_bancor)

P19_b_bancor<-importaAbiertas(misDatos = datos, misVaria ="P19_B_AMBOS_BANCO",micatalog = read.csv("Pregunta BancosDR.csv"))
datos<- cbind(datos, P19_b_bancor)

P20_c_bancor<-importaAbiertas(misDatos = datos, misVaria ="P20_C_AMBOS_BANCO",micatalog = read.csv("Pregunta BancosDR.csv"))
datos<- cbind(datos, P20_c_bancor)

P20_e_bancor<-importaAbiertas(misDatos = datos, misVaria ="P19_E_AMBOS_BANCO",micatalog = read.csv("Pregunta BancosDR.csv"))
datos<- cbind(datos, P20_e_bancor)


P13ModeloLimpia<-importaAbiertas(misDatos = datos, misVaria ="P15_3_MODELO",micatalog = read.csv("Ciclo de vida autos_motos_ModeloVF.csv"))
datos<- cbind(datos, P13ModeloLimpia)
P10ModeloLimpia<-importaAbiertas(misDatos = datos, misVaria ="P15_10_MODELO",micatalog = read.csv("Ciclo de vida autos_motos_ModeloVF.csv"))
datos<- cbind(datos, P10ModeloLimpia)

# exportarAbiertas(datos,c("P15_3_MODELO_1","P15_10_MODELO_1"),xpc = "AbiertasModelo1.csv")
# exportarAbiertas(datos,c("P15_4_1","P15_11_1"),xpc = "AbiertasModeloAño.csv")

P13ModeloLimpia1<-importaAbiertas(misDatos = datos, misVaria ="P15_3_MODELO_1",micatalog = read.csv("Ciclo de vida autos_motos_ModeloVF.csv"))
datos<- cbind(datos, P13ModeloLimpia1)
P10ModeloLimpia1<-importaAbiertas(misDatos = datos, misVaria ="P15_10_MODELO_1",micatalog = read.csv("Ciclo de vida autos_motos_ModeloVF.csv"))
datos<- cbind(datos, P10ModeloLimpia1)

# exportarAbiertas(datos,"P15_E_OTRO",xpc = "P15EAbierta.csv")
# exportarAbiertas(datos,"P15_D_ESPOSOESPOSA_OTRO",xpc = "P15DESPOSO.csv")
# exportarAbiertas(datos,"P15_D_PAPAMAMA_OTRO",xpc = "P15DPAPA.csv")
# exportarAbiertas(datos,"P15_D_HIJOHIJA_OTRO",xpc = "P15DHIJO.csv")
# exportarAbiertas(datos,"P15_D_OTROFAMILIAR_OTRO",xpc = "P15DOTRO.csv")
# exportarAbiertas(datos,"P17_B_OTRO",xpc = "P17BAbierta.csv")
# exportarAbiertas(datos,"P17_B1_OTRO",xpc = "P17B1Abierta.csv")


###########################Agrupacion de preguntas
##Uso de transporte
misP <- nombresR(datos,"P15_2")

misP <- matrix(data = misP,nrow = 6,ncol = 5)

listaNombres<-list()
for(i in 1:nrow(misP)){
  # i <- 1
  yoSoy <- misP[i,1]
  yoSoy <- paste("Rr_", yoSoy,sep="")
  listaNombres[[i]]<-paste(misP[i,])
  names(listaNombres)[[i]]<-yoSoy  
}

for(i in 1:length(listaNombres)){
  # i<-1
  mmm<- names(listaNombres)[i]
  mhijitos<- listaNombres[[i]]
  datos[,mmm]<-FALSE
  for(t in 1:length(mhijitos)){
    # t<-2
    logico<- datos[,mhijitos[t]]==1
    logico[is.na(logico)]<-FALSE
    datos[logico,mmm]<-TRUE
  }
}

misP <- nombresR(datos,"P15_9_AUTO")

misP <- matrix(data = misP,nrow = 6,ncol = 5)

listaNombres<-list()
for(i in 1:nrow(misP)){
  # i <- 1
  yoSoy <- misP[i,1]
  yoSoy <- paste("Rr_", yoSoy,sep="")
  listaNombres[[i]]<-paste(misP[i,])
  names(listaNombres)[[i]]<-yoSoy  
}

for(i in 1:length(listaNombres)){
  # i<-1
  mmm<- names(listaNombres)[i]
  mhijitos<- listaNombres[[i]]
  datos[,mmm]<-FALSE
  for(t in 1:length(mhijitos)){
    # t<-2
    logico<- datos[,mhijitos[t]]==1
    logico[is.na(logico)]<-FALSE
    datos[logico,mmm]<-TRUE
  }
}

misP <- nombresR(datos,"P_15_9_MOTO")

misP <- matrix(data = misP,nrow = 6,ncol = 5)

listaNombres<-list()
for(i in 1:nrow(misP)){
  # i <- 1
  yoSoy <- misP[i,1]
  yoSoy <- paste("Rr_", yoSoy,sep="")
  listaNombres[[i]]<-paste(misP[i,])
  names(listaNombres)[[i]]<-yoSoy  
}

for(i in 1:length(listaNombres)){
  # i<-1
  mmm<- names(listaNombres)[i]
  mhijitos<- listaNombres[[i]]
  datos[,mmm]<-FALSE
  for(t in 1:length(mhijitos)){
    # t<-2
    logico<- datos[,mhijitos[t]]==1
    logico[is.na(logico)]<-FALSE
    datos[logico,mmm]<-TRUE
  }
}



##########################################################################################

# Banner
bandera1 <- c("Total","P10","F1Genero","NSE","Edad_Rango","Plaza","EdocivilHijos")
bandera3 <- c("Total","A11")

nombresR(datos,"11")
# names(datos)[1:100]
# 
# table(
#   datos$P10
# )
# datos[datos$P10=="Automóvil",]
# 

table(datos$EdocivilHijos)

resultados <- list(
  # Banner vs Banner
  Genero = frecuentator(fTtabla = datos,fTvariables = "F1Genero",fTlevels = T,fbanner = bandera1),
  NSE = frecuentator(fTtabla = datos,fTvariables = "NSE",fTlevels = T,fbanner = bandera1),
  Edad_Rango = frecuentator(fTtabla = datos,fTvariables = "Edad_Rango",fTlevels = T,fbanner = bandera1),
  Plaza = frecuentator(fTtabla = datos,fTvariables = "Plaza",fTlevels = T,fbanner = bandera1),
  P9 = frecuentator(fTtabla = datos,fTvariables = "P9",fTlevels = T,fbanner = bandera1),
  P9Estudios = frecuentator(fTtabla = datos,fTvariables = "P9_1",fTlevels = T,fbanner = bandera1),
  P10 = frecuentator(fTtabla = datos,fTvariables = "P10",fTlevels = T,fbanner = bandera1),
  P11 = frecuentator(fTtabla = datos,fTvariables = "A4",fTlevels = T,fbanner = bandera1),
  P11Tiempo = frecuentator(fTtabla = datos,fTvariables = "TiempoJuntos",fTlevels = T,fbanner = bandera1),
  P12 = frecuentator(fTtabla = datos,fTvariables = "A6",fTlevels = T,fbanner = bandera1),
  P12CuantosHijos = frecuentator(fTtabla = datos,fTvariables = "A7",fTlevels = T,fbanner = bandera1),
  P13EdadHijosenCasa = frecuentator(fTtabla = datos,fTvariables = hijitosEdad,fTlevels = T,fbanner = bandera1),
  P14 = frecuentator(fTtabla = datos,fTvariables = "A11",fTlevels = T,fbanner = bandera1)
)
exportator(resultados, "./resultados/resultadosPrimeros.csv")


resultadosAutos <- list(
  P10 = frecuentator(fTtabla = datos,fTvariables = "P10",fTlevels = T,fbanner = bandera1),
  P15NumAutos = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_1",fTlevels = T,fbanner = bandera1),
  P15MarcaAutos = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = nombresR(datos,"P15_3_MARCA"),fTlevels = T,fbanner = bandera1),
  P15Modelo = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = nombresR(datos,"P13ModeloLimpia"),fTlevels = T,fbanner = bandera1),
  P15AñoAutos= frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = nombresR(datos,"CLP15_4_1"),fTlevels = T,fbanner = bandera1),
  P15UsoAutos = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = nombresR(datos,"Rr_P15_2"),fTlevels = F,fbanner = bandera1),
  P15AntAutos = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = nombresR(datos,"P15_5"),fTlevels = T,fbanner = bandera1),
  P15PrestarAutos = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_A",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienAutos = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")),],fTvariables =nombresR(datos,"P15_B"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienAutosf1 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Esposo___Esposa=="TRUE")),],fTvariables ="P15_C_EsposaEsposo",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienAutosf2 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Pap____Mam=="TRUE")),],fTvariables ="P15_C_PapaMama",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienAutosf3 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Hijos___Hijas=="TRUE")),],fTvariables ="P15_C_HijoHija",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienAutosf4 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Otro_familiar=="TRUE")),],fTvariables ="P15_C_OtroFamiliar",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienAutosRazon1 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Esposo___Esposa=="TRUE")),],fTvariables = nombresR(datos,"P15_D_EsposoEsposa"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienAutosRazon2 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Pap____Mam=="TRUE")),],fTvariables =nombresR(datos,"P15_D_PapaMama"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienAutosRazon3 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Hijos___Hijas=="TRUE")),],fTvariables =nombresR(datos,"P15_D_HijoHija"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienAutosRazon4 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Otro_familiar=="TRUE")),],fTvariables =nombresR(datos,"P15_D_OtroFamiliar"),fTlevels = F,fbanner = bandera1),
  P15NoPrestaAutosPQ = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "No")),],fTvariables =(nombresR(datos,"P15_E")[1:7]),fTlevels = F,fbanner = bandera1),  
  P15NoPrestaAutosProb = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "No")),],fTvariables ="P15_F",fTlevels = T,fbanner = bandera1),
  P15NoPrestaAutosProbQ = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "No")&(datos$P15_F== 'Sí')),],fTvariables ="P15_G",fTlevels = T,fbanner = bandera1),
  P15NoPrestarQuienAutosSol1 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "No")&(datos$P15_G=="Esposa/ Esposo")),],fTvariables = (nombresR(datos,"P15_H_ESPOSAESPOSO")[1:5]),fTlevels = F,fbanner = bandera1),
  P15NoPrestarQuienAutosSol2 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "No")&(datos$P15_G=="Mamá/ Papá")),],fTvariables = (nombresR(datos,"P15_H_PAPAMAMA")[1:5]),fTlevels = F,fbanner = bandera1),
  P15NoPrestarQuienAutosSol3 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "No")&(datos$P15_G=="Hijo/hija")),],fTvariables = (nombresR(datos,"P15_H_HIJOHIJA")[1:5]),fTlevels = F,fbanner = bandera1),
  P15NoPrestarQuienAutosSol4 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "No")&(datos$P15_G=="Otro familiar")),],fTvariables = (nombresR(datos,"P15_H_OTROFAMILIAR")[1:5]),fTlevels = F,fbanner = bandera1),
  P15Actividades1 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_I_La_uso_para_ir_a_trabajar",fTlevels = T,fbanner = bandera3),
  P15Actividades2 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_I_La_uso_para_ir_al_Doctor___Den",fTlevels = T,fbanner = bandera3),
  P15Actividades3 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_I_La_uso_para_llevar_hijos_a_la_",fTlevels = T,fbanner = bandera3),
  P15Actividades4 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_I_La_uso_para_salir_de_paseo___s",fTlevels = T,fbanner = bandera3),
  P15Actividades5 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_I_La_uso_para_salir_de_vacacione",fTlevels = T,fbanner = bandera3),
  P15Actividades6 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_I_La_uso_para_llevar___transport",fTlevels = T,fbanner = bandera3),
  P15Actividades7 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_I_La_uso_para_salir_de_fiesta",fTlevels = T,fbanner = bandera3),
  P15Actividades8 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_I_La_uso_para_ir_al_s_per___merc",fTlevels = T,fbanner = bandera3),
  P15Actividades9 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_I_La_uso_para_ir_al_banco",fTlevels = T,fbanner = bandera3),
  P15Actividades10 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_I_La_uso_para_ir_a_la_Escuela___",fTlevels = T,fbanner = bandera3),
  P15Actividades11 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_I_La_uso_para_ir_a_visitar_a_mi_",fTlevels = T,fbanner = bandera3),
  P15InfluenciaAutos = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables =nombresR(datos,"P15_L_"),fTlevels = F,fbanner = bandera1),
  P15CompraAutos = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = nombresR(datos,"P15_M_"),fTlevels = F,fbanner = bandera1),
  P16 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables ="P16",fTlevels = T,fbanner = bandera1),
  P16Cambio = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = (nombresR(datos,"P16_A_")[1:12]),fTlevels = F,fbanner = bandera1),
  P16TransporteEficiente = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables ="P16_B",fTlevels = T,fbanner = bandera1),
  P16C1 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P16_C_Me_da_prestigio___status",fTlevels = T,fbanner = bandera1),
  P16C2 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P16_C_Me_da_tranquilidad",fTlevels = T,fbanner = bandera1),
  P16C3 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P16_C_Me_da_confianza",fTlevels = T,fbanner = bandera1),
  P16C4 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P16_C_Me_da_seguridad",fTlevels = T,fbanner = bandera1),
  P16C5 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P16_C_Me_siento_contento__a",fTlevels = T,fbanner = bandera1),
  P17 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables ="P17",fTlevels = T,fbanner = bandera1),
  P17A = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P17== "Es usado (a)")),],fTvariables ="P17_A",fTlevels = T,fbanner = bandera1),
  P17B = frecuentator(fTtabla = datos[(datos$P10=="Automóvil"),],fTvariables =(nombresR(datos,"P17_B_")[1:6]),fTlevels = F,fbanner = bandera1),
  P17BBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P17_B_Tarjeta_de_Cr_dito=="TRUE")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P17_B_Cr_dito_personal_del_banco=="TRUE")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P17_B_Otro__Especificar=="TRUE")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P17_B_No_sabe__no_contest=="TRUE")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17B1  = frecuentator(fTtabla = datos[datos$P10=="Automóvil",], fTvariables = nombresR(datos,"P17_B1")[1:4],fTlevels = F,fbanner = bandera1),
  P18  = frecuentator(fTtabla = datos[datos$P10=="Automóvil",], fTvariables = "P18",fTlevels = T,fbanner = bandera1),
  P18C  = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si")),], fTvariables = "P18_C",fTlevels = T,fbanner = bandera1),
  P18D = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si")),],fTvariables =(nombresR(datos,"P18_D_")[1:6]),fTlevels = F,fbanner = bandera1),
  P18CBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si") & (datos$P18_D_Tarjeta_de_Cr_dito=="TRUE")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si") & (datos$P18_D_Cr_dito_personal_del_banco=="TRUE")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si") & (datos$P18_D_Otro__Especificar=="TRUE")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si") & (datos$P18_D_No_sabe__no_contest=="TRUE")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18E = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_E_")[1:13],fTlevels = F,fbanner = bandera1),
  p18E1 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si" & datos$P18_E_Nissan=="TRUE",], fTvariables =nombresR(datos,"P18_F_")[1:12],fTlevels = F,fbanner = bandera1),
  p18E2 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si" & datos$P18_E_General_Motors=="TRUE",], fTvariables =nombresR(datos,"P18_F_")[14:25],fTlevels = F,fbanner = bandera1),
  p18E3 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si"& datos$P18_E_Volkswagen=="TRUE",], fTvariables =nombresR(datos,"P18_F_")[27:38],fTlevels = F,fbanner = bandera1),
  p18E5 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si"& datos$P18_E_Ford_Motor=="TRUE",], fTvariables =nombresR(datos,"P18_F_")[53:64],fTlevels = F,fbanner = bandera1),
  p18E6 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si"& datos$P18_E_Toyota=="TRUE",], fTvariables =nombresR(datos,"P18_F_")[66:77],fTlevels = F,fbanner = bandera1),
  p18E7 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si"& datos$P18_E_Honda=="TRUE",], fTvariables =nombresR(datos,"P18_F_")[79:90],fTlevels = F,fbanner = bandera1),
  p18E8 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si"& datos$P18_E_Mazda=="TRUE",], fTvariables =nombresR(datos,"P18_F_")[92:103],fTlevels = F,fbanner = bandera1),
  p18E9 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si"& datos$P18_E_Hyundai=="TRUE",], fTvariables =nombresR(datos,"P18_F_")[105:116],fTlevels = F,fbanner = bandera1),
  p18E10 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si"& datos$P18_E_Renault=="TRUE",], fTvariables =nombresR(datos,"P18_F_")[118:129],fTlevels = F,fbanner = bandera1),
  p18E11 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si"& datos$P18_E_Mitsubichi=="TRUE",], fTvariables =nombresR(datos,"P18_F_")[131:142],fTlevels = F,fbanner = bandera1),
  p18E12 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si"& datos$P18_E_FAW=="TRUE",], fTvariables =nombresR(datos,"P18_F_")[144:155],fTlevels = F,fbanner = bandera1)
 )
exportator(resultadosAutos, "./resultados/resultadosAutos.csv")


resultadosMotos <- list(
  P10 = frecuentator(fTtabla = datos,fTvariables = "P10",fTlevels = T,fbanner = bandera1),
  P15NumMotos = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_1",fTlevels = T,fbanner = bandera1),
  P15MarcaMotos = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = (nombresR(datos,"P15_6_MARCA")[c(1,3)]),fTlevels = T,fbanner = bandera1),
  P15CategoriaMotos = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = (nombresR(datos,"P15_6_CATEGORIA")[c(1,3)]),fTlevels = T,fbanner = bandera1),
  P15AntMotos = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = nombresR(datos,"P15_7"),fTlevels = T,fbanner = bandera1),
  P15UsoMotos = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = nombresR(datos,"Rr_P15_2"),fTlevels = F,fbanner = bandera1),
  P15PrestarMotos = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_A",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienMotos = frecuentator(fTtabla = datos[((datos$P10== "Moto / Motocicleta")& (datos$P15_A== "Sí")),],fTvariables =nombresR(datos,"P15_B"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienMotosf1 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Esposo___Esposa=="TRUE")),],fTvariables ="P15_C_EsposaEsposo",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienMotosf2 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Pap____Mam=="TRUE")),],fTvariables ="P15_C_PapaMama",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienMotosf3 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Hijos___Hijas=="TRUE")),],fTvariables ="P15_C_HijoHija",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienMotosf4 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Otro_familiar=="TRUE")),],fTvariables ="P15_C_OtroFamiliar",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienMotosRazon1 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Esposo___Esposa=="TRUE")),],fTvariables = nombresR(datos,"P15_D_EsposoEsposa"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienMotosRazon2 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Pap____Mam=="TRUE")),],fTvariables =nombresR(datos,"P15_D_PapaMama"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienMotosRazon3 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Hijos___Hijas=="TRUE")),],fTvariables =nombresR(datos,"P15_D_HijoHija"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienMotosRazon4 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Otro_familiar=="TRUE")),],fTvariables =nombresR(datos,"P15_D_OtroFamiliar"),fTlevels = F,fbanner = bandera1),
  P15NoPrestaMotosPQ = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")),],fTvariables =(nombresR(datos,"P15_E")[1:7]),fTlevels = F,fbanner = bandera1),  
  P15NoPrestaMotosProb = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")),],fTvariables ="P15_F",fTlevels = T,fbanner = bandera1),
  P15NoPrestaMotosProbQ = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")&(datos$P15_F== 'Sí')),],fTvariables ="P15_G",fTlevels = T,fbanner = bandera1),
  #P15NoPrestarQuienMotosSol1 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")&(datos$P15_G=="Esposa/ Esposo")),],fTvariables = (nombresR(datos,"P15_H_ESPOSAESPOSO")[1:5]),fTlevels = F,fbanner = bandera1),
  #P15NoPrestarQuienMotosSol2 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")&(datos$P15_G=="Mamá/ Papá")),],fTvariables = (nombresR(datos,"P15_H_PAPAMAMA")[1:5]),fTlevels = F,fbanner = bandera1),
  #P15NoPrestarQuienMotosSol3 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")&(datos$P15_G=="Hijo/hija")),],fTvariables = (nombresR(datos,"P15_H_HIJOHIJA")[1:5]),fTlevels = F,fbanner = bandera1),
  P15NoPrestarQuienMotosSol4 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")&(datos$P15_G=="Otro familiar")),],fTvariables = (nombresR(datos,"P15_H_OTROFAMILIAR")[1:5]),fTlevels = F,fbanner = bandera1),
  P15Actividades1 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_ir_a_trabajar",fTlevels = T,fbanner = bandera3),
  P15Actividades2 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_ir_al_Doctor___Den",fTlevels = T,fbanner = bandera3),
  P15Actividades3 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_llevar_hijos_a_la_",fTlevels = T,fbanner = bandera3),
  P15Actividades4 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_salir_de_paseo___s",fTlevels = T,fbanner = bandera3),
  P15Actividades5 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_salir_de_vacacione",fTlevels = T,fbanner = bandera3),
  P15Actividades6 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_llevar___transport",fTlevels = T,fbanner = bandera3),
  P15Actividades7 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_salir_de_fiesta",fTlevels = T,fbanner = bandera3),
  P15Actividades8 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_ir_al_s_per___merc",fTlevels = T,fbanner = bandera3),
  P15Actividades9 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_ir_al_banco",fTlevels = T,fbanner = bandera3),
  P15Actividades10 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_ir_a_la_Escuela___",fTlevels = T,fbanner = bandera3),
  P15Actividades11 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_ir_a_visitar_a_mi_",fTlevels = T,fbanner = bandera3),
  P15Actividades12 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_1_ATRIBUTOMOTO_La_uso_para_salir_a_las_rodada",fTlevels = T,fbanner = bandera3),
  P15InfluenciaMotos = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables =nombresR(datos,"P15_L_"),fTlevels = F,fbanner = bandera1),
  P15CompraMotos = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = nombresR(datos,"P15_M_"),fTlevels = F,fbanner = bandera1),
  P16 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables ="P16",fTlevels = T,fbanner = bandera1),
  P16Cambio = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = (nombresR(datos,"P16_A_")[1:12]),fTlevels = F,fbanner = bandera1),
  P16TransporteEficiente = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables ="P16_B",fTlevels = T,fbanner = bandera1),
  P16C1 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P16_C_Me_da_prestigio___status",fTlevels = T,fbanner = bandera1),
  P16C2 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P16_C_Me_da_tranquilidad",fTlevels = T,fbanner = bandera1),
  P16C3 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P16_C_Me_da_confianza",fTlevels = T,fbanner = bandera1),
  P16C4 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P16_C_Me_da_seguridad",fTlevels = T,fbanner = bandera1),
  P16C5 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P16_C_Me_siento_contento__a",fTlevels = T,fbanner = bandera1),
  P17 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables ="P17",fTlevels = T,fbanner = bandera1),
  P17A = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P17== "Es usado (a)")),],fTvariables ="P17_A",fTlevels = T,fbanner = bandera1),
  P17B = frecuentator(fTtabla = datos[(datos$P10=="Moto / Motocicleta"),],fTvariables =(nombresR(datos,"P17_B_")[1:6]),fTlevels = F,fbanner = bandera1),
  P17BBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P17_B_Tarjeta_de_Cr_dito=="TRUE")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P17_B_Cr_dito_personal_del_banco=="TRUE")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P17_B_Otro__Especificar=="TRUE")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P17_B_No_sabe__no_contest=="TRUE")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17B1  = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",], fTvariables = nombresR(datos,"P17_B1"),fTlevels = F,fbanner = bandera1),
  P18  = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",], fTvariables = "P18",fTlevels = T,fbanner = bandera1),
  P18C  = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si")),], fTvariables = "P18_C",fTlevels = T,fbanner = bandera1),
  P18D = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si")),],fTvariables =(nombresR(datos,"P18_D_")[1:6]),fTlevels = F,fbanner = bandera1),
  P18CBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si") & (datos$P18_D_Tarjeta_de_Cr_dito=="TRUE")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si") & (datos$P18_D_Cr_dito_personal_del_banco=="TRUE")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si") & (datos$P18_D_Otro__Especificar=="TRUE")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si") & (datos$P18_D_No_sabe__no_contest=="TRUE")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  p18G = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_G_")[1:10],fTlevels = F,fbanner = bandera1),
  p18F1 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si" & datos$P18_G_Italika=="TRUE",], fTvariables =nombresR(datos,"P18_H_")[1:15],fTlevels = F,fbanner = bandera1),
  p18F2 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si" & datos$P18_G_Yamaha=="TRUE",], fTvariables =nombresR(datos,"P18_H_")[17:31],fTlevels = F,fbanner = bandera1),
  p18F3 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si" & datos$P18_G_Honda=="TRUE",], fTvariables =nombresR(datos,"P18_H_")[33:47],fTlevels = F,fbanner = bandera1),
  p18F6 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si" & datos$P18_G_Kawasaki=="TRUE",], fTvariables =nombresR(datos,"P18_H_")[81:95],fTlevels = F,fbanner = bandera1),
  p18F7 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si" & datos$P18_G_Harley_Davidson=="TRUE",], fTvariables =nombresR(datos,"P18_H_")[97 :111],fTlevels = F,fbanner = bandera1)
  )
  
exportator(resultadosMotos, "./resultados/resultadosMotos.csv")


 
 resultadosAmbosAutos <- list(
   AP10 = frecuentator(fTtabla = datos,fTvariables = "P10",fTlevels = T,fbanner = bandera1),
   P15NumAmbosAuto = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_8_AUTO",fTlevels = T,fbanner = bandera1),
   P15UsoAmbosAuto = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"Rr_P15_9_AUTO"),fTlevels = F,fbanner = bandera1),
   P15MarcaAmbosAutos = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P15_10_MARCA"),fTlevels = T,fbanner = bandera1),
   P15AmbosModelo = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P10ModeloLimpia"),fTlevels = T,fbanner = bandera1),
   P15AñoAmbosAutos= frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"CLP15_11_1"),fTlevels = T,fbanner = bandera1),
   P15AntAmbosAuto = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P15_12"),fTlevels = T,fbanner = bandera1),
   P16PrestarAmbosAutos = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P16_A_AMBOS",fTlevels = T,fbanner = bandera1),
   P16PrestarQuienAmbosAutos = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")),],fTvariables =nombresR(datos,"P16_B_AMBOS"),fTlevels = F,fbanner = bandera1),
   P16PrestarQuienAmbosAutosf1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Mi_Esposo___Esposa=="TRUE")),],fTvariables ="P16_C_AMBOS_ESPOSAESPOSO",fTlevels = T,fbanner = bandera1),
   P16PrestarQuienAmbosAutosf2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Mi_Pap____Mam=="TRUE")),],fTvariables ="P16_C_AMBOS_MAMAPAPA",fTlevels = T,fbanner = bandera1),
   P16PrestarQuienAmbosAutosf3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Hijos___Hijas=="TRUE")),],fTvariables ="P16_C_AMBOS_HIJOHIJA",fTlevels = T,fbanner = bandera1),
   P16PrestarQuienAmbosAutosf4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Otro_familiar=="TRUE")),],fTvariables ="P16_C_AMBOS_OTROFAMILIAR",fTlevels = T,fbanner = bandera1),
   AP16AmbosPrestarQuienAutosRazon1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Mi_Esposo___Esposa=="TRUE")),],fTvariables = nombresR(datos,"P16_D_AMBOS_ESPOSAESPOSO")[1:12],fTlevels = F,fbanner = bandera1),
   AP16AmbosPrestarQuienAutosRazon2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Mi_Pap____Mam=="TRUE")),],fTvariables =nombresR(datos,"P16_D_AMBOS_MAMAPA")[1:12],fTlevels = F,fbanner = bandera1),
   AP16AmbosPrestarQuienAutosRazon3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Hijos___Hijas=="TRUE")),],fTvariables =nombresR(datos,"P16_D_AMBOS_HIJOHIJA")[1:12],fTlevels = F,fbanner = bandera1),
   AP16AmbosPrestarQuienAutosRazon4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Otro_familiar=="TRUE")),],fTvariables =nombresR(datos,"P16_D_AMBOS_OTROFAMILIAR")[1:12],fTlevels = F,fbanner = bandera1),
   AP16NoPrestaAutosPQ = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")),],fTvariables ="P16_E_AMBOS",fTlevels = T,fbanner = bandera1), 
   AP16NoPrestaAutosProb = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")),],fTvariables ="P16_F",fTlevels = T,fbanner = bandera1),
   AP16NoPrestaAutosProbQ = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")&(datos$P16_F== 'Sí')),],fTvariables ="P16_G_AMBOS",fTlevels = T,fbanner = bandera1),
   AP16NoPrestarQuienAutosSol1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")&(datos$P16_G=="Esposa/ Esposo")),],fTvariables = "P16_H_AMBOS_ESPOSAESPOSO",fTlevels = T,fbanner = bandera1),
   AP16NoPrestarQuienAutosSol2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")&(datos$P16_G=="Mamá/ Papá")),],fTvariables = "P16_H_AMBOS_MAMAPAPA",fTlevels = T,fbanner = bandera1),
   AP16NoPrestarQuienAutosSol3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")&(datos$P16_G=="Hijo/hija")),],fTvariables = "P16_H_AMBOS_HIJAHIJO",fTlevels = T,fbanner = bandera1),
   AP16NoPrestarQuienAutosSol4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")&(datos$P16_G=="Otro familiar")),],fTvariables = "P16_H_AMBOS_OTROFAMILIAR",fTlevels = T,fbanner = bandera1),
   AP16Actividades1 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_I_AMBOS_Lo_uso_para_ir_a_trabajar",fTlevels = T,fbanner = bandera3),
   AP16Actividades2 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_I_AMBOS_Lo_uso_para_ir_al_Doctor___Den",fTlevels = T,fbanner = bandera3),
   AP16Actividades3 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_I_AMBOS_Lo_uso_para_llevar_hijos_a_la_",fTlevels = T,fbanner = bandera3),
   AP16Actividades4 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_I_AMBOS_Lo_uso_para_salir_de_paseo___s",fTlevels = T,fbanner = bandera3),
   AP16Actividades5 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_I_AMBOS_Lo_uso_para_salir_de_vacacione",fTlevels = T,fbanner = bandera3),
   AP16Actividades6 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_I_AMBOS_Lo_uso_para_llevar___transport",fTlevels = T,fbanner = bandera3),
   AP16Actividades7 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_I_AMBOS_Lo_uso_para_salir_de_fiesta",fTlevels = T,fbanner = bandera3),
   AP16Actividades8 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_I_AMBOS_Lo_uso_para_ir_al_s_per___merc",fTlevels = T,fbanner = bandera3),
   AP16Actividades9 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_I_AMBOS_Lo_uso_para_ir_al_banco",fTlevels = T,fbanner = bandera3),
   AP16Actividades10 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_I_AMBOS_Lo_uso_para_ir_a_la_Escuela___",fTlevels = T,fbanner = bandera3),
   AP16Actividades11 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_I_AMBOS_Lo_uso_para_ir_a_visitar_a_mi_",fTlevels = T,fbanner = bandera3),
   AP17Preferencia = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_K_AMBOS",fTlevels = T,fbanner = bandera1),
   AP17PREFERENCIAAUTOS= frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables =nombresR(datos,"P17_L_AMBOS")[1:7],fTlevels = F,fbanner = bandera1),
   AP16InfluenciaAutos = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables =nombresR(datos,"P17_O_")[c(1,3,5,7,9,11)],fTlevels = F,fbanner = bandera1),
   AP16CompraAutos = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P17_P_")[c(1,3,5,7,9,11,13)],fTlevels = F,fbanner = bandera1),
   AP18 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables ="P18_AMBOS_Auto",fTlevels = T,fbanner = bandera1),
   AP18Cambio = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P18_A_Ambos_")[c(1,3,5,7,9,11,13,15,17,19,21,23)],fTlevels = F,fbanner = bandera1),
   AP18TransporteEficiente = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables ="P18_B_AMBOS",fTlevels = T,fbanner = bandera1),
   AP18C1 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P18_C_AMBOS_Me_da_prestigio___status",fTlevels = T,fbanner = bandera1),
   AP18C2 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P18_C_AMBOS_Me_da_tranquilidad",fTlevels = T,fbanner = bandera1),
   AP18C3 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P18_C_AMBOS_Me_da_confianza",fTlevels = T,fbanner = bandera1),
   AP18C4 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P18_C_AMBOS_Me_da_seguridad",fTlevels = T,fbanner = bandera1),
   AP18C5 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P18_C_AMBOS_Me_siento_contento__a",fTlevels = T,fbanner = bandera1),
   AP19 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables ="P19_AMBOS",fTlevels = T,fbanner = bandera1),
   AP19A = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P19_AMBOS== "Es usado")),],fTvariables ="P19_A_AMBOS",fTlevels = T,fbanner = bandera1),
   AP19B = frecuentator(fTtabla = datos[(datos$P10=="Ambas"),],fTvariables =(nombresR(datos,"P19_B_")[1:6]),fTlevels = F,fbanner = bandera1),
   AP19BBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P19_B_AMBOS_Tarjeta_de_Cr_dito=="TRUE")),],fTvariables ="P19_b_bancor",fTlevels = T,fbanner = bandera1),
   AP19BBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P19_B_AMBOS_Cr_dito_personal_del_banco=="TRUE")),],fTvariables ="P19_b_bancor",fTlevels = T,fbanner = bandera1),
   AP19BBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P19_B_AMBOS_Otro__Especificar=="TRUE")),],fTvariables ="P19_b_bancor",fTlevels = T,fbanner = bandera1),
   AP19BBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P19_B_AMBOS_No_sabe__no_contest=="TRUE")),],fTvariables ="P19_b_bancor",fTlevels = T,fbanner = bandera1),
   AP19B1  = frecuentator(fTtabla = datos[datos$P10=="Ambas",], fTvariables = nombresR(datos,"P19_B_1")[1:4],fTlevels = F,fbanner = bandera1),
   AP20 = frecuentator(fTtabla = datos[datos$P10=="Ambas",], fTvariables = "P20",fTlevels = T,fbanner = bandera1),
   AP18B  = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P20=="Si")),], fTvariables = "P20_B_AMBOS",fTlevels = T,fbanner = bandera1),
   AP18C = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P20=="Si")),],fTvariables =(nombresR(datos,"P20_C_AMBOS")[1:6]),fTlevels = F,fbanner = bandera1),
   AP20CBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P20=="Si") & (datos$P20_C_AMBOS_Tarjeta_de_Cr_dito=="TRUE")),],fTvariables ="P20_c_bancor",fTlevels = T,fbanner = bandera1),
   AP20CBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P20=="Si") & (datos$P20_C_AMBOS_Cr_dito_personal_del_banco=="TRUE")),],fTvariables ="P20_c_bancor",fTlevels = T,fbanner = bandera1),
   AP20CBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P20=="Si") & (datos$P20_C_AMBOS_Otro__Especificar=="TRUE")),],fTvariables ="P20_c_bancor",fTlevels = T,fbanner = bandera1),
   AP20CBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P20=="Si") & (datos$P20_C_AMBOS_No_sabe__no_contest=="TRUE")),],fTvariables ="P20_c_bancor",fTlevels = T,fbanner = bandera1),
   Ap18E = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" ,], fTvariables =nombresR(datos,"P20_D_")[1:12],fTlevels = F,fbanner = bandera1),
   Ap18E1 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_D_AMBOS_Nissan=="TRUE",], fTvariables =nombresR(datos,"P20_E_")[1:12],fTlevels = F,fbanner = bandera1),
   Ap18E2 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_D_AMBOS_General_Motors=="TRUE",], fTvariables =nombresR(datos,"P20_E_")[14:25],fTlevels = F,fbanner = bandera1),
   Ap18E3 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_D_AMBOS_Volkswagen=="TRUE",], fTvariables =nombresR(datos,"P20_E_")[27:38],fTlevels = F,fbanner = bandera1),
   Ap18E5 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_D_AMBOS_Ford_Motor=="TRUE",], fTvariables =nombresR(datos,"P20_E_")[53:64],fTlevels = F,fbanner = bandera1),
   Ap18E6 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_D_AMBOS_Toyota=="TRUE",], fTvariables =nombresR(datos,"P20_E_")[66:77],fTlevels = F,fbanner = bandera1),
   Ap18E8 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_D_AMBOS_Mazda=="TRUE",], fTvariables =nombresR(datos,"P20_E_")[92:103],fTlevels = F,fbanner = bandera1),
   Ap18E10 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_D_AMBOS_Hyundai=="TRUE",], fTvariables =nombresR(datos,"P20_E_")[105:116],fTlevels = F,fbanner = bandera1),
   Ap18E11 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_D_AMBOS_Renault=="TRUE",], fTvariables =nombresR(datos,"P20_E_")[118:129],fTlevels = F,fbanner = bandera1),
   Ap18E12 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_D_AMBOS_Mitsubichi=="TRUE",], fTvariables =nombresR(datos,"P20_E_")[131:142],fTlevels = F,fbanner = bandera1)   
 )
 exportator(resultadosAmbosAutos, "./resultados/resultadosAmbosAutos.csv")
 
   
  
resultadosAmbosMotos<- list(
  
  
  MP15NumAmbosMoto = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_8_MOTO",fTlevels = T,fbanner = bandera1),
  MP15UsoAmbosMoto = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"Rr_P_15_9_MOTO"),fTlevels = F,fbanner = bandera1),
  MP15MarcaAmbosMoto = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P15_6_MARCA_AMBOS"),fTlevels = T,fbanner = bandera1),
  MP15AmbosCategoriaMoto = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P15_6_CATEGORIA_AMBOS"),fTlevels = T,fbanner = bandera1),
  MP15AntAmbosMoto = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P15_7_AMBOS"),fTlevels = T,fbanner = bandera1),
  MP17PrestarAmbosMotos = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_A_AMBOS",fTlevels = T,fbanner = bandera1),
  MP17PrestarQuienAmbosMotos = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "Sí")),],fTvariables =nombresR(datos,"P17_B_AMBOS"),fTlevels = F,fbanner = bandera1),
  MP17PrestarQuienAmbosMotosf1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "Sí")&(datos$P17_B_AMBOS_Mi_Esposo___Esposa=="TRUE")),],fTvariables ="P17_C_AMBOS_ESPOSAESPOSO",fTlevels = T,fbanner = bandera1),
  MP17PrestarQuienAmbosMotosf2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "Sí")&(datos$P17_B_AMBOS_Mi_Pap____Mam=="TRUE")),],fTvariables ="P17_C_AMBOS_MAMAPAPA",fTlevels = T,fbanner = bandera1),
  MP17PrestarQuienAmbosMotosf3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "Sí")&(datos$P17_B_AMBOS_Hijos___Hijas=="TRUE")),],fTvariables ="P17_C_AMBOS_HIJOHIJA",fTlevels = T,fbanner = bandera1),
  MP17PrestarQuienAmbosMotosf4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "Sí")&(datos$P17_B_AMBOS_Otro_familiar=="TRUE")),],fTvariables ="P17_C_AMBOS_OTROFAMILIAR",fTlevels = T,fbanner = bandera1),
  MP17AmbosPrestarQuienMotosRazon1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "Sí")&(datos$P17_B_AMBOS_Mi_Esposo___Esposa=="TRUE")),],fTvariables = nombresR(datos,"P17_D_AMBOS_ESPOSAESPOSO")[1:12],fTlevels = F,fbanner = bandera1),
  MP17AmbosPrestarQuienMotosRazon2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "Sí")&(datos$P17_B_AMBOS_Mi_Pap____Mam=="TRUE")),],fTvariables =nombresR(datos,"P17_D_AMBOS_MAMAPA")[1:12],fTlevels = F,fbanner = bandera1),
  MP17AmbosPrestarQuienMotosRazon3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "Sí")&(datos$P17_B_AMBOS_Hijos___Hijas=="TRUE")),],fTvariables =nombresR(datos,"P17_D_AMBOS_HIJOHIJA")[1:12],fTlevels = F,fbanner = bandera1),
  MP17AmbosPrestarQuienMotosRazon4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "Sí")&(datos$P17_B_AMBOS_Otro_familiar=="TRUE")),],fTvariables =nombresR(datos,"P17_D_AMBOS_OTROFAMILIAR")[1:12],fTlevels = F,fbanner = bandera1),
  MP17NoPrestaMotosPQ = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "No")),],fTvariables ="P17_E_AMBOS",fTlevels = T,fbanner = bandera1), 
  MP17NoPrestaMotosProb = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "No")),],fTvariables ="P17_F",fTlevels = T,fbanner = bandera1),
  MP17NoPrestaMotosProbQ = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "No")&(datos$P17_F== 'Sí')),],fTvariables ="P17_G_AMBOS",fTlevels = T,fbanner = bandera1),
  MP17NoPrestarQuienMotosSol1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17_A_AMBOS== "No")&(datos$P17_G_AMBOS=="Esposa/ Esposo")),],fTvariables = "P17_H_AMBOS_ESPOSAESPOSO",fTlevels = T,fbanner = bandera1),
  MP17Actividades1 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_ir_a_trabajar",fTlevels = T,fbanner = bandera3),
  MP17Actividades2 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_ir_al_Doctor___Den",fTlevels = T,fbanner = bandera3),
  MP17Actividades3 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_llevar_hijos_a_la_",fTlevels = T,fbanner = bandera3),
  MP17Actividades4 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_salir_de_paseo___s",fTlevels = T,fbanner = bandera3),
  MP17Actividades5 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_salir_de_vacacione",fTlevels = T,fbanner = bandera3),
  MP17Actividades6 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_llevar___transport",fTlevels = T,fbanner = bandera3),
  MP17Actividades7 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_salir_de_fiesta",fTlevels = T,fbanner = bandera3),
  MP17Actividades8 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_ir_al_s_per___merc",fTlevels = T,fbanner = bandera3),
  MP17Actividades9 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_ir_al_banco",fTlevels = T,fbanner = bandera3),
  MP17Actividades10 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_ir_a_la_Escuela___",fTlevels = T,fbanner = bandera3),
  MP17Actividades11 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_ir_a_visitar_a_mi_",fTlevels = T,fbanner = bandera3),
  MP17Actividades12 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_J_AMBOS_La_uso_para_salir_a_las_rodada",fTlevels = T,fbanner = bandera3),
  MAP17Preferencia = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P17_K_AMBOS",fTlevels = T,fbanner = bandera1),
  MAP17PREFERENCIAMOTOS= frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables =nombresR(datos,"P17_M_AMBOS")[1:7],fTlevels = F,fbanner = bandera1),
  MP16InfluenciaMotos = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables =nombresR(datos,"P17_O_")[c(2,4,6,8,10,12)],fTlevels = F,fbanner = bandera1),
  MP16CompraMotos = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P17_P_")[c(2,4,6,8,10,12,14)],fTlevels = F,fbanner = bandera1),
  MP18 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables ="P18_AMBOS_Moto",fTlevels = T,fbanner = bandera1),
  MP18Cambio = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P18_A_Ambos_")[c(2,4,6,8,10,12,14,16,18,20,22,24)],fTlevels = F,fbanner = bandera1),
  MP18TransporteEficiente = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables ="P18_B_AMBOS",fTlevels = T,fbanner = bandera1),
  MP18C1 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P18_D_AMBOS_Me_da_prestigio___status",fTlevels = T,fbanner = bandera1),
  MP18C2 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P18_D_AMBOS_Me_da_tranquilidad",fTlevels = T,fbanner = bandera1),
  MP18C3 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P18_D_AMBOS_Me_da_confianza",fTlevels = T,fbanner = bandera1),
  MP18C4 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P18_D_AMBOS_Me_da_seguridad",fTlevels = T,fbanner = bandera1),
  MP18C5 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P18_D_AMBOS_Me_siento_contento__a",fTlevels = T,fbanner = bandera1),
  MP19 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables ="P19_C_AMBOS",fTlevels = T,fbanner = bandera1),
  MP19A = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P19_C_AMBOS== "Es usada")),],fTvariables ="P19_D_AMBOS",fTlevels = T,fbanner = bandera1),
  MP19B = frecuentator(fTtabla = datos[(datos$P10=="Ambas"),],fTvariables =(nombresR(datos,"P19_E_")[1:6]),fTlevels = F,fbanner = bandera1),
  MP19BBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P19_E_AMBOS_Tarjeta_de_Cr_dito=="TRUE")),],fTvariables ="P20_e_bancor",fTlevels = T,fbanner = bandera1),
  MP19BBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P19_E_AMBOS_Cr_dito_personal_del_banco=="TRUE")),],fTvariables ="P20_e_bancor",fTlevels = T,fbanner = bandera1),
  MP19BBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P19_E_AMBOS_Otro__Especificar=="TRUE")),],fTvariables ="P20_e_bancor",fTlevels = T,fbanner = bandera1),
  MP19BBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P19_E_AMBOS_No_sabe__no_contest=="TRUE")),],fTvariables ="P20_e_bancor",fTlevels = T,fbanner = bandera1),
  MP19E1  = frecuentator(fTtabla = datos[datos$P10=="Ambas",], fTvariables = nombresR(datos,"P19_E_1")[1:4],fTlevels = F,fbanner = bandera1),
  MP20 = frecuentator(fTtabla = datos[datos$P10=="Ambas",], fTvariables = "P20_F",fTlevels = T,fbanner = bandera1),
  MP20H  = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P20_F=="Si")),], fTvariables = "P20_H_AMBOS",fTlevels = T,fbanner = bandera1),
  MP18C = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P20_F=="Si")),],fTvariables =(nombresR(datos,"P20_I_AMBOS")[1:6]),fTlevels = F,fbanner = bandera1),
  MAp18E = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si",], fTvariables =nombresR(datos,"P20_J_")[1:10],fTlevels = F,fbanner = bandera1),
  MAMP20F1 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_J_AMBOS_Italika=="TRUE",], fTvariables =nombresR(datos,"P20_K_")[1:15],fTlevels = F,fbanner = bandera1),
  MAMP20F2 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_J_AMBOS_Yamaha=="TRUE",], fTvariables =nombresR(datos,"P20_K_")[17:31],fTlevels = F,fbanner = bandera1),
  MAMP20F3 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_J_AMBOS_Honda=="TRUE",], fTvariables =nombresR(datos,"P20_K_")[33:47],fTlevels = F,fbanner = bandera1),
  MAMP20F4 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_J_AMBOS_Suzuki=="TRUE",], fTvariables =nombresR(datos,"P20_K_")[49:63],fTlevels = F,fbanner = bandera1),
  MAMP20F5 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_J_AMBOS_Bajaj=="TRUE",], fTvariables =nombresR(datos,"P20_K_")[65:79],fTlevels = F,fbanner = bandera1),
  MAMP20F6 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_J_AMBOS_Kawasaki=="TRUE",], fTvariables =nombresR(datos,"P20_K_")[81:95],fTlevels = F,fbanner = bandera1),
  MAMP20F7 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P20=="Si" & datos$P20_J_AMBOS_Harley_Davidson=="TRUE",], fTvariables =nombresR(datos,"P20_K_")[97 :111],fTlevels = F,fbanner = bandera1) 
  
)

exportator(resultadosAmbosMotos, "./resultados/resultadosAmbosMotos.csv")

##Maxdiff1------------

#Selecciono las columnas que van en el max diff
MaxDiff<- datos %>%
  filter(datos$P10=="Automóvil") %>%
  select(contains("AUTO_BLOQUE"),-contains("AMBOS")) 

names(MaxDiff)

##Cambio los NA por 0
#Funcion que quita NA
qna <- function(var){
  b <- as.character(var)
  b[which(is.na(b))] <- "0"
  b <- factor(b)
  return(b)
}

for (k in 1:ncol(MaxDiff)){
  MaxDiff[[k]] <- qna(var = MaxDiff[[k]])
}

head(MaxDiff)
###Selecciono el nombre de los atributos
nombresItems<-unique(str_sub(names(MaxDiff),18,-1))
atributos1<-unique(str_sub(names(MaxDiff),18,-1))
##Asigno nuevos nobres a las variables, del tipo BiVk  #donde la i es el numero de bloque y la k el numero de atributo
MaxDiff<- nombresMaxDiff(MaxDiff,6,nombresItems,18)
names(MaxDiff)

#incializo el data frame
b<-MaxDiff
## el 18 es de numero de atributos por dos
nombres<-paste0('v',1:18)
a<-data.frame(as.factor(nombres))
a<-cbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
a<-a[0,]
names(a)<-paste0('v',1:18)
a<-a %>%
  mutate_each(funs(as.character))

names(a)<-paste0('v',1:18)

#lleno el data frame
names(b)<-str_sub(names(b),3,-1)
for(k in 1:length(b)){
  for(i in 1:9){
    for(j in 1:6){
      if(b[k,(i-1)*6+j]=='Más importante'){
        a[k,(i-1)*2+1]<-names(b)[(i-1)*6+j]
      }
      if(b[k,(i-1)*6+j]=='Menos importante'){
        a[k,(i-1)*2+2]<-names(b)[(i-1)*6+j]
      }
    }
  }
  
}

head(a)

funcionLlenaBloque<-function(bloque=2,nBloques=9,atributos=nombresItems,matriz=Maxdiff){
  
  nAtributos<-length(nombresItems)
  z<-matrix(nrow=1,ncol=nAtributos)
  z<-as.data.frame(z)
  k<-1
  for(j in 1:nAtributos){
    names(z)[k]<-paste0('b',bloque,'v',j)
    k<-k+1
  }
  z<- z %>%
    mutate_each(funs(as.numeric))
  matcheo<-match(levels(matriz[,(1+(2*(bloque-1)))]),nombresItems)
  for(i in 1:nrow(matriz)){
    z[i,matcheo]<-0
    matcheoMejor<-match(matriz[i,(1+(2*(bloque-1)))],nombresItems)
    z[i,matcheoMejor]<-1
    matcheoPeor<-match(matriz[i,(2+(2*(bloque-1)))],nombresItems)
    z[i,matcheoPeor]<-(-1)
  }
  
  return(z)
  
}



datosMaxdiff<-a
datosMaxdiff<-na.omit(datosMaxdiff)

for(i in 1:length(datosMaxdiff)){
  datosMaxdiff[,i]<-factor(datosMaxdiff[,i])
}

#cambio la estructura de esa cosa



nombresItems<-unique(c(levels(datosMaxdiff[,1]),
                       levels(datosMaxdiff[,3]),
                       levels(datosMaxdiff[,5]),
                       levels(datosMaxdiff[,7]),
                       levels(datosMaxdiff[,9]),
                       levels(datosMaxdiff[,11]),
                       levels(datosMaxdiff[,13]),
                       levels(datosMaxdiff[,15]),
                       levels(datosMaxdiff[,17])))

bloques<-list()
for(i in 1:9){
  bloques[[i]]<-funcionLlenaBloque(bloque=i,nBloques=9,atributos=nombresItems,matriz=datosMaxdiff)
}
z<-do.call('cbind',bloques)

nItems = length(nombresItems)
nBloques = ncol(z) / nItems
nItemsPorBloque = 6
n = nrow(z)
nObservaciones = n * nBloques 
datosMaxDiff = matrix(as.numeric(t(z)),ncol = nItems,byrow = TRUE, dimnames = list(1:nObservaciones, nombresItems))

#análisis de conteos

conteos = apply(datosMaxDiff, 2, mean, na.rm = TRUE)
rankings = nItems + 1 - rank(conteos)
global<-cbind(Conteos = conteos, Rankings = rankings)
global

#análisis de conteos individuales

id = rep(1:n,rep(nBloques,n))
conteosIndividuales = aggregate(datosMaxDiff,list(id),mean, na.rm = TRUE)[,-1]
round(conteosIndividuales[1:17,],1) 

#calculo la densidad

set.seed(23567) # prefijo la semilla para tener reproducibilidad
conteosIndividualesSinEmpates = conteosIndividuales + matrix(runif(n * nItems)/100000, n) #adding random numbers to break ties
rankings = nItems + 1 - apply(conteosIndividualesSinEmpates,1,rank) #rankings

sucio<-apply(rankings,1,table)

limpio<-list()
for(i in 1:length(sucio)){
  completo<-as.character(1:nItems)
  miniLimpio<-rep(0,nItems)
  llave<-match(completo,names(sucio[[i]]))
  miniLimpio<-sucio[[i]][llave]
  limpio[[i]]<-miniLimpio
}

limpio<-do.call('cbind',limpio)
rownames(limpio)<-1:nItems
limpio[is.na(limpio)]<-0
proporcionesRankings<-t(limpio/n*100)
round(proporcionesRankings,1)
#vemos la misma distribución pero de forma acumulativa
proporcionesAcumulativasRankings = t(apply(proporcionesRankings,1,cumsum))
round(proporcionesAcumulativasRankings,1)
#calculo los promedios de los rankings como sigue

rankingsPromedio = proporcionesRankings %*% (1:17)/100
cbind(rankingsPromedio, Ranking = rank(rankingsPromedio))

####Modelo Logit Total
#Previo a ejecutar el modelo logit es necesario construir una base con "truco"
#Creamos la variable "Eleccion", es una variable indicadora de la marca seleccionada
#por el respondente

#nRows = 90000 #no. de alternativas por bloque* 2*no. de bloques*no. de personas
nRows=nItemsPorBloque*2*nBloques*n
datosLargos = matrix(0, nRows,nItems + 3)
counter = 0
setCounter = 0
for (rr in 1:nObservaciones){
  nAlts = 0
  alternatives = NULL
  respondent = floor(rr/nBloques) + 1
  for (cc in 1:nItems){
    v = datosMaxDiff[rr,cc]
    if (!is.na(v)){
      nAlts = nAlts + 1
      alternatives[nAlts] = cc
      if (v == 1)
        best = cc
      if (v == -1)
        worst = cc
    }
  }
  setCounter = setCounter + 1
  for (a in 1:nAlts){
    counter = counter + 1
    this_a = alternatives[a]
    if (this_a == best)
      datosLargos[counter,3] = 1
    else if (this_a == worst)
      datosLargos[counter + nAlts,3] = 1
    datosLargos[counter, 1] = respondent 
    datosLargos[counter + nAlts,1] = respondent 
    datosLargos[counter, 2] = setCounter 
    datosLargos[counter + nAlts, 2] = setCounter + 1
    datosLargos[counter,3 + this_a] = 1
    datosLargos[counter + nAlts,3 + this_a] = -1
  }
  setCounter = setCounter + 1
  counter = counter + nAlts
}

datosLargos = as.data.frame(datosLargos)
#names(datosLargosDE) = c("ID","Set","Eleccion",nombresItems)

nombres<-paste0('v',1:17)
names(datosLargos)<-c("ID","Set","Eleccion",nombres)

datosLargos[1:20,]
tail(datosLargos)
datosLargos<-datosLargos %>%
  filter(ID!=0)

#corro el logit

logitModel = mlogit(Eleccion ~ v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12+v13+v14+v15+v16+v17 | 0, 
                    data = datosLargos, alt.levels = paste(1:nItemsPorBloque), 
                    shape = "long")
summary(logitModel)

#comparamos los modelos

b<-cbind(rankingsPromedio, Rankings = rank(rankingsPromedio))
c<-cbind(Conteos=c(0,as.numeric(logitModel$coefficients)),rankings=nItems+1-rank(c(0,as.numeric(logitModel$coefficients))))
resultados<-cbind(promedioGlobal=global[,2],promedioIndividual=b[,2],logit=c[,2])
resultados<-data.frame(resultados)
resultados$atributos<-rownames(resultados)

resultados<-resultados[,c(4,1,2,3)]
resultados
resultados<-resultados %>%
  dplyr::arrange(logit)
resultados
write.csv(resultados,'RankeoMaxDiff1.csv')
valores<-c(0,logitModel$coefficients)
resultadoFinal<-cbind(valores,atributos1)
resultadoFinal
write.csv(resultadoFinal,'resultado final max diff1.csv')

###Maxdiff2----


#Selecciono las columnas que van en el max diff
MaxDiff<- datos %>%
  filter(datos$P10=="Moto / Motocicleta") %>%
  select(contains("MOTO_BLOQUE"),-contains("AMBOS")) 

names(MaxDiff)

##Cambio los NA por 0

for (k in 1:ncol(MaxDiff)){
  MaxDiff[[k]] <- qna(var = MaxDiff[[k]])
}

head(MaxDiff)
###Selecciono el nombre de los atributos
temp1<-unique(str_sub(names(MaxDiff)[1:45],18,-1))
temp2<-unique(str_sub(names(MaxDiff)[46:55],19,-1))
nombresItems<-unique(temp1,temp2)
                     
atributos2<-unique(temp1,temp2)
##Asigno nuevos nobres a las variables, del tipo BiVk  #donde la i es el numero de bloque y la k el numero de atributo

MaxDiff<- nombresMaxDiff(MaxDiff,5,nombresItems,18)
names(MaxDiff)


#incializo el data frame
b<-MaxDiff
## el 18 es de numero de atributos por dos
nombres<-paste0('v',1:22)
a<-data.frame(as.factor(nombres))
a<-cbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
a<-a[0,]
names(a)<-paste0('v',1:22)
a<-a %>%
  mutate_each(funs(as.character))

names(a)<-paste0('v',1:22)

#lleno el data frame
names(b)<-str_sub(names(b),3,-1)
for(k in 1:length(b)){
  for(i in 1:11){
    for(j in 1:5){
      if(b[k,(i-1)*5+j]=='Más importante'){
        a[k,(i-1)*2+1]<-names(b)[(i-1)*5+j]
      }
      if(b[k,(i-1)*5+j]=='Menos importante'){
        a[k,(i-1)*2+2]<-names(b)[(i-1)*5+j]
      }
    }
  }
  
}

head(a)


datosMaxdiff<-a
datosMaxdiff<-na.omit(datosMaxdiff)

for(i in 1:length(datosMaxdiff)){
  datosMaxdiff[,i]<-factor(datosMaxdiff[,i])
}

#cambio la estructura de esa cosa

nombresItems<-unique(c(levels(datosMaxdiff[,1]),
                       levels(datosMaxdiff[,3]),
                       levels(datosMaxdiff[,5]),
                       levels(datosMaxdiff[,7]),
                       levels(datosMaxdiff[,9]),
                       levels(datosMaxdiff[,11]),
                       levels(datosMaxdiff[,13]),
                       levels(datosMaxdiff[,15]),
                       levels(datosMaxdiff[,17]),
                       levels(datosMaxdiff[,19]),
                       levels(datosMaxdiff[,21])
                       ))

bloques<-list()
for(i in 1:11){
  bloques[[i]]<-funcionLlenaBloque(bloque=i,nBloques=11,atributos=nombresItems,matriz=datosMaxdiff)
}
z<-do.call('cbind',bloques)

nItems = length(nombresItems)
nBloques = ncol(z) / nItems
nItemsPorBloque = 5
n = nrow(z)
nObservaciones = n * nBloques 
datosMaxDiff = matrix(as.numeric(t(z)),ncol = nItems,byrow = TRUE, dimnames = list(1:nObservaciones, nombresItems))

#análisis de conteos

conteos = apply(datosMaxDiff, 2, mean, na.rm = TRUE)
rankings = nItems + 1 - rank(conteos)
global<-cbind(Conteos = conteos, Rankings = rankings)
global

#análisis de conteos individuales

id = rep(1:n,rep(nBloques,n))
conteosIndividuales = aggregate(datosMaxDiff,list(id),mean, na.rm = TRUE)[,-1]
round(conteosIndividuales[1:17,],1) 

#calculo la densidad

set.seed(23567) # prefijo la semilla para tener reproducibilidad
conteosIndividualesSinEmpates = conteosIndividuales + matrix(runif(n * nItems)/100000, n) #adding random numbers to break ties
rankings = nItems + 1 - apply(conteosIndividualesSinEmpates,1,rank) #rankings

sucio<-apply(rankings,1,table)

limpio<-list()
for(i in 1:length(sucio)){
  completo<-as.character(1:nItems)
  miniLimpio<-rep(0,nItems)
  llave<-match(completo,names(sucio[[i]]))
  miniLimpio<-sucio[[i]][llave]
  limpio[[i]]<-miniLimpio
}

limpio<-do.call('cbind',limpio)
rownames(limpio)<-1:nItems
limpio[is.na(limpio)]<-0
proporcionesRankings<-t(limpio/n*100)
round(proporcionesRankings,1)
#vemos la misma distribución pero de forma acumulativa
proporcionesAcumulativasRankings = t(apply(proporcionesRankings,1,cumsum))
round(proporcionesAcumulativasRankings,1)
#calculo los promedios de los rankings como sigue

rankingsPromedio = proporcionesRankings %*% (1:18)/100
cbind(rankingsPromedio, Ranking = rank(rankingsPromedio))

####Modelo Logit Total
#Previo a ejecutar el modelo logit es necesario construir una base con "truco"
#Creamos la variable "Eleccion", es una variable indicadora de la marca seleccionada
#por el respondente

#nRows = 90000 #no. de alternativas por bloque* 2*no. de bloques*no. de personas
nRows=nItemsPorBloque*2*nBloques*n
datosLargos = matrix(0, nRows,nItems + 3)
counter = 0
setCounter = 0
for (rr in 1:nObservaciones){
  nAlts = 0
  alternatives = NULL
  respondent = floor(rr/nBloques) + 1
  for (cc in 1:nItems){
    v = datosMaxDiff[rr,cc]
    if (!is.na(v)){
      nAlts = nAlts + 1
      alternatives[nAlts] = cc
      if (v == 1)
        best = cc
      if (v == -1)
        worst = cc
    }
  }
  setCounter = setCounter + 1
  for (a in 1:nAlts){
    counter = counter + 1
    this_a = alternatives[a]
    if (this_a == best)
      datosLargos[counter,3] = 1
    else if (this_a == worst)
      datosLargos[counter + nAlts,3] = 1
    datosLargos[counter, 1] = respondent 
    datosLargos[counter + nAlts,1] = respondent 
    datosLargos[counter, 2] = setCounter 
    datosLargos[counter + nAlts, 2] = setCounter + 1
    datosLargos[counter,3 + this_a] = 1
    datosLargos[counter + nAlts,3 + this_a] = -1
  }
  setCounter = setCounter + 1
  counter = counter + nAlts
}

datosLargos = as.data.frame(datosLargos)
#names(datosLargosDE) = c("ID","Set","Eleccion",nombresItems)

nombres<-paste0('v',1:18)
names(datosLargos)<-c("ID","Set","Eleccion",nombres)

datosLargos[1:20,]
tail(datosLargos)
datosLargos<-datosLargos %>%
  filter(ID!=0)

#corro el logit

logitModel = mlogit(Eleccion ~ v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12+v13+v14+v15+v16+v17+v18 | 0, 
                    data = datosLargos, alt.levels = paste(1:nItemsPorBloque), 
                    shape = "long")
summary(logitModel)

#comparamos los modelos

b<-cbind(rankingsPromedio, Rankings = rank(rankingsPromedio))
c<-cbind(Conteos=c(0,as.numeric(logitModel$coefficients)),rankings=nItems+1-rank(c(0,as.numeric(logitModel$coefficients))))
resultados<-cbind(promedioGlobal=global[,2],promedioIndividual=b[,2],logit=c[,2])
resultados<-data.frame(resultados)
resultados$atributos<-rownames(resultados)

resultados<-resultados[,c(4,1,2,3)]
resultados
resultados<-resultados %>%
  dplyr::arrange(logit)
resultados
write.csv(resultados,'RankeoMaxDiff1.csv')
valores<-c(0,logitModel$coefficients)
resultadoFinal<-cbind(valores,atributos1)
resultadoFinal
write.csv(resultadoFinal,'resultado final max diff1.csv')

##MaxDiff3-----
#Selecciono las columnas que van en el max diff
MaxDiff<- datos %>%
  filter(datos$P10=="Automóvil" & datos$P18=="Si") %>%
  select(contains("AUTO_MAS"),-contains("AMBOS")) 

names(MaxDiff)

##Cambio los NA por 0
#Funcion que quita NA
qna <- function(var){
  b <- as.character(var)
  b[which(is.na(b))] <- "0"
  b <- factor(b)
  return(b)
}

for (k in 1:ncol(MaxDiff)){
  MaxDiff[[k]] <- qna(var = MaxDiff[[k]])
}

head(MaxDiff)
###Selecciono el nombre de los atributos
nombresItems<-unique(str_sub(names(MaxDiff),18,-1))
atributos3<-unique(str_sub(names(MaxDiff),18,-1))
##Asigno nuevos nobres a las variables, del tipo BiVk  #donde la i es el numero de bloque y la k el numero de atributo
MaxDiff<- nombresMaxDiff(MaxDiff,5,nombresItems,18)
names(MaxDiff)

#incializo el data frame
b<-MaxDiff
## el 18 es de numero de bloques por dos
nombres<-paste0('v',1:16)
a<-data.frame(as.factor(nombres))
a<-cbind(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
a<-a[0,]
names(a)<-paste0('v',1:16)
a<-a %>%
  mutate_each(funs(as.character))

names(a)<-paste0('v',1:16)

#lleno el data frame
names(b)<-str_sub(names(b),3,-1)
for(k in 1:length(b)){
  for(i in 1:8){
    for(j in 1:5){
      if(b[k,(i-1)*5+j]=='Más importante'){
        a[k,(i-1)*2+1]<-names(b)[(i-1)*5+j]
      }
      if(b[k,(i-1)*5+j]=='Menos importante'){
        a[k,(i-1)*2+2]<-names(b)[(i-1)*5+j]
      }
    }
  }
  
}

head(a)

datosMaxdiff<-a
datosMaxdiff<-na.omit(datosMaxdiff)

for(i in 1:length(datosMaxdiff)){
  datosMaxdiff[,i]<-factor(datosMaxdiff[,i])
}

#cambio la estructura de esa cosa



nombresItems<-unique(c(levels(datosMaxdiff[,1]),
                       levels(datosMaxdiff[,3]),
                       levels(datosMaxdiff[,5]),
                       levels(datosMaxdiff[,7]),
                       levels(datosMaxdiff[,9]),
                       levels(datosMaxdiff[,11]),
                       levels(datosMaxdiff[,13]),
                       levels(datosMaxdiff[,15])))

bloques<-list()
for(i in 1:8){
  bloques[[i]]<-funcionLlenaBloque(bloque=i,nBloques=8,atributos=nombresItems,matriz=datosMaxdiff)
}
z<-do.call('cbind',bloques)

nItems = length(nombresItems)
nBloques = ncol(z) / nItems
nItemsPorBloque = 5
n = nrow(z)
nObservaciones = n * nBloques 
datosMaxDiff = matrix(as.numeric(t(z)),ncol = nItems,byrow = TRUE, dimnames = list(1:nObservaciones, nombresItems))

#análisis de conteos

conteos = apply(datosMaxDiff, 2, mean, na.rm = TRUE)
rankings = nItems + 1 - rank(conteos)
global<-cbind(Conteos = conteos, Rankings = rankings)
global

#análisis de conteos individuales

id = rep(1:n,rep(nBloques,n))
conteosIndividuales = aggregate(datosMaxDiff,list(id),mean, na.rm = TRUE)[,-1]
round(conteosIndividuales[1:12,],1) 

#calculo la densidad

set.seed(23567) # prefijo la semilla para tener reproducibilidad
conteosIndividualesSinEmpates = conteosIndividuales + matrix(runif(n * nItems)/100000, n) #adding random numbers to break ties
rankings = nItems + 1 - apply(conteosIndividualesSinEmpates,1,rank) #rankings

sucio<-apply(rankings,1,table)

limpio<-list()
for(i in 1:length(sucio)){
  completo<-as.character(1:nItems)
  miniLimpio<-rep(0,nItems)
  llave<-match(completo,names(sucio[[i]]))
  miniLimpio<-sucio[[i]][llave]
  limpio[[i]]<-miniLimpio
}

limpio<-do.call('cbind',limpio)
rownames(limpio)<-1:nItems
limpio[is.na(limpio)]<-0
proporcionesRankings<-t(limpio/n*100)
round(proporcionesRankings,1)
#vemos la misma distribución pero de forma acumulativa
proporcionesAcumulativasRankings = t(apply(proporcionesRankings,1,cumsum))
round(proporcionesAcumulativasRankings,1)
#calculo los promedios de los rankings como sigue

rankingsPromedio = proporcionesRankings %*% (1:12)/100
cbind(rankingsPromedio, Ranking = rank(rankingsPromedio))

####Modelo Logit Total
#Previo a ejecutar el modelo logit es necesario construir una base con "truco"
#Creamos la variable "Eleccion", es una variable indicadora de la marca seleccionada
#por el respondente

#nRows = 90000 #no. de alternativas por bloque* 2*no. de bloques*no. de personas
nRows=nItemsPorBloque*2*nBloques*n
datosLargos = matrix(0, nRows,nItems + 3)
counter = 0
setCounter = 0
for (rr in 1:nObservaciones){
  nAlts = 0
  alternatives = NULL
  respondent = floor(rr/nBloques) + 1
  for (cc in 1:nItems){
    v = datosMaxDiff[rr,cc]
    if (!is.na(v)){
      nAlts = nAlts + 1
      alternatives[nAlts] = cc
      if (v == 1)
        best = cc
      if (v == -1)
        worst = cc
    }
  }
  setCounter = setCounter + 1
  for (a in 1:nAlts){
    counter = counter + 1
    this_a = alternatives[a]
    if (this_a == best)
      datosLargos[counter,3] = 1
    else if (this_a == worst)
      datosLargos[counter + nAlts,3] = 1
    datosLargos[counter, 1] = respondent 
    datosLargos[counter + nAlts,1] = respondent 
    datosLargos[counter, 2] = setCounter 
    datosLargos[counter + nAlts, 2] = setCounter + 1
    datosLargos[counter,3 + this_a] = 1
    datosLargos[counter + nAlts,3 + this_a] = -1
  }
  setCounter = setCounter + 1
  counter = counter + nAlts
}

datosLargos = as.data.frame(datosLargos)
#names(datosLargosDE) = c("ID","Set","Eleccion",nombresItems)

nombres<-paste0('v',1:12)
names(datosLargos)<-c("ID","Set","Eleccion",nombres)

datosLargos[1:50,]
tail(datosLargos)
datosLargos<-datosLargos %>%
  filter(ID!=0)

#corro el logit

logitModel = mlogit(Eleccion ~ v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12 | 0, 
                    data = datosLargos, alt.levels = paste(1:nItemsPorBloque), 
                    shape = "long")
summary(logitModel)

#comparamos los modelos

b<-cbind(rankingsPromedio, Rankings = rank(rankingsPromedio))
c<-cbind(Conteos=c(0,as.numeric(logitModel$coefficients)),rankings=nItems+1-rank(c(0,as.numeric(logitModel$coefficients))))
resultados<-cbind(promedioGlobal=global[,2],promedioIndividual=b[,2],logit=c[,2])
resultados<-data.frame(resultados)
resultados$atributos<-rownames(resultados)

resultados<-resultados[,c(4,1,2,3)]
resultados
resultados<-resultados %>%
  dplyr::arrange(logit)
resultados
write.csv(resultados,'RankeoMaxDiff3.csv')
valores<-c(0,logitModel$coefficients)
resultadoFinal<-cbind(valores,atributos3)
resultadoFinal
write.csv(resultadoFinal,'resultado final max diff3.csv')

