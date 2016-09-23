#hola mundo
devtools::install_github("metronhomo/brostatistics")
library(dplyr)
library(brostatistics)
library(sjmisc)


 datos <- gaseosa(xfile = "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS 13-09-2016 18-23-09.csv",yfile = "CICLO DE VIDA DENTRO DEL HOGAR AUTOS_MOTOS_DataMap_13-09-2016 18_24_04.xlsx")
# 
 names(datos)[188] <- "P15_12_1"
# 
 saveRDS(datos,"datos.rds")

datos <- readRDS(
  "datos.rds"
)


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
bandera3 <- c("TOtal","P14")

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
  p15Modelo = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = nombresR(datos,"P13ModeloLimpia"),fTlevels = T,fbanner = bandera1),
  P15UsoAutos = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = nombresR(datos,"Rr_P15_2"),fTlevels = F,fbanner = bandera1),
  P15AntAutos = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = nombresR(datos,"P15_5"),fTlevels = T,fbanner = bandera1),
  P15PrestarAutos = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P15_A",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienAutos = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")),],fTvariables =nombresR(datos,"P15_B"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienAutosf1 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Esposo___Esposa=="1")),],fTvariables ="P15_C_EsposaEsposo",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienAutosf2 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Pap____Mam=="1")),],fTvariables ="P15_C_PapaMama",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienAutosf3 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Hijos___Hijas=="1")),],fTvariables ="P15_C_HijoHija",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienAutosf4 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Otro_familiar=="1")),],fTvariables ="P15_C_OtroFamiliar",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienAutosRazon1 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Esposo___Esposa=="1")),],fTvariables = nombresR(datos,"P15_D_EsposoEsposa"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienAutosRazon2 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Pap____Mam=="1")),],fTvariables =nombresR(datos,"P15_D_PapaMama"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienAutosRazon3 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Hijos___Hijas=="1")),],fTvariables =nombresR(datos,"P15_D_HijoHija"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienAutosRazon4 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "Sí")&(datos$P15_B_Otro_familiar=="1")),],fTvariables =nombresR(datos,"P15_D_OtroFamiliar"),fTlevels = F,fbanner = bandera1),
  P15NoPrestaAutosPQ = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "No")),],fTvariables =(nombresR(datos,"P15_E")[1:7]),fTlevels = F,fbanner = bandera1),  
  P15NoPrestaAutosProb = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P15_A== "No")),],fTvariables ="P15_F",fTlevels = F,fbanner = bandera1),
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
  P16Cambio = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = (nombresR(datos,"P16_A_")[1:11]),fTlevels = F,fbanner = bandera1),
  P16TransporteEficiente = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables ="P16_B",fTlevels = T,fbanner = bandera1),
  P16C1 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P16_C_Me_da_prestigio___status",fTlevels = T,fbanner = bandera1),
  P16C2 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P16_C_Me_da_tranquilidad",fTlevels = T,fbanner = bandera1),
  P16C3 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P16_C_Me_da_confianza",fTlevels = T,fbanner = bandera1),
  P16C4 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P16_C_Me_da_seguridad",fTlevels = T,fbanner = bandera1),
  P16C5 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables = "P16_C_Me_siento_contento__a",fTlevels = T,fbanner = bandera1),
  P17 = frecuentator(fTtabla = datos[datos$P10=="Automóvil",],fTvariables ="P17",fTlevels = T,fbanner = bandera1),
  P17A = frecuentator(fTtabla = datos[((datos$P10=="Automóvil")& (datos$P17== "Es usado (a)")),],fTvariables ="P17_A",fTlevels = T,fbanner = bandera1),
  P17B = frecuentator(fTtabla = datos[(datos$P10=="Automóvil"),],fTvariables =(nombresR(datos,"P17_B_")[1:6]),fTlevels = F,fbanner = bandera1),
  P17BBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P17_B_Tarjeta_de_Cr_dito=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P17_B_Cr_dito_personal_del_banco=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P17_B_Otro__Especificar=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P17_B_No_sabe__no_contest=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17B1  = frecuentator(fTtabla = datos[datos$P10=="Automóvil",], fTvariables = nombresR(datos,"P17_B1"),fTlevels = F,fbanner = bandera1),
  P18  = frecuentator(fTtabla = datos[datos$P10=="Automóvil",], fTvariables = "P18",fTlevels = T,fbanner = bandera1),
  P18C  = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si")),], fTvariables = "P18_C",fTlevels = T,fbanner = bandera1),
  P18D = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si")),],fTvariables =(nombresR(datos,"P18_D_")[1:6]),fTlevels = F,fbanner = bandera1),
  P18CBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si") & (datos$P18_D_Tarjeta_de_Cr_dito=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si") & (datos$P18_D_Cr_dito_personal_del_banco=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si") & (datos$P18_D_Otro__Especificar=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Automóvil") & (datos$P18=="Si") & (datos$P18_D_No_sabe__no_contest=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  p18E = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_E_")[1:13],fTlevels = F,fbanner = bandera1),
  p18E1 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[1:12],fTlevels = F,fbanner = bandera1),
  p18E2 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[14:25],fTlevels = F,fbanner = bandera1),
  p18E3 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[27:38],fTlevels = F,fbanner = bandera1),
  p18E4 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[40:51],fTlevels = F,fbanner = bandera1),
  p18E5 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[53:64],fTlevels = F,fbanner = bandera1),
  p18E6 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[66:77],fTlevels = F,fbanner = bandera1),
  p18E7 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[79:90],fTlevels = F,fbanner = bandera1),
  p18E8 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[92:103],fTlevels = F,fbanner = bandera1),
  p18E10 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[105:116],fTlevels = F,fbanner = bandera1),
  p18E11 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[118:129],fTlevels = F,fbanner = bandera1),
  p18E12 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[131:142],fTlevels = F,fbanner = bandera1),
  p18E13 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[144:155],fTlevels = F,fbanner = bandera1),
  p18E14 = frecuentator(fTtabla = datos[datos$P10=="Automóvil" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[157:168],fTlevels = F,fbanner = bandera1)
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
  P15PrestarQuienMotosf1 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Esposo___Esposa=="1")),],fTvariables ="P15_C_EsposaEsposo",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienMotosf2 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Pap____Mam=="1")),],fTvariables ="P15_C_PapaMama",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienMotosf3 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Hijos___Hijas=="1")),],fTvariables ="P15_C_HijoHija",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienMotosf4 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Otro_familiar=="1")),],fTvariables ="P15_C_OtroFamiliar",fTlevels = T,fbanner = bandera1),
  P15PrestarQuienMotosRazon1 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Esposo___Esposa=="1")),],fTvariables = nombresR(datos,"P15_D_EsposoEsposa"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienMotosRazon2 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Mi_Pap____Mam=="1")),],fTvariables =nombresR(datos,"P15_D_PapaMama"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienMotosRazon3 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Hijos___Hijas=="1")),],fTvariables =nombresR(datos,"P15_D_HijoHija"),fTlevels = F,fbanner = bandera1),
  P15PrestarQuienMotosRazon4 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "Sí")&(datos$P15_B_Otro_familiar=="1")),],fTvariables =nombresR(datos,"P15_D_OtroFamiliar"),fTlevels = F,fbanner = bandera1),
  P15NoPrestaMotosPQ = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")),],fTvariables =(nombresR(datos,"P15_E")[1:7]),fTlevels = F,fbanner = bandera1),  
  P15NoPrestaMotosProb = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")),],fTvariables ="P15_F",fTlevels = F,fbanner = bandera1),
  P15NoPrestaMotosProbQ = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")&(datos$P15_F== 'Sí')),],fTvariables ="P15_G",fTlevels = T,fbanner = bandera1),
  P15NoPrestarQuienMotosSol1 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")&(datos$P15_G=="Esposa/ Esposo")),],fTvariables = (nombresR(datos,"P15_H_ESPOSAESPOSO")[1:5]),fTlevels = F,fbanner = bandera1),
  P15NoPrestarQuienMotosSol2 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")&(datos$P15_G=="Mamá/ Papá")),],fTvariables = (nombresR(datos,"P15_H_PAPAMAMA")[1:5]),fTlevels = F,fbanner = bandera1),
  P15NoPrestarQuienMotosSol3 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")&(datos$P15_G=="Hijo/hija")),],fTvariables = (nombresR(datos,"P15_H_HIJOHIJA")[1:5]),fTlevels = F,fbanner = bandera1),
  P15NoPrestarQuienMotosSol4 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P15_A== "No")&(datos$P15_G=="Otro familiar")),],fTvariables = (nombresR(datos,"P15_H_OTROFAMILIAR")[1:5]),fTlevels = F,fbanner = bandera1),
  P15Actividades1 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P15_I_La_uso_para_ir_a_trabajar",fTlevels = T,fbanner = bandera1),
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
  P16Cambio = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = (nombresR(datos,"P16_A_")[1:11]),fTlevels = F,fbanner = bandera1),
  P16TransporteEficiente = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables ="P16_B",fTlevels = T,fbanner = bandera1),
  P16C1 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P16_C_Me_da_prestigio___status",fTlevels = T,fbanner = bandera1),
  P16C2 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P16_C_Me_da_tranquilidad",fTlevels = T,fbanner = bandera1),
  P16C3 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P16_C_Me_da_confianza",fTlevels = T,fbanner = bandera1),
  P16C4 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P16_C_Me_da_seguridad",fTlevels = T,fbanner = bandera1),
  P16C5 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables = "P16_C_Me_siento_contento__a",fTlevels = T,fbanner = bandera1),
  P17 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",],fTvariables ="P17",fTlevels = T,fbanner = bandera1),
  P17A = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta")& (datos$P17== "Es usado (a)")),],fTvariables ="P17_A",fTlevels = T,fbanner = bandera1),
  P17B = frecuentator(fTtabla = datos[(datos$P10=="Moto / Motocicleta"),],fTvariables =(nombresR(datos,"P17_B_")[1:6]),fTlevels = F,fbanner = bandera1),
  P17BBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P17_B_Tarjeta_de_Cr_dito=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P17_B_Cr_dito_personal_del_banco=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P17_B_Otro__Especificar=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P17_B_No_sabe__no_contest=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17B1  = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",], fTvariables = nombresR(datos,"P17_B1"),fTlevels = F,fbanner = bandera1),
  P18  = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta",], fTvariables = "P18",fTlevels = T,fbanner = bandera1),
  P18C  = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si")),], fTvariables = "P18_C",fTlevels = T,fbanner = bandera1),
  P18D = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si")),],fTvariables =(nombresR(datos,"P18_D_")[1:6]),fTlevels = F,fbanner = bandera1),
  P18CBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si") & (datos$P18_D_Tarjeta_de_Cr_dito=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si") & (datos$P18_D_Cr_dito_personal_del_banco=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si") & (datos$P18_D_Otro__Especificar=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Moto / Motocicleta") & (datos$P18=="Si") & (datos$P18_D_No_sabe__no_contest=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  p18G = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_G_")[1:10],fTlevels = F,fbanner = bandera1),
  p18F1 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_H_")[1:15],fTlevels = F,fbanner = bandera1),
  p18F2 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[17:31],fTlevels = F,fbanner = bandera1),
  p18F3 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[33:47],fTlevels = F,fbanner = bandera1),
  p18F4 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[49:63],fTlevels = F,fbanner = bandera1),
  p18F5 = frecuentator(fTtabla = datos[datos$P10="Moto / Motocicleta" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[65:79],fTlevels = F,fbanner = bandera1),
  p18F6 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[81:95],fTlevels = F,fbanner = bandera1),
  p18F7 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[97 :111],fTlevels = F,fbanner = bandera1),
  p18F8 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[113:127],fTlevels = F,fbanner = bandera1),
  p18F9 = frecuentator(fTtabla = datos[datos$P10=="Moto / Motocicleta" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[129:143],fTlevels = F,fbanner = bandera1)
  )
exportator(resultadosMotos, "./resultados/resultadosMotos.csv")


 
 resultadosAmbos <- list(
   P10 = frecuentator(fTtabla = datos,fTvariables = "P10",fTlevels = T,fbanner = bandera1),
   P15NumAmbosAuto = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_8_AUTO",fTlevels = T,fbanner = bandera1),
   P15UsoAmbosAuto = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"Rr_P15_9_AUTO"),fTlevels = F,fbanner = bandera1),
   P15MarcaAmbosAutos = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P15_10_MARCA"),fTlevels = T,fbanner = bandera1),
   P15AmbosModelo = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P10ModeloLimpia"),fTlevels = T,fbanner = bandera1),
   P15AntAmbosAuto = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P15_12"),fTlevels = T,fbanner = bandera1),
   P16PrestarAmbosAutos = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P16_A_AMBOS",fTlevels = T,fbanner = bandera1),
   P16PrestarQuienAmbosAutos = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")),],fTvariables =nombresR(datos,"P16_B_AMBOS"),fTlevels = F,fbanner = bandera1),
   P16PrestarQuienAmbosAutosf1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Mi_Esposo___Esposa=="1")),],fTvariables ="P16_C_AMBOS_ESPOSAESPOSO",fTlevels = T,fbanner = bandera1),
   P16PrestarQuienAmbosAutosf2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Mi_Pap____Mam=="1")),],fTvariables ="P16_C_AMBOS_MAMAPAPA",fTlevels = T,fbanner = bandera1),
   P16PrestarQuienAmbosAutosf3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Hijos___Hijas=="1")),],fTvariables ="P16_C_AMBOS_HIJOHIJA",fTlevels = T,fbanner = bandera1),
   P16PrestarQuienAmbosAutosf4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Otro_familiar=="1")),],fTvariables ="P16_C_AMBOS_OTROFAMILIAR",fTlevels = T,fbanner = bandera1),
  
  ####de aqui hacia arriba voy bien 
  P16AmbosPrestarQuienAutosRazon1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Mi_Esposo___Esposa=="1")),],fTvariables = nombresR(datos,"P16_D_AMBOS_ESPOSAESPOSO")[1:12],fTlevels = F,fbanner = bandera1),
  P16AmbosPrestarQuienAutosRazon2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Mi_Pap____Mam=="1")),],fTvariables =nombresR(datos,"P16_D_AMBOS_MAMAPA"),fTlevels = F,fbanner = bandera1),
  P16AmbosPrestarQuienAutosRazon3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Hijos___Hijas=="1")),],fTvariables =nombresR(datos,"P16_D_AMBOS_HIJOHIJA"),fTlevels = F,fbanner = bandera1),
  P16AmbosPrestarQuienAutosRazon4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "Sí")&(datos$P16_B_AMBOS_Otro_familiar=="1")),],fTvariables =nombresR(datos,"P16_D_AMBOS_OTROFAMILIAR"),fTlevels = F,fbanner = bandera1),
  
  
  P15NoPrestaAutosPQ = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")),],fTvariables =(nombresR(datos,"P15_E")[1:7]),fTlevels = F,fbanner = bandera1),  
  P15NoPrestaAutosProb = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")),],fTvariables ="P15_F",fTlevels = F,fbanner = bandera1),
  P15NoPrestaAutosProbQ = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")&(datos$P15_F== 'Sí')),],fTvariables ="P15_G",fTlevels = T,fbanner = bandera1),
  P15NoPrestarQuienAutosSol1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")&(datos$P15_G=="Esposa/ Esposo")),],fTvariables = (nombresR(datos,"P15_H_ESPOSAESPOSO")[1:5]),fTlevels = F,fbanner = bandera1),
  P15NoPrestarQuienAutosSol2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")&(datos$P15_G=="Mamá/ Papá")),],fTvariables = (nombresR(datos,"P15_H_PAPAMAMA")[1:5]),fTlevels = F,fbanner = bandera1),
  P15NoPrestarQuienAutosSol3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")&(datos$P15_G=="Hijo/hija")),],fTvariables = (nombresR(datos,"P15_H_HIJOHIJA")[1:5]),fTlevels = F,fbanner = bandera1),
  P15NoPrestarQuienAutosSol4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P16_A_AMBOS== "No")&(datos$P15_G=="Otro familiar")),],fTvariables = (nombresR(datos,"P15_H_OTROFAMILIAR")[1:5]),fTlevels = F,fbanner = bandera1),
  P15Actividades1 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_I_La_uso_para_ir_a_trabajar",fTlevels = T,fbanner = bandera3),
  P15Actividades2 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_I_La_uso_para_ir_al_Doctor___Den",fTlevels = T,fbanner = bandera3),
  P15Actividades3 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_I_La_uso_para_llevar_hijos_a_la_",fTlevels = T,fbanner = bandera3),
  P15Actividades4 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_I_La_uso_para_salir_de_paseo___s",fTlevels = T,fbanner = bandera3),
  P15Actividades5 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_I_La_uso_para_salir_de_vacacione",fTlevels = T,fbanner = bandera3),
  P15Actividades6 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_I_La_uso_para_llevar___transport",fTlevels = T,fbanner = bandera3),
  P15Actividades7 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_I_La_uso_para_salir_de_fiesta",fTlevels = T,fbanner = bandera3),
  P15Actividades8 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_I_La_uso_para_ir_al_s_per___merc",fTlevels = T,fbanner = bandera3),
  P15Actividades9 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_I_La_uso_para_ir_al_banco",fTlevels = T,fbanner = bandera3),
  P15Actividades10 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_I_La_uso_para_ir_a_la_Escuela___",fTlevels = T,fbanner = bandera3),
  P15Actividades11 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P15_I_La_uso_para_ir_a_visitar_a_mi_",fTlevels = T,fbanner = bandera3),
  P15InfluenciaAutos = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables =nombresR(datos,"P15_L_"),fTlevels = F,fbanner = bandera1),
  P15CompraAutos = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = nombresR(datos,"P15_M_"),fTlevels = F,fbanner = bandera1),
  P16 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables ="P16",fTlevels = T,fbanner = bandera1),
  P16Cambio = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = (nombresR(datos,"P16_A_")[1:11]),fTlevels = F,fbanner = bandera1),
  P16TransporteEficiente = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables ="P16_B",fTlevels = T,fbanner = bandera1),
  P16C1 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P16_C_Me_da_prestigio___status",fTlevels = T,fbanner = bandera1),
  P16C2 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P16_C_Me_da_tranquilidad",fTlevels = T,fbanner = bandera1),
  P16C3 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P16_C_Me_da_confianza",fTlevels = T,fbanner = bandera1),
  P16C4 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P16_C_Me_da_seguridad",fTlevels = T,fbanner = bandera1),
  P16C5 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables = "P16_C_Me_siento_contento__a",fTlevels = T,fbanner = bandera1),
  P17 = frecuentator(fTtabla = datos[datos$P10=="Ambas",],fTvariables ="P17",fTlevels = T,fbanner = bandera1),
  P17A = frecuentator(fTtabla = datos[((datos$P10=="Ambas")& (datos$P17== "Es usado (a)")),],fTvariables ="P17_A",fTlevels = T,fbanner = bandera1),
  P17B = frecuentator(fTtabla = datos[(datos$P10=="Ambas"),],fTvariables =(nombresR(datos,"P17_B_")[1:6]),fTlevels = F,fbanner = bandera1),
  P17BBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P17_B_Tarjeta_de_Cr_dito=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P17_B_Cr_dito_personal_del_banco=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P17_B_Otro__Especificar=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17BBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P17_B_No_sabe__no_contest=="1")),],fTvariables ="P17_b_bancor",fTlevels = T,fbanner = bandera1),
  P17B1  = frecuentator(fTtabla = datos[datos$P10=="Ambas",], fTvariables = nombresR(datos,"P17_B1"),fTlevels = F,fbanner = bandera1),
  P18  = frecuentator(fTtabla = datos[datos$P10=="Ambas",], fTvariables = "P18",fTlevels = T,fbanner = bandera1),
  P18C  = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P18=="Si")),], fTvariables = "P18_C",fTlevels = T,fbanner = bandera1),
  P18D = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P18=="Si")),],fTvariables =(nombresR(datos,"P18_D_")[1:6]),fTlevels = F,fbanner = bandera1),
  P18CBanco1 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P18=="Si") & (datos$P18_D_Tarjeta_de_Cr_dito=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco2 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P18=="Si") & (datos$P18_D_Cr_dito_personal_del_banco=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco3 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P18=="Si") & (datos$P18_D_Otro__Especificar=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  P18CBanco4 = frecuentator(fTtabla = datos[((datos$P10=="Ambas") & (datos$P18=="Si") & (datos$P18_D_No_sabe__no_contest=="1")),],fTvariables ="P18_d_bancor",fTlevels = T,fbanner = bandera1),
  p18E = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_E_")[1:13],fTlevels = F,fbanner = bandera1),
  p18E1 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[1:12],fTlevels = F,fbanner = bandera1),
  p18E2 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[14:25],fTlevels = F,fbanner = bandera1),
  p18E3 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[27:38],fTlevels = F,fbanner = bandera1),
  p18E4 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[40:51],fTlevels = F,fbanner = bandera1),
  p18E5 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[53:64],fTlevels = F,fbanner = bandera1),
  p18E6 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[66:77],fTlevels = F,fbanner = bandera1),
  p18E7 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[79:90],fTlevels = F,fbanner = bandera1),
  p18E8 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[92:103],fTlevels = F,fbanner = bandera1),
  p18E10 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[105:116],fTlevels = F,fbanner = bandera1),
  p18E11 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[118:129],fTlevels = F,fbanner = bandera1),
  p18E12 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[131:142],fTlevels = F,fbanner = bandera1),
  p18E13 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[144:155],fTlevels = F,fbanner = bandera1),
  p18E14 = frecuentator(fTtabla = datos[datos$P10=="Ambas" & datos$P18=="Si",], fTvariables =nombresR(datos,"P18_F_")[157:168],fTlevels = F,fbanner = bandera1)

   
   
   
   
   
    )
 
 exportator(resultadosAmbos, "./resultados/resultadosAmbos.csv")
 
   




names(datos$p17)