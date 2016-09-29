#########Max Diff1----------------
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

























