
nombresMaxDiff<-function(x,y,z,p){
  # x<-datos
  # y<-AtributosporBoque
  # z<-nombresItems
  # p<-Caracteres antes del atributo
  for (i in 1:ncol(x)) {
    # i<-11
    names(x)[i]<- paste('b',ceiling(i/y),'v',match(str_sub(names(x)[i],p,-1), z),sep = "")
  }
  return((x))}