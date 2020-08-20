
rm(list=ls())
setwd("~/Universidad/Big Data/depa 2")

library(xml2)
library(rvest)
library(ggplot2)

readHtml<-read_html("http://www.gorearaucania.cl/transparencia/2018/remuneraciones.html")
nodeTabla <- html_nodes(readHtml, "#remuneraciones")
nodeTabla<-html_nodes(nodeTabla,".tabla")
tablaLeida<-html_table(nodeTabla)
datos3<-data.frame(tablaLeida)

##Se arreglan los nombres, ya que la tabla los tiene en la fila 1 y no de header.

colnames(datos3) <- datos3[1,]
datos3 <- datos3[-1,] 
colnames(datos3) <- make.unique(names(datos3))
#se eliminan los . de la columna total bruto

for(i in 1:18){
  
  datos3[i,15]<-as.numeric(gsub("[.]","",datos3[i,15]))
}

for(i in 1:18){
  
  datos3[i,4]<-as.numeric(gsub("[.]","",datos3[i,4]))
}
print(names(datos3))
##Se revisa si hay un mayor numero de contratados de cierta profesion.
ggplot(datos3,aes(x=Estamento))+
  geom_bar()+
##En el caso del Gobierno Regional de La Araucanía si se puede apreciar una leve tendencia
## a contratar más profesionales.

write.table(datos3,file="data2.csv",sep=",",dec = " ")
