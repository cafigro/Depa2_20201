
library(ggplot2)
library(xml2)
library(rvest)
rm(list=ls())
setwd("~/Universidad/Big Data/depa 2")

##Se lee la info de ambas paginas
readHtml<-read_html("https://www.portaltransparencia.cl/PortalPdT/pdtta/-/ta/AV001/PR/PCONT/15762429")
readHtml2<-read_html("https://www.portaltransparencia.cl/PortalPdT/pdtta/-/ta/AR004/PR/PCONT/52470664")
tablaLeida<-data.frame(html_table(readHtml))
tablaLeida2<-data.frame(html_table(readHtml2))

##Se elimina la columna que no coincide en ambas.
tablaLeida2<-tablaLeida2[,-12]

##Se mezclan ambas tablas
datos<-rbind(tablaLeida,tablaLeida2)

#Variables auxiliares para calcular promedio
pro<-0
cPro<-0
adm<-0
cAdm<-0
tec<-0
cTec<-0
#Limpiando puntos de la Remuneracion bruta.
  for(i in 1:200){
    
    datos[i,11]<-as.numeric(gsub("[.]","",datos[i,11]))
  }

#Se transforma a numeros la remuneracion bruta.
datos$Remuneración.bruta.mensualizada<-as.numeric(datos$Remuneración.bruta.mensualizada)
for(i in 1:200){
  
  if(datos[i,3]=="Profesional"){
  pro<-pro+datos[i,11]
  cPro<-cPro+1
  
  }else if(datos[i,3]=="Administrativo"){
    adm<-adm+datos[i,11]
    cAdm<-cAdm+1
  }else if(datos[i,3]=="Técnico"){
    tec<-tec+datos[i,11]
    cTec<-cTec+1
  }
  
}
adm<-adm/cAdm
pro<-pro/cPro
tec<-tec/cTec

##Se revisa si existe alguna tendencia en la de un estamento especifico.
ggplot(datos,aes(x=Estamento))+
  geom_bar(fill = "steelblue", color ="steelblue")+
  labs(title="Numero de contratados por estamento",y="Cantidad")

#Se revisa de que region hay mas contratados  
  ggplot(datos,aes(x=Región))+
  geom_bar(fill = "steelblue", color ="steelblue")+ 
  coord_flip()

##Se revisa como esta compuesta la distribucion de sueldos.
ggplot(datos,aes(sample=Remuneración.bruta.mensualizada,data=Estamento,color=Estamento))+
  stat_qq()+
labs(title="Distribucion de sueldos por estamento",
     y = "Sueldos")
table(datos$Región)

write.table(datos,file="Carlos_Figueroa.csv",sep=",",dec = " ")

print(paste("Hay",cPro,"profesionales contratados que ganan en promedio",round(pro,0),",",cAdm,"Administrativos que ganan en promedio",round(adm,0),"y",cTec,"Tecnicos que ganan en promedio",round(tec,0)))

##Con la informacion expuesta, ya sabemos que estamento mas popular es profesional y son los que mas ganan
## tambien que la mayoria de las personas contratadas son de la region metropolitana.
