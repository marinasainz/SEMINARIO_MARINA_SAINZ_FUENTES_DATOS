library(readr)
library(dplyr)
library(ggplot2)


#caso jubilados
casoJubilados<-select( .data  =  tablasCompletas , Sexo : `Actividad económica` )

#MUJERES
#selecciono que sean mujeres(por sexo), por edad y por actividad
tablacasoJubilados<- casoJubilados[casoJubilados$`Sexo`=="Mujeres" &
                                     casoJubilados$`Edad del fumador`=="De 65 y más años"&
                                     casoJubilados$`Actividad económica`=="Jubilado/a o prejubilado/a",]
#View(tablacasoJubilados)

jubilados<-distinct(tablacasoJubilados)
#View(jubilados)
#PRUEBA: CASO 1
#A: estudios bajos
jubBajo<- jubilados[jubilados$`Nivel de estudios`=="Básico e inferior",]

#View(jubBajo)




#mujeres jubiladas con estudios bajos: ESTA BIEN!!!!!!!!!
GrafJubMujInf<-ggplot(data=jubBajo, aes(x=`Consumo de tabaco`, y=`Total fumadores`), fill = "RED") + 
  geom_bar(stat="identity", position="dodge")+
  labs( title = "FUMADORAS CON ESTUDIOS BAJOS",
        x = "Nivel de consumo",
        y = "Total fumadoras")+
  geom_bar(stat="identity", color = "blue",fill="steelblue") + theme_minimal()+
  theme (plot.title = element_text(size=rel(2),   color="darkslategray4",hjust = 0.5,face="bold"))

#GrafJubMujInf
#
#

#


#B: estudios medios: BIEN
jubMedioMuj<- jubilados[jubilados$`Nivel de estudios`=="Intermedio",]

#View(jubMedioMuj)

#grafica buena
GrafJubMujMed<-ggplot(data=jubMedioMuj, aes(x=`Consumo de tabaco`, y=`Total fumadores`), fill = "RED") + 
  geom_bar(stat="identity", position="dodge")+
  labs( title = "FUMADORAS CON ESTUDIOS MEDIOS",
        x = "Nivel de consumo",
        y = "Total fumadoras")+
  geom_bar(stat="identity", color = "blue",fill="steelblue") + theme_minimal()+
  theme (plot.title = element_text(size=rel(2),   color="darkslategray4",hjust = 0.5,face="bold"))

#GrafJubMujMed


#C: estudios superiores
jubSupMuj<- jubilados[jubilados$`Nivel de estudios`=="Superior",]

#View(jubSupMuj)


GrafJubMujSub<-ggplot(data=jubSupMuj, aes(x=`Consumo de tabaco`, y=`Total fumadores`), fill = "RED") + 
  geom_bar(stat="identity", position="dodge")+
  labs( title = "FUMADORAS CON ESTUDIOS ALTOS",
        x = "Nivel de consumo",
        y = "Total fumadoras")+
  geom_bar(stat="identity", color = "blue",fill="steelblue") + theme_minimal()+
  theme (plot.title = element_text(size=rel(2),   color="darkslategray4",hjust = 0.5,face="bold"))

#GrafJubMujSub


#HOMBRESSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
tablacasoJubiladoshOMBRES<- casoJubilados[casoJubilados$`Sexo`=="Hombres" &
                                            casoJubilados$`Edad del fumador`=="De 65 y más años"&
                                            casoJubilados$`Actividad económica`=="Jubilado/a o prejubilado/a",]
#View(tablacasoJubiladoshOMBRES)

jubiladosHomb<-distinct(tablacasoJubiladoshOMBRES)
#View(jubiladosHomb)

#A: estudios bajos
jubBajoH<- jubiladosHomb[jubiladosHomb$`Nivel de estudios`=="Básico e inferior",]

#View(jubBajoH)

#GRAFICA BUENA
GrafJubHominf<-ggplot(data=jubBajoH, aes(x=`Consumo de tabaco`, y=`Total fumadores`), fill = "RED") + 
  geom_bar(stat="identity", position="dodge")+
  labs( title = "FUMADORES CON ESTUDIOS BAJOS",
        x = "Nivel de consumo",
        y = "Total fumadores")+
  geom_bar(stat="identity", color = "red",fill="red") + theme_minimal()+
  theme (plot.title = element_text(size=rel(2),   color="red",hjust = 0.5,face="bold"))

#GrafJubHominf

#B: estudios medios
jubMedioHom<- jubiladosHomb[jubiladosHomb$`Nivel de estudios`=="Intermedio",]

#View(jubMedioHom)

GrafJubHomMed<-ggplot(data=jubMedioHom, aes(x=`Consumo de tabaco`, y=`Total fumadores`), fill = "RED") + 
  geom_bar(stat="identity", position="dodge")+
  labs( title = "FUMADORES CON ESTUDIOS MEDIOS",
        x = "Nivel de consumo",
        y = "Total fumadores")+
  geom_bar(stat="identity", color = "red",fill="red") + theme_minimal()+
  theme (plot.title = element_text(size=rel(2),   color="red",hjust = 0.5,face="bold"))

#GrafJubHomMed


#C: estudios superiores
jubSupHom<- jubiladosHomb[jubiladosHomb$`Nivel de estudios`=="Superior",]

#View(jubSupHom)

GrafJubHomSup<-ggplot(data=jubSupHom, aes(x=`Consumo de tabaco`, y=`Total fumadores`), fill = "RED") + 
  geom_bar(stat="identity", position="dodge")+
  labs( title = "FUMADORES CON ESTUDIOS SUPERIORES",
        x = "Nivel de consumo",
        y = "Total fumadores")+
  geom_bar(stat="identity", color = "red",fill="red") + theme_minimal()+
  theme (plot.title = element_text(size=rel(2),   color="red",hjust = 0.5,face="bold"))

#GrafJubHomSup
#

#fin jubilados