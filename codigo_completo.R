#MANIPULACION INICIAL DE LAS BASES DE DATOS


library(readr)
setwd("C:/Users/USUARIO/Documents/RSTUDIO")

enfTab <- read_delim("enfermedades_relacion.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)



tabTab <- read_delim("tabaquismo.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

tabTab
enfTab

#SEGUNDO: necesito cambiar nombres de variables para que cuando haga los joins no 
#me elimine variables por llamarse igual
library(dbplyr)
library(dplyr)
TabEnfermedad<-rename ( .data  = enfTab ,`Total enfermos` = Total, `Sexo persona enferma` = Sexo,
                        `Presencia enfermedad` = `Sí o no`)
View(TabEnfermedad)

TabTabaquismo<-rename ( .data  = tabTab ,`Total fumadores` = Total, `Sexo del fumador` = Sexo,
                        `Edad del fumador` = Edad)


#TERCERO: FILTRADO DE LA TABLA ENFERMEDAD

FiltradoTablaEnfermedad <- TabEnfermedad[(TabEnfermedad$`Actividad económica`=="Trabajando" |
                                            TabEnfermedad$`Actividad económica`=="Estudiando" |
                                            TabEnfermedad$`Actividad económica`=="Jubilado/a o prejubilado/a" |
                                            TabEnfermedad$`Actividad económica`=="Labores del hogar")&
                                           TabEnfermedad$`Presencia enfermedad`=="Sí", ]
View(FiltradoTablaEnfermedad)

#CUARTO: FILTRADO DE LA TABLA TABAQUISMO
FiltradoDeTablaTabaquismo <- TabTabaquismo[(TabTabaquismo$`Nivel de estudios`=="Superior" |
                                              TabTabaquismo$`Nivel de estudios`=="Básico e inferior" |
                                              TabTabaquismo$`Nivel de estudios`=="Intermedio" )&
                                             (TabTabaquismo$`Edad del fumador`=="De 25 a 64 años" |
                                                TabTabaquismo$`Edad del fumador`=="De 15 a 24 años" |
                                                TabTabaquismo$`Edad del fumador`=="De 65 y más años")&
                                             (TabTabaquismo$`Consumo de tabaco`=="Fumador diario" |
                                                TabTabaquismo$`Consumo de tabaco`=="Fumador ocasional" |
                                                TabTabaquismo$`Consumo de tabaco`=="Nunca ha fumado" |
                                                TabTabaquismo$`Consumo de tabaco`=="Ex fumador"), ]

View(FiltradoDeTablaTabaquismo)


#QUINTO:  CAMBIO DE TOTAL FUMADORES A DOUBLE (ERA CHAR)


int2<- gsub(",",".",FiltradoDeTablaTabaquismo$`Total fumadores`,fixed = T)
FiltradoDeTablaTabaquismo$`Total fumadores` =as.numeric(int2)


View(FiltradoDeTablaTabaquismo)
summary(FiltradoDeTablaTabaquismo)
class(FiltradoDeTablaTabaquismo$`Total fumadores`)
View(TabTabaquismo)

#SEXTO: IGUAL PERO CON TOTAL ENFERMOS
int3<- gsub(",",".",FiltradoTablaEnfermedad$`Total enfermos`,fixed = T)
FiltradoTablaEnfermedad$`Total enfermos` =as.numeric(int3)
FiltradoTablaEnfermedad

View(FiltradoTablaEnfermedad)
summary(FiltradoTablaEnfermedad)
class(FiltradoTablaEnfermedad$`Total enfermos`)
#hay veces que da error, hay que guardar de nuevo el archivo csv 



#SEPTIMO:vuelvo a cambiar el nombre de sexo para que coincidan en esa variable
#tabaquismo
FiltradoDeTablaTabaquismo<-rename ( .data  = FiltradoDeTablaTabaquismo ,
                                    `Sexo` = `Sexo del fumador`)
View(FiltradoDeTablaTabaquismo)
#enfermedad
FiltradoTablaEnfermedad<-rename ( .data  = FiltradoTablaEnfermedad ,
                                  `Sexo` = `Sexo persona enferma`)
View(FiltradoTablaEnfermedad)

dosTab<- left_join(x=FiltradoDeTablaTabaquismo, y=FiltradoTablaEnfermedad, 
                   by = "Sexo" )
View(dosTab)


#OCTAVO: VER CUANTOS EJEMPLARES DE ENFERMEDADES HAY
#filtro las enfermedades que podrían estar asociadas con el tabaquismo
#ejemplo: elimino accidentes
totalEnfermedades<-TabEnfermedad[(TabEnfermedad$`Enfermedades`=="Tensión alta" |
                                    TabEnfermedad$`Enfermedades`=="Infarto de miocardio" |
                                    TabEnfermedad$`Enfermedades`=="Angina de pecho, enfermedad coronaria" |
                                    TabEnfermedad$`Enfermedades`=="Otras enfermedades del corazón" |
                                    TabEnfermedad$`Enfermedades`=="Bronquitis crónica, enfisema, enfermedad pulmonar obstructiva crónica (EPOC)" |
                                    TabEnfermedad$`Enfermedades`=="Diabetes" |
                                    TabEnfermedad$`Enfermedades`=="Úlcera de estómago o duodeno" |
                                    TabEnfermedad$`Enfermedades`=="Incontinencia urinaria o problemas de control de la orina" |
                                    TabEnfermedad$`Enfermedades`=="Colesterol alto" |
                                    TabEnfermedad$`Enfermedades`=="Estreñimiento crónico" |
                                    TabEnfermedad$`Enfermedades`=="Cirrosis, disfunción hepática" |
                                    TabEnfermedad$`Enfermedades`=="Depresión" |
                                    TabEnfermedad$`Enfermedades`=="Ansiedad crónica" |
                                    TabEnfermedad$`Enfermedades`=="Ictus (embolia, infarto cerebral, hemorragia cerebral)" |
                                    TabEnfermedad$`Enfermedades`=="Migraña o dolor de cabeza frecuente" |
                                    TabEnfermedad$`Enfermedades`=="Tumores malignos" |
                                    TabEnfermedad$`Enfermedades`=="Problemas de tiroides" |
                                    TabEnfermedad$`Enfermedades`=="Problemas del riñón" |
                                    TabEnfermedad$`Enfermedades`=="Problemas de próstata (solo hombres)" |
                                    TabEnfermedad$`Enfermedades`=="Problemas del periodo menopáusico (solo mujeres)")&
                                   TabEnfermedad$`Actividad económica`=="TOTAL" &
                                   TabEnfermedad$`Presencia enfermedad`=="Sí",]


View(totalEnfermedades)


#NUEVE: HALLO EL NUMERO DE ENFERMEDADES

#paso a numerics otra vez
int4<- gsub(",",".",totalEnfermedades$`Total enfermos`,fixed = T)
totalEnfermedades$`Total enfermos` =as.numeric(int4)
totalEnfermedades
summm<- colSums(totalEnfermedades[5])
summm
#da 60111.9 enfermos

#DIEZ: FILTRO POR ENFERMEDADES RELEVANTES

tablasEnfRelev<-dosTab[dosTab$`Enfermedades`=="Tensión alta" |
                         dosTab$`Enfermedades`=="Infarto de miocardio" |
                         dosTab$`Enfermedades`=="Angina de pecho, enfermedad coronaria" |
                         dosTab$`Enfermedades`=="Otras enfermedades del corazón" |
                         dosTab$`Enfermedades`=="Bronquitis crónica, enfisema, enfermedad pulmonar obstructiva crónica (EPOC)" |
                         dosTab$`Enfermedades`=="Diabetes" |
                         dosTab$`Enfermedades`=="Úlcera de estómago o duodeno" |
                         dosTab$`Enfermedades`=="Incontinencia urinaria o problemas de control de la orina" |
                         dosTab$`Enfermedades`=="Colesterol alto" |
                         dosTab$`Enfermedades`=="Estreñimiento crónico" |
                         dosTab$`Enfermedades`=="Cirrosis, disfunción hepática" |
                         dosTab$`Enfermedades`=="Depresión" |
                         dosTab$`Enfermedades`=="Ansiedad crónica" |
                         dosTab$`Enfermedades`=="Ictus (embolia, infarto cerebral, hemorragia cerebral)" |
                         dosTab$`Enfermedades`=="Migraña o dolor de cabeza frecuente" |
                         dosTab$`Enfermedades`=="Tumores malignos" |
                         dosTab$`Enfermedades`=="Problemas de tiroides" |
                         dosTab$`Enfermedades`=="Problemas del riñón" |
                         dosTab$`Enfermedades`=="Problemas de próstata (solo hombres)" |
                         dosTab$`Enfermedades`=="Problemas del periodo menopáusico (solo mujeres)" ,]
porEnfermedad
View(tablasEnfRelev)
#ONCE: antes de empezar, creo una nueva variable con el porcentaje de enfemedad

tablasCompletas<- mutate(tablasEnfRelev,`Enfermos en porcentaje`=(`Total enfermos`/60111.9)*100 )

View(tablasCompletas)

#HE OBTENIDO LA TABLA tablasCompletas, QUE ES CON LA QUE TRABAJARÉ.

#CASO 1: SEXO Y ENFERMEDADES: ENFERMEDADES DE MUJERES VS HOMBRES
#a. mujeres

library(ggplot2)
#selecciono que sean mujeres(por sexo), por edad y por actividad
tablasCaso4muj<- tablasCompletas[tablasCompletas$`Sexo`=="Mujeres" ,]

#View(tablasCaso4muj)
#elimino duplicados
caso4SinDupMuj<-distinct(tablasCaso4muj)
#View(tablasCaso4muj)


#GRAFICO BIEN?SIIIIIIIIIIIIIIIIIIIIIIIIIII

library(ggplot2)
graficaMujeres<-ggplot(data = caso4SinDupMuj, aes(x =`Enfermos en porcentaje`, y = `Enfermedades`)) + 
  geom_bar(stat="identity", position="dodge", colour ="black", fill = "red")+
  labs( title = "ENFERMEDADES EN MUJERES",
        x = "Porcentaje de enfermos",
        y = "Enfermedad")+
  theme (plot.title = element_text(size=rel(1),   color="black")) 
#graficaMujeres


#b.hombres

#library(ggplot2)
#selecciono que sean mujeres(por sexo), por edad y por actividad
tablasCaso4hom<- tablasCompletas[tablasCompletas$`Sexo`=="Hombres" ,]

#View(tablasCaso4hom)
#elimino duplicados
caso4homSinDup<-distinct(tablasCaso4hom)

#View(caso4homSinDup)




library(ggplot2)
graficaHombres<-ggplot(data = caso4homSinDup, aes(x =`Enfermos en porcentaje`, y = `Enfermedades`)) + 
  geom_bar(stat="identity", position="dodge", colour ="black", fill = "red")+
  labs( title = "ENFERMEDADES EN HOMBRES",
        x = "Porcentaje de enfermos",
        y = "Enfermedad")+
  theme (plot.title = element_text(size=rel(1),   color="black")) 
#graficaHombres


#CASO 2: VER COMO INFLUYE EL SEXO EN LA APARICION DE ENFERMEDADES
#SEGUN LA ACTIVIDAD ECONOMICA

library(readr)
library(dplyr)
library(ggplot2)

#View(tablasCompletas)
#CASO 2: IMPACTO DE LA ACTIVIDAD ECONOMICA EN LA APARICION DE ENF
#MUJERES FRENTE A LAS ACTIVIDADES ECONOMICAS
tablasCaso3mujTotal<- tablasCompletas[tablasCompletas$`Sexo`=="Mujeres" ,]

#View(tablasCaso3muj)
#elimino duplicados
caso3aSinDupMujeresTotal<-distinct(tablasCaso3mujTotal)
#View(caso2SinDup)



GraficaMT <- ggplot(data = caso3aSinDupMujeresTotal, aes(x = `Enfermos en porcentaje`, y = `Enfermedades`))+
  ggtitle("Enf. MUJERES/ACTIVIDAD")+
  geom_point(aes(colour = `Actividad económica`))
#GraficaMT



#HOMBRES FRENTE A LAS ACTIVIDADES ECONOMICAS
tablasCaso3homTotal<- tablasCompletas[tablasCompletas$`Sexo`=="Hombres" ,]

#View(tablasCaso3muj)
#elimino duplicados
caso3aSinDupHombresTotal<-distinct(tablasCaso3homTotal)
#View(caso2SinDup)



GraficaHT <- ggplot(data = caso3aSinDupHombresTotal, aes(x = `Enfermos en porcentaje`, y = `Enfermedades`))+
  ggtitle("Enf. HOMBRES/ ACTIVIDAD")+
  geom_point(aes(colour = `Actividad económica`))
#GraficaHT




#otra forma de hacerlo es esta

#a. mujeres
#TRABAJANDO
library(ggplot2)
#selecciono que sean mujeres(por sexo), por edad y por actividad
tablasCaso3muj<- tablasCompletas[tablasCompletas$`Sexo`=="Mujeres" &
                                   tablasCompletas$`Actividad económica`=="Trabajando" ,]

View(tablasCaso3muj)
#elimino duplicados
caso3aSinDup<-distinct(tablasCaso3muj)
View(caso2SinDup)
#grafico
grafcaso3a<-ggplot(data = caso3aSinDup, aes(x=`Enfermos en porcentaje`, y =`Enfermedades`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafcaso3a

#LABORES DEL HOGAR
library(ggplot2)
#selecciono que sean mujeres(por sexo), por edad y por actividad
tablasCaso3mujHOGAR<- tablasCompletas[tablasCompletas$`Sexo`=="Mujeres" &
                                        tablasCompletas$`Actividad económica`=="Labores del hogar" ,]

View(tablasCaso3muj)
#elimino duplicados
caso3aSinDupHOGAR<-distinct(tablasCaso3mujHOGAR)
View(caso2SinDup)
#grafico
grafcaso3HOGAR<-ggplot(data = caso3aSinDupHOGAR, aes(x=`Enfermos en porcentaje`, y =`Enfermedades`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafcaso3HOGAR



###Jubilado/a o prejubilado/a
library(ggplot2)
#selecciono que sean mujeres(por sexo), por edad y por actividad
tablasCaso3mujJU<- tablasCompletas[tablasCompletas$`Sexo`=="Mujeres" &
                                     tablasCompletas$`Actividad económica`=="Jubilado/a o prejubilado/a" ,]

View(tablasCaso3muj)
#elimino duplicados
caso3aSinDupJU<-distinct(tablasCaso3mujJU)
View(caso2SinDup)
#grafico
grafcaso3JU<-ggplot(data = tablasCaso3mujJU, aes(x=`Enfermos en porcentaje`, y =`Enfermedades`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafcaso3JU

#ESTUDIANDO

tablasCaso3mujEST<- tablasCompletas[tablasCompletas$`Sexo`=="Mujeres" &
                                      tablasCompletas$`Actividad económica`=="Estudiando" ,]

View(tablasCaso3muj)
#elimino duplicados
caso3aSinDupEST<-distinct(tablasCaso3mujEST)
View(caso3aSinDupEST)
#grafico
grafcaso3EST<-ggplot(data = caso3aSinDupEST, aes(x=`Enfermos en porcentaje`, y =`Enfermedades`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafcaso3EST


#HAGO LO MISMO EN HOMBRES
#TRABAJANDO
tablasCaso3hom<- tablasCompletas[tablasCompletas$`Sexo`=="Hombres" &
                                   tablasCompletas$`Actividad económica`=="Trabajando" ,]


#elimino duplicados
caso3aSinDupTRAB<-distinct(tablasCaso3hom)
View(caso2SinDup)
#grafico
grafcaso3aHTRAB<-ggplot(data = caso3aSinDupTRAB, aes(x=`Enfermos en porcentaje`, y =`Enfermedades`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafcaso3aHTRAB


#LABORES DEL HOGAR hombres
library(ggplot2)
#selecciono que sean mujeres(por sexo), por edad y por actividad
tablasCaso3hojHOGAR<- tablasCompletas[tablasCompletas$`Sexo`=="Hombres" &
                                        tablasCompletas$`Actividad económica`=="Labores del hogar" ,]

View(tablasCaso3muj)
#elimino duplicados
caso3aSinDuphHOGAR<-distinct(tablasCaso3hojHOGAR)
View(caso2SinDup)

grafcaso3hoHOGAR<-ggplot(data = caso3aSinDuphHOGAR, aes(x=`Enfermos en porcentaje`, y =`Enfermedades`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafcaso3hoHOGAR


###Jubilado/a o prejubilado/a hombres
library(ggplot2)
#selecciono que sean mujeres(por sexo), por edad y por actividad
tablasCaso3hoJU<- tablasCompletas[tablasCompletas$`Sexo`=="Hombres" &
                                    tablasCompletas$`Actividad económica`=="Jubilado/a o prejubilado/a" ,]

View(tablasCaso3muj)
#elimino duplicados
caso3aSinDuphJU<-distinct(tablasCaso3hoJU)
View(caso2SinDup)
#grafico
grafcaso3hJU<-ggplot(data = caso3aSinDuphJU, aes(x=`Enfermos en porcentaje`, y =`Enfermedades`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafcaso3hJU


#ESTUDIANDO hombres

tablasCaso3hojEST<- tablasCompletas[tablasCompletas$`Sexo`=="Hombres" &
                                      tablasCompletas$`Actividad económica`=="Estudiando" ,]

View(tablasCaso3muj)
#elimino duplicados
caso3aSinDuphomEST<-distinct(tablasCaso3hojEST)
View(caso3aSinDupEST)
#grafico
grafcaso3homEST<-ggplot(data = caso3aSinDuphomEST, aes(x=`Enfermos en porcentaje`, y =`Enfermedades`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafcaso3homEST


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
#
#fin jubilados 
#
#
#CASO 3****: RELACION ENTRE EL CONSUMO DE TABACO CON ESTUDIANTES, ASOCIADOS A
#UNA EDAD ENTRE 15 Y 24 AÑOS, RESPECTO AL SEXO
#finalmente no lo inclui en la presentacion
#wait: CASO MUJERES
#CASO 1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#FILTRO A NIVEL DE NIVEL DE ESTUDIOS

#reduzco la tabla
caso1<-select( .data  =  tablasCompletas , Sexo : `Actividad económica` )

View(caso1)

#selecciono que sean mujeres(por sexo), por edad y por actividad
tablasCaso1<- caso1[caso1$`Sexo`=="Mujeres" &
                      caso1$`Edad del fumador`=="De 15 a 24 años"&
                      caso1$`Actividad económica`=="Estudiando",]

View(tablasCaso1)
#elimino duplicados
caso1Def<-distinct(tablasCaso1)
View(caso1Def)


#ahora miro el de hombres
tablasCaso1h<- caso1[caso1$`Sexo`=="Hombres" &
                       caso1$`Edad del fumador`=="De 15 a 24 años"&
                       caso1$`Actividad económica`=="Estudiando",]

View(tablasCaso1h)
#elimino duplicados
caso1Defh<-distinct(tablasCaso1h)
View(caso1Defh)


#intento hacer una grafica
library(ggplot2)
ggplot(caso1Def,aes(x=`Nivel de estudios`))+geom_bar(aes(fill=`Total fumadores`))


#grafico relacion total fumadores ccon consumo de tabaco:
graf<-ggplot(data = caso1Def, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point()
graf

graf2<-ggplot(data = caso1Def, aes(x=`Total fumadores`, y =`Nivel de estudios`)) + geom_point()
graf2
ggplot(data = mpg, aes(x = displ, y = hwy))
#
#
#
#PRUEBA: CASO 1
#A: estudios bajos
pruebaAcaso1<- caso1[caso1$`Sexo`=="Mujeres" &
                       caso1$`Edad del fumador`=="De 15 a 24 años"&
                       caso1$`Actividad económica`=="Estudiando"&
                       caso1$`Nivel de estudios`=="Básico e inferior",]

View(pruebaAcaso1)
#elimino duplicados
prr1A<-distinct(pruebaAcaso1)
View(prr1A)


grafprr1<-ggplot(data = prr1A, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1
#B: estudios medios
pruebaBcaso1<- caso1[caso1$`Sexo`=="Mujeres" &
                       caso1$`Edad del fumador`=="De 15 a 24 años"&
                       caso1$`Actividad económica`=="Estudiando"&
                       caso1$`Nivel de estudios`=="Intermedio",]

View(pruebaBcaso1)
#elimino duplicados
prr1B<-distinct(pruebaBcaso1)
View(prr1B)


grafprr1B<-ggplot(data = prr1B, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1B

#C: estudios superiores
pruebaCcaso1<- caso1[caso1$`Sexo`=="Mujeres" &
                       caso1$`Edad del fumador`=="De 15 a 24 años"&
                       caso1$`Actividad económica`=="Estudiando"&
                       caso1$`Nivel de estudios`=="Superior",]

View(pruebaCcaso1)
#elimino duplicados
prr1c<-distinct(pruebaCcaso1)
View(prr1c)


grafprr1c<-ggplot(data = prr1c, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1c


#AHORA EN HOMBRES
#A: estudios bajos
pruebaAcaso1h<- caso1[caso1$`Sexo`=="Hombres" &
                        caso1$`Edad del fumador`=="De 15 a 24 años"&
                        caso1$`Actividad económica`=="Estudiando"&
                        caso1$`Nivel de estudios`=="Básico e inferior",]

View(pruebaAcaso1)
#elimino duplicados
prr1Ah<-distinct(pruebaAcaso1h)
View(prr1Ah)


grafprr1h<-ggplot(data = prr1Ah, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1h
#B: estudios medios
pruebaBcaso1h<- caso1[caso1$`Sexo`=="Hombres" &
                        caso1$`Edad del fumador`=="De 15 a 24 años"&
                        caso1$`Actividad económica`=="Estudiando"&
                        caso1$`Nivel de estudios`=="Intermedio",]

View(pruebaBcaso1h)
#elimino duplicados
prr1Bh<-distinct(pruebaBcaso1h)
View(prr1Bh)


grafprr1Bh<-ggplot(data = prr1Bh, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1Bh

#C: estudios superiores
pruebaCcaso1h<- caso1[caso1$`Sexo`=="Hombres" &
                        caso1$`Edad del fumador`=="De 15 a 24 años"&
                        caso1$`Actividad económica`=="Estudiando"&
                        caso1$`Nivel de estudios`=="Superior",]

View(pruebaCcaso1h)
#elimino duplicados
prr1ch<-distinct(pruebaCcaso1h)
View(prr1ch)


grafprr1ch<-ggplot(data = prr1ch, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1ch



#grafico circular
pipi<- pie(prr1ch$`Total fumadores`, labels = prr1ch)
pipi1<- pie(prr1ch$`Total fumadores`)








