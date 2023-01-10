#CASO 2: VER COMO INFLUYE EL SEXO EN LA APARICION DE ENFERMEDADES
#SEGUN LA ACTIVIDAD ECONOMICA

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









