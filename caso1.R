#CASO 1 DEL ESTUDIO: RELACIÓN ENTRE EL SEXO Y LA APARICION DE ENFERMEDADES

#a. mujeres

library(ggplot2)
#selecciono que sean mujeres(por sexo)
tablasCaso4muj<- tablasCompletas[tablasCompletas$`Sexo`=="Mujeres" ,]

View(tablasCaso4muj)
#elimino duplicados
caso4SinDup<-distinct(tablasCaso4muj)
View(caso4SinDup)
#grafico
grafcaso4<-ggplot(data = caso4SinDup, aes(x=`Enfermos en porcentaje`, y =`Enfermedades`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafcaso4

#b.hombres

library(ggplot2)
#selecciono que sean mujeres(por sexo), por edad y por actividad
tablasCaso4hom<- tablasCompletas[tablasCompletas$`Sexo`=="Hombres" ,]

View(tablasCaso4hom)
#elimino duplicados
caso4homSinDup<-distinct(tablasCaso4hom)
View(caso4homSinDup)
#grafico
grafcaso4hom<-ggplot(data = caso4homSinDup, aes(x=`Enfermos en porcentaje`, y =`Enfermedades`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafcaso4hom







