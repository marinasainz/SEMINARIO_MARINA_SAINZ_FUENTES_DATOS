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










