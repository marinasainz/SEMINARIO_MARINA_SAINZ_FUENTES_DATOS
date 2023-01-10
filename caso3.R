#CASO 3: RELACION ENTRE EL CONSUMO DE TABACO CON ESTUDIANTES, ASOCIADOS A
#UNA EDAD ENTRE 15 Y 24 AÑOS, RESPECTO AL SEXO

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








