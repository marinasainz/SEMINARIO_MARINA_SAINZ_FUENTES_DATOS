#caso jubilados
casoJubilados<-select( .data  =  tablasCompletas , Sexo : `Actividad económica` )

#MUJERES
#selecciono que sean mujeres(por sexo), por edad y por actividad
tablacasoJubilados<- casoJubilados[casoJubilados$`Sexo`=="Mujeres" &
                                     casoJubilados$`Edad del fumador`=="De 65 y más años"&
                                     casoJubilados$`Actividad económica`=="Jubilado/a o prejubilado/a",]
View(tablacasoJubilados)

jubilados<-distinct(tablacasoJubilados)
View(jubilados)
#PRUEBA: CASO 1
#A: estudios bajos
jubBajo<- jubilados[jubilados$`Nivel de estudios`=="Básico e inferior",]

View(jubBajo)


#****graficas
grafprr1<-ggplot(data = prr1A, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1
#B: estudios medios
jubMedioMuj<- jubilados[jubilados$`Nivel de estudios`=="Intermedio",]

View(jubMedioMuj)



grafprr1B<-ggplot(data = prr1B, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1B

#C: estudios superiores
jubSupMuj<- jubilados[jubilados$`Nivel de estudios`=="Superior",]

View(jubSupMuj)




grafprr1c<-ggplot(data = prr1c, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1c

#HOMBRESSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
tablacasoJubiladoshOMBRES<- casoJubilados[casoJubilados$`Sexo`=="Hombres" &
                                            casoJubilados$`Edad del fumador`=="De 65 y más años"&
                                            casoJubilados$`Actividad económica`=="Jubilado/a o prejubilado/a",]
View(tablacasoJubiladoshOMBRES)

jubiladosHomb<-distinct(tablacasoJubiladoshOMBRES)
View(jubiladosHomb)
#A: estudios bajos
jubBajoH<- jubiladosHomb[jubiladosHomb$`Nivel de estudios`=="Básico e inferior",]

View(jubBajoH)


#****graficas
grafprr1<-ggplot(data = prr1A, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1
#B: estudios medios
jubMedioHom<- jubiladosHomb[jubiladosHomb$`Nivel de estudios`=="Intermedio",]

View(jubMedioHom)



grafprr1B<-ggplot(data = prr1B, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1B

#C: estudios superiores
jubSupHom<- jubiladosHomb[jubiladosHomb$`Nivel de estudios`=="Superior",]

View(jubSupHom)




grafprr1c<-ggplot(data = prr1c, aes(x=`Total fumadores`, y =`Consumo de tabaco`)) + geom_point(colour = "palevioletred2") + geom_smooth(method = "lm", formula = y~poly(x,1), colour = "red", se = TRUE)
grafprr1c
