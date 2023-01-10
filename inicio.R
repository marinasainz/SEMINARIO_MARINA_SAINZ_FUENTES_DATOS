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