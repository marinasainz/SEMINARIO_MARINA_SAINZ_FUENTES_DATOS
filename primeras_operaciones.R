#INTENTO DE SACAR CIERTOS DATOS PARA EL ESTUDIO: TABLA TABAQUISMO
#por sexo
mediaSexo  <- group_by( .data  =  FiltradoDeTablaTabaquismo , `Sexo del fumador` )
intento
View(intento)
summarise( .data  =  mediaSexo , prom  = mean( `Total fumadores` , 
                                               na.rm  =  TRUE ), 
           desv  = sd( `Total fumadores` , na.rm  =  TRUE ))
#por edad
mediaEdad  <- group_by( .data  =  FiltradoDeTablaTabaquismo , `Edad del fumador` )
intento
View(intento)
summarise( .data  =  mediaEdad , prom  = mean( `Total fumadores` , 
                                               na.rm  =  TRUE ), 
           desv  = sd( `Total fumadores` , na.rm  =  TRUE ))
#por nivel de estudios
mediaNivelEstudios  <- group_by( .data  =  FiltradoDeTablaTabaquismo , `Nivel de estudios` )
intento
mediaNivelEstudios
View(intento)
summarise( .data  =  mediaNivelEstudios , prom  = mean( `Total fumadores` , 
                                                        na.rm  =  TRUE ), 
           desv  = sd( `Total fumadores` , na.rm  =  TRUE ))

#por consumo de tabaco
mediaNivelConsumo  <- group_by( .data  =  FiltradoDeTablaTabaquismo , `Consumo de tabaco` )
intento
View(intento)
summarise( .data  =  mediaNivelConsumo , prom  = mean( `Total fumadores` , 
                                                       na.rm  =  TRUE ), 
           desv  = sd( `Total fumadores` , na.rm  =  TRUE ))

#TABLA ENFERMEDAD
#por sexo
mediaSexo  <- group_by( .data  =  FiltradoTablaEnfermedad , `Sexo` )

summarise( .data  =  mediaSexo , prom  = mean( `Total enfermos` , 
                                               na.rm  =  TRUE ), 
           desv  = sd( `Total enfermos` , na.rm  =  TRUE ))
#por actividad economica
mediaActividadEcon  <- group_by( .data  =  FiltradoTablaEnfermedad , `Actividad económica` )

summarise( .data  =  mediaActividadEcon , prom  = mean( `Total enfermos` , 
                                                        na.rm  =  TRUE ), 
           desv  = sd( `Total enfermos` , na.rm  =  TRUE ))

#por tipo de enfermedad
mediaTipoEnf  <- group_by( .data  =  FiltradoTablaEnfermedad , `Enfermedades` )

summarise( .data  =  mediaTipoEnf , prom  = mean( `Total enfermos` , 
                                                  na.rm  =  TRUE ), 
           desv  = sd( `Total enfermos` , na.rm  =  TRUE ))
View(mediaTipoEnf)


#AHORA DE LA TABLA CON LA QUE TRABAJAMOS: tablasCompletas, solo cambian un par

View(tablasCompletas)



#por actividad economica
mediaActividadEconFin  <- group_by( .data  =  tablasCompletas , `Actividad económica` )

summarise( .data  =  mediaActividadEconFin , prom  = mean( `Total enfermos` , 
                                                           na.rm  =  TRUE ), 
           desv  = sd( `Total enfermos` , na.rm  =  TRUE ))
#por tipo de enfermedad
mediaTipoEnfFin  <- group_by( .data  =  tablasCompletas , `Enfermedades` )

summarise( .data  =  mediaTipoEnfFin , prom  = mean( `Total enfermos` , 
                                                     na.rm  =  TRUE ), 
           desv  = sd( `Total enfermos` , na.rm  =  TRUE ))
