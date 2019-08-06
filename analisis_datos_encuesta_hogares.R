library(haven)
df <- read_sav("C:/Users/morel/Documents/nexter/201907/blog_encuesta_hogares/datos/encuesta_hogares/compilados/eph2002_2018_1.sav")

### reemplazamos los ceros por nulos 
df[df==0]<-NA
summary(df)
#df<-na.omit(df)



## porcentaje de valores nulos
library(dplyr)
library(data.table)
perc_mis<-df %>% purrr::map(~ mean(is.na(.)))
library (plyr)
perc_mis <- ldply (perc_mis, data.frame)
colnames(perc_mis) <-c("ID","PERC")

perc_mis <- perc_mis[order(perc_mis$PERC),] 

### eliminamos los que no tienen ingresos

df<-na.omit(df,ingreso_mensual_total)

anyNA(df) 

df <- as.data.frame(df)

summary(df)
options(scipen=10000)
write.table(df,"C:/Users/morel/Documents/nexter/201907/blog_encuesta_hogares/datos/encuesta_hogares/compilados/eph2002_2018_1_variables_numericas.csv",sep = ";",row.names = FALSE,dec = ",")



#############################################################
###cambiando los factores por sus correspondientes valores#### 
#############################################################
library(plyr)

#Ocupación Principal (Recodificada)
df$ocupacion_principal<-as.factor(df$ocupacion_principal)
df$ocupacion_principal<-revalue(df$ocupacion_principal, c("1" = "Miembros P.Ejec.,Leg.,Judicial y Personal Direct.","2" = "Profesionales Cientificos e Intelectuales","3" = "Tecnicos y Profesionales de Nivel Medio","4" = "Empleados de Oficina","5" = "Trabaj. de Servicios y Vend. de Comercios y Merc.","6" = "Agricultores y Trabaj. Agropecuarios y Pesqueros","7" = "Oficiales, Operarios y Artesanos","8" = "Operadores de instalac.y maquinas y montadores","9"="Trabajadores no calificados","10"="Fuerzas Armadas"))
table(df$ocupacion_principal)

#Rama de Actividad en Ocupación Principal (Recodificada)
df$rama_ocupacion<-as.factor(df$rama_ocupacion)
df$rama_ocupacion<-revalue(df$rama_ocupacion, c("1" = "Agricultura, Ganaderia, Caza y Pesca","2" = "Minas y Canteras e Industrias Manufactureras","3" = "Electricidad, Gas y Agua","4" = "Construccion","5" = "Comercio, Restaurantes y Hoteles","6" = "Transporte, Almacen. y Comunicaciones","7" = "Finanzas, Seguros, Inmuebles","8" = "Servicios Comunales, Sociales y Personales","9"="Trabajadores no calificados"))
table(df$rama_ocupacion)
##filtro
#df<-df[(df$RECB02=="Servicios Comunales, Sociales y Personales"),]


#Idioma
df$idioma<-as.factor(df$idioma)
#df$idioma<-as.integer(df$idioma)
df$idioma<-revalue(df$idioma, c("1" = "Guarani","2" = "Guarani","3" = "Castellano","4" = "Castellano","5" = "Castellano","6" = "Guarani","9" = "Guarani"))
table(df$idioma)






#Sexo
df$sexo<-as.factor(df$sexo)
df$sexo<-revalue(df$sexo, c("1" = "Varon","6" = "Mujer"))
table(df$sexo)
#summary(df$E01T)
#df$P04<-as.numeric(df$P04)

## ESTADO CIVIL
df$estado_civil <-as.factor(df$estado_civil)
df$estado_civil<-revalue(df$estado_civil, c("1" = "Casado","2" = "Unido","3" = "Separado","4" = "Viudo","5" = "Soltero","6" = "Divorciado"))
table(df$estado_civil)

## AREA
df$area <-as.factor(df$area)
df$area<-revalue(df$area, c("1" = "Urbana","6" = "Rural"))
table(df$area)

## Estatus pobreza
df$estatus_pobreza <-as.factor(df$estatus_pobreza)
df$estatus_pobreza<-revalue(df$estatus_pobreza, c("1" = "Pobre extremo","2" = "Pobre no extremo","3" = "No pobre"))
table(df$estatus_pobreza)



str(df)

options(scipen=10000)
df.sample<-subset(df,ingreso_mensual_total==sample(df$ingreso_mensual_total,size = 100000))
write.table(df.sample,"C:/Users/morel/Documents/nexter/201907/blog_encuesta_hogares/datos/encuesta_hogares/compilados/eph2002_2018_1_sample.csv",sep = ";",row.names = FALSE,dec = ",")



#################
for (i in 1:10) {
  outlierKD(df, ingreso_mensual_total)
  #outlierKD(df, años_de_estudio)
  #df<-na.omit(df)
}
df<-na.omit(df,ingreso_mensual_total)

#################################################

## df$ingreso_mensual_total en funcion de df$anho;   Y + f(X)
#df$ingreso_mensual_total = f(df$anho)

library(ggplot2)
qplot(df$anho,df$ingreso_mensual_total, geom='smooth', span =0.5)

plot(df$anho,df$ingreso_mensual_total)


library(ggplot2)
ggplot(df, aes(anho, ingreso_mensual_total)) + geom_point() +
  geom_smooth(method = "gam", formula = y ~ poly(x, 2)) 

ggplot(df, aes(anho, ingreso_mensual_total)) + geom_point() +
  geom_smooth(method = "loess", span = 0.3, se = FALSE) 

#################################################

## df$ingreso_mensual_total en funcion de df$anho;   Y + f(X)
#df$ingreso_mensual_total = f(df$anho)

library(ggplot2)
qplot(df$anho,df$ingreso_mensual_total, geom='smooth', span =0.5)

plot(df$edad,df$ingreso_mensual_total)


library(ggplot2)


ggplot(df, aes(edad, ingreso_mensual_total)) + geom_point() +
  geom_smooth(method = "loess", span = 0.3, se = FALSE) 

#################################################

## df$ingreso_mensual_total en funcion de df$anho;   Y + f(X)
#df$ingreso_mensual_total = f(df$anho)

library(ggplot2)
qplot(df$anho_estudio,df$ingreso_mensual_total, geom='smooth', span =0.5)

plot(df$anho_estudio,df$ingreso_mensual_total)


library(ggplot2)


ggplot(df, aes(anho_estudio, ingreso_mensual_total)) + geom_point() +
  geom_smooth(method = "loess", span = 0.3, se = FALSE) 

#################################################

## df$ingreso_mensual_total en funcion de df$anho;   Y + f(X)
#df$ingreso_mensual_total = f(df$anho)

library(ggplot2)
qplot(df$idioma,df$ingreso_mensual_total, geom='smooth', span =0.5)

plot(df$idioma,df$ingreso_mensual_total)


library(ggplot2)


ggplot(df, aes(idioma, ingreso_mensual_total)) + geom_point() +
  geom_smooth(method = "loess", span = 0.3, se = FALSE) 
