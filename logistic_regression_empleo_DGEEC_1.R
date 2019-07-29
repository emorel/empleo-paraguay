library(haven)
encuesta_empleo_2010_2017 <- read_sav("nexter/datos/empleo/encuesta_empleo_2010_2017.sav")
df<-encuesta_empleo_2010_2017
df<-as.data.frame(df)
#str(df)
####################################
############ tratamiento de nulos 
####################################


#library(naniar)
#vis_miss(df,warn_large_data=FALSE)
#df<-na.omit(df)
#summary(df$E01T)

## percentage of missing values

#df %>% purrr::map(~ mean(is.na(.)))
#df<-na.omit(df)

## seleccionamos las columnas con mas del 60% de datos no nulos

df[df==0]<-NA
df<-df[c("AÑO","UPM","NVIVI","NHOGA","TRIM","RONDA","LINEA","P02","P03","P04","P06","TIPOHOGA","ED01","ED02","ED03","AÑOESTU","PEAA","PEAD","ED05","E01T","RECB01","RECB02","B04","B08","B10","B11","D01","HORABH")]
df<-na.omit(df)

###################################

#############################################################
###cambiando los factores por sus correspondientes valores#### 
#############################################################
library(plyr)

#Ocupación Principal (Recodificada)
df$RECB01<-as.factor(df$RECB01)
df$RECB01<-revalue(df$RECB01, c("1" = "Miembros P.Ejec.,Leg.,Judicial y Personal Direct.","2" = "Profesionales Científicos e Intelectuales","3" = "Técnicos y Profesiónales de Nivel Medio","4" = "Empleados de Oficina","5" = "Trabaj. de Servicios y Vend. de Comercios y Merc.","6" = "Agricultores y Trabaj. Agropecuarios y Pesqueros","7" = "Oficiales, Operarios y Artesanos","8" = "Operadores de instalac.y máquinas y montadores","9"="Trabajadores no calificados","10"="Fuerzas Armadas","99"="NR"))
table(df$RECB01)

#Rama de Actividad en Ocupación Principal (Recodificada)
df$RECB02<-as.factor(df$RECB02)
df$RECB02<-revalue(df$RECB02, c("1" = "Agricultura, Ganadería, Caza y Pesca","2" = "Minas y Canteras e Industrias Manufactureras","3" = "Electricidad, Gas y Agua","4" = "Construcción","5" = "Comercio, Restaurantes y Hoteles","6" = "Transporte, Almacen. y Comunicaciones","7" = "Finanzas, Seguros, Inmuebles","8" = "Servicios Comunales, Sociales y Personales","9"="Trabajadores no calificados","99"="NR"))
table(df$RECB02)
##filtro
#df<-df[(df$RECB02=="Servicios Comunales, Sociales y Personales"),]


#Idioma
#df$ED01<-as.factor(df$ED01)
df$ED01<-as.integer(df$ED01)
#df$ED01<-revalue(df$ED01, c("1" = "Guarani","2" = "Guarani","3" = "Castellano","4" = "Castellano","5" = "Castellano","9" = "Guarani"))
#table(df$ED01)

df<-df[!(df$ED01==9),]
df<-df[!(df$ED01==5),]
df<-df[!(df$ED01==4),]

#df<-df[!(df$E01T==9999999999),]
df<-df[!(df$E01T>=100000000),]
#df<-df[!(df$E01T==150000000),]

df<-df[!(df$AÑOESTU==99),]
df<-df[!(df$AÑOESTU==99),]

### filtro 
#df<-df[(df$AÑOESTU==12),]


#Sexo
df$P04<-as.factor(df$P04)
df$P04<-revalue(df$P04, c("1" = "Varon","6" = "Mujer"))
table(df$P04)
#summary(df$E01T)
#df$P04<-as.numeric(df$P04)


options(scipen=10000)
write.table(df,"C:/Users/morel/Documents/nexter/encuesta_empleo.csv",sep = ";",row.names = FALSE)

#### seleccionamos solo columnas a utilizar
df<-df[c("AÑO","AÑOESTU","ED01","P04","RECB01","RECB02","E01T")]

##cambiamos el nombre de las variables

colnames(df)<-c("año","años_de_estudio","idioma","sexo","ocupacion_principal","rama_ocupacion_principal","total_ingreso_mensual")

df<-as.data.frame(df)

#str(df)
#summary(df)

#options(scipen=10000)
#write.table(df,"C:/Users/morel/Documents/nexter/encuesta_empleo.csv",sep = ";",row.names = FALSE)
# 
# ###outliers 
# for (i in 1:10) {
#   outlierKD(df, total_ingreso_mensual)
#   outlierKD(df, años_de_estudio)
#   #df<-na.omit(df)
# }
# 
# ### manejo de nulos generados por los outliers
# library(DMwR)
# df <- knnImputation(df,k=100)  ## SE NECESITAN 3 COLUMNAS
# anyNA(df)
# 
# options(scipen=10000)
# write.table(df,"C:/Users/morel/Documents/nexter/encuesta_empleo_outliers.csv",sep = ";",row.names = FALSE,dec = ",")
# 

## Generamos la variable a predecir
df$mayor_sueldo_minimo<-ifelse(df$total_ingreso_mensual>2192839,1,0)
table(df$mayor_sueldo_minimo)
df$mayor_sueldo_minimo <- as.factor(df$mayor_sueldo_minimo)

df$mayor_dos_sueldo_minimo<-ifelse(df$total_ingreso_mensual>2192839*2,1,0)
table(df$mayor_dos_sueldo_minimo)
df$mayor_dos_sueldo_minimo <- as.factor(df$mayor_dos_sueldo_minimo)

df$mayor_tres_sueldo_minimo<-ifelse(df$total_ingreso_mensual>2192839*3,1,0)
table(df$mayor_tres_sueldo_minimo)
df$mayor_tres_sueldo_minimo <- as.factor(df$mayor_tres_sueldo_minimo)

summary(df)


#########################



## Generamos la variable a predecir
df$categoria_sueldo_minimo<-ifelse(df$total_ingreso_mensual<=2192839*1,"<=01_sueldos_minimo",
                               ifelse(df$total_ingreso_mensual<=2192839*2,"<=02_sueldos_minimos",
                                      ifelse(df$total_ingreso_mensual<=2192839*3,"<=03_sueldos_minimos",
                                             ifelse(df$total_ingreso_mensual<=2192839*4,"<=04_sueldos_minimos",
                                                    ifelse(df$total_ingreso_mensual<=2192839*5,"<=05_sueldos_minimos",
                                                           

                               ">05_sueldos_minimos")))))
                               
table(df$categoria_sueldo_minimo)
df$categoria_sueldo_minimo <- as.factor(df$categoria_sueldo_minimo)


#options(scipen=10000)
#write.table(df,"C:/Users/morel/Documents/nexter/encuesta_empleo_categoria_sueldos_minimos.csv",sep = ";",row.names = FALSE,dec = ",")

#########################

### Correlation Histograma Matriz

#library("PerformanceAnalytics")
#my_data <- df[, c(1,2,5)]
#chart.Correlation(my_data, histogram=TRUE, pch=19)

#library(GGally)
#ggpairs(my_data, title = "Scatterplot Matrix")



################### Analisis de IDIOMA - UN SUELDO MINIMO##################################


logit <- glm(mayor_sueldo_minimo ~ idioma,data=df,family="binomial")

summary(logit)

graphics.off()
plot(df$idioma, predict(logit, type="response"), xlab="Idioma (1=Guarani,2=Guarani y Castellano,3=Castellano)",ylab="Probabilidad")

## En paraguay, hablando castellano, tenes un 35 % mas de probabilidad de ganar mas del sueldo minimo

###############################
#### Grafico de los datos #####
###############################

options(scipen=10000)
library(ggplot2)
library(scales)


ggplot(df, aes(idioma, predict(logit, type="response")*100
)) + 
  geom_line() + 
  labs(title= "Paraguay : Probabilidad de percibir salario mayor al minimo"
       ,subtitle = "(Idiomas)"
       ,caption = "Fuente: Direccion general de Estadisticas, Encuestas y Censos - Encuesta Permanente de Empleo - Paraguay 2018"
       ,x = "Idiomas (1=Guarani,2=Castellano y Guarani, 3= Castellano)"
       ,y="Probabilidad") +
  labs(color='Estatus') +
  geom_point() + 
  scale_color_brewer(palette = "Dark2")+
  scale_x_discrete(limits=c("1","2","3")) +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5)
        ,plot.subtitle = element_text(hjust = 0.5))


#########################
## UN SUELDO MINIMO ##
#########################

################### Analisis de AÑos de estudio - SUELDO MINIMO##################################


logit <- glm(mayor_sueldo_minimo ~ años_de_estudio,data=df,family="binomial")

summary(logit)

graphics.off()
plot(df$años_de_estudio, predict(logit, type="response"),xlab="Años de estudio" ,ylab="Probabilidad")

newdata = data.frame(años_de_estudio =24,mayor_dos_sueldo_minimo=0)

predict(logit, newdata, type="response")
## 

my_data <- df[, c(1,6)]
###############################
#### Grafico de los datos #####
###############################

options(scipen=10000)
library(ggplot2)
library(scales)

graphics.off()
ggplot(df, aes(años_de_estudio, predict(logit, type="response")*100
               )) + 
  geom_point() + 
  geom_line() + 
  labs(title= "Paraguay : Probabilidad de percibir salario mayor al minimo"
       ,subtitle = "(Años)"
       ,caption = "Fuente: Direccion general de Estadisticas, Encuestas y Censos - Encuesta Permanente de Empleo - Paraguay 2018"
       ,x = "Años de estudio"
       ,y="Probabilidad") +
  labs(color='Estatus') +
  geom_point() + 
  scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5)
        ,plot.subtitle = element_text(hjust = 0.5))

#########################
## DOS SUELDOS MINIMOS ##
#########################



################### Analisis de IDIOMA - SUELDO MINIMO##################################


logit <- glm(mayor_dos_sueldo_minimo ~ idioma,data=df,family="binomial")

summary(logit)

graphics.off()
plot(df$idioma, predict(logit, type="response"), xlab="Idioma (1=Guarani,2=Guarani y Castellano,3=Castellano)",ylab="Probabilidad")

## En paraguay, hablando castellano, tenes un 35 % mas de probabilidad de ganar mas del sueldo minimo

###############################
#### Grafico de los datos #####
###############################

options(scipen=10000)
library(ggplot2)
library(scales)


ggplot(df, aes(idioma, predict(logit, type="response")*100
)) + 
  geom_line() + 
  labs(title= "Paraguay : Probabilidad de percibir mas de dos(2) salarios minimos"
       ,subtitle = "(Idiomas)"
       ,caption = "Fuente: Direccion general de Estadisticas, Encuestas y Censos - Encuesta Permanente de Empleo - Paraguay 2018"
       ,x = "Idiomas (1=Guarani,2=Castellano y Guarani, 3= Castellano)"
       ,y="Probabilidad") +
  labs(color='Estatus') +
  geom_point() + 
  scale_color_brewer(palette = "Dark2")+
  scale_x_discrete(limits=c("1","2","3")) +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5)
        ,plot.subtitle = element_text(hjust = 0.5))


################### Analisis de AÑos de estudio - SUELDO MINIMO##################################


logit <- glm(mayor_dos_sueldo_minimo ~ años_de_estudio,data=df,family="binomial")

summary(logit)

graphics.off()
plot(df$años_de_estudio, predict(logit, type="response"),xlab="Años de estudio" ,ylab="Probabilidad")


newdata = data.frame(años_de_estudio =24,mayor_dos_sueldo_minimo=0)

predict(logit, newdata, type="response")

## 

###############################
#### Grafico de los datos #####
###############################

options(scipen=10000)
library(ggplot2)
library(scales)

graphics.off()
ggplot(df, aes(años_de_estudio, predict(logit, type="response")*100
)) + 
  geom_point() + 
  geom_line() + 
  labs(title= "Paraguay : Probabilidad de percibir mas de dos(2) salarios minimos"
       ,subtitle = "(Años)"
       ,caption = "Fuente: Direccion general de Estadisticas, Encuestas y Censos - Encuesta Permanente de Empleo - Paraguay 2018"
       ,x = "Años de estudio"
       ,y="Probabilidad") +
  labs(color='Estatus') +
  geom_point() + 
  scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5)
        ,plot.subtitle = element_text(hjust = 0.5))

##############

#########################
## TRES SUELDOS MINIMOS ##
#########################



################### Analisis de IDIOMA - SUELDO MINIMO##################################


logit <- glm(mayor_tres_sueldo_minimo ~ idioma,data=df,family="binomial")

summary(logit)

graphics.off()
plot(df$idioma, predict(logit, type="response"), xlab="Idioma (1=Guarani,2=Guarani y Castellano,3=Castellano)",ylab="Probabilidad")

## En paraguay, hablando castellano, tenes un 35 % mas de probabilidad de ganar mas del sueldo minimo

###############################
#### Grafico de los datos #####
###############################

options(scipen=10000)
library(ggplot2)
library(scales)


ggplot(df, aes(idioma, predict(logit, type="response")*100
)) + 
  geom_line() + 
  labs(title= "Paraguay : Probabilidad de percibir mas de tres(3) salarios minimos"
       ,subtitle = "(Idiomas)"
       ,caption = "Fuente: Direccion general de Estadisticas, Encuestas y Censos - Encuesta Permanente de Empleo - Paraguay 2018"
       ,x = "Idiomas (1=Guarani,2=Castellano y Guarani, 3= Castellano)"
       ,y="Probabilidad") +
  labs(color='Estatus') +
  geom_point() + 
  scale_color_brewer(palette = "Dark2")+
  scale_x_discrete(limits=c("1","2","3")) +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5)
        ,plot.subtitle = element_text(hjust = 0.5))


################### Analisis de AÑos de estudio - SUELDO MINIMO##################################


logit <- glm(mayor_tres_sueldo_minimo ~ años_de_estudio,data=df,family="binomial")

summary(logit)

graphics.off()
plot(df$años_de_estudio, predict(logit, type="response"),xlab="Años de estudio" ,ylab="Probabilidad")

newdata = data.frame(años_de_estudio =24,mayor_dos_sueldo_minimo=0)

predict(logit, newdata, type="response")

## 

###############################
#### Grafico de los datos #####
###############################

options(scipen=10000)
library(ggplot2)
library(scales)

graphics.off()
ggplot(df, aes(años_de_estudio, predict(logit, type="response")*100
)) + 
  geom_point() + 
  geom_line() + 
  labs(title= "Paraguay : Probabilidad de percibir mas de tres(3) salarios minimos"
       ,subtitle = "(Años)"
       ,caption = "Fuente: Direccion general de Estadisticas, Encuestas y Censos - Encuesta Permanente de Empleo - Paraguay 2018"
       ,x = "Años de estudio"
       ,y="Probabilidad") +
  labs(color='Estatus') +
  geom_point() + 
  scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5)
        ,plot.subtitle = element_text(hjust = 0.5))

##############3


############
#####SEXO###
############

################### Analisis de SEXO - Un sueldo minimo##################################


logit <- glm(mayor_sueldo_minimo ~ sexo,data=df,family="binomial")

summary(logit)

graphics.off()
plot(df$sexo, predict(logit, type="response"), xlab="Sexo (1=Hombre,6=Mujer)",ylab="Probabilidad")

## En paraguay, Los hombres tienen un 45% mas de probabilidad de ganar mas de un sueldo minimo


logit <- glm(mayor_dos_sueldo_minimo ~ sexo,data=df,family="binomial")

summary(logit)

graphics.off()
plot(df$sexo, predict(logit, type="response"), xlab="Sexo (1=Hombre,6=Mujer)",ylab="Probabilidad")

## En paraguay, Los hombres tienen un 45% mas de probabilidad de ganar mas de un sueldo minimo



logit <- glm(mayor_tres_sueldo_minimo ~ sexo,data=df,family="binomial")

summary(logit)

graphics.off()
plot(df$sexo, predict(logit, type="response"), xlab="Sexo (1=Hombre,6=Mujer)",ylab="Probabilidad")

## En paraguay, Los hombres tienen un 45% mas de probabilidad de ganar mas de un sueldo minimo




################### Analisis de IDIOMA - UN SUELDO MINIMO##################################


logit <- glm(mayor_tres_sueldo_minimo ~ idioma,data=df,family="binomial")

summary(logit)

graphics.off()
plot(df$idioma, predict(logit, type="response"), xlab="Idioma (1=Guarani,2=Guarani y Castellano,3=Castellano)",ylab="Probabilidad")


###Rattle()
Sys.setenv(LANGUAGE="en")
library(rattle)
rattle()


