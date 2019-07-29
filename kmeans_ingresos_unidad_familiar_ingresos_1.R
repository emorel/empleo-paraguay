

######################################
####Cargamos en memoria el archivo####
######################################
##https://www.dgeec.gov.py/datos/encuestas/eph/IngFliar/EPH-2018/data/918a1ingrefam_ephc_2018.csv

library(readr)
df <- read_delim("https://www.dgeec.gov.py/datos/encuestas/eph/IngFliar/EPH-2018/data/918a1ingrefam_ephc_2018.csv", 
";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
grouping_mark = "."), trim_ws = TRUE)

df<-as.data.frame(df)
str(df)

df<-df[c("e01aimde","ipcm","pobrezai")]
colnames(df)<-c("ingreso_principal","ingreso_per_capita","estatus_pobreza")

str(df)

summary(df)



#############################################################
###cambiando los factores por sus correspondientes valores#### 
#############################################################
library(plyr)

df$estatus_pobreza<-as.factor(df$estatus_pobreza)
df$estatus_pobreza<-revalue(df$estatus_pobreza, c("1"= "Pobre extremo","2" = "Pobre no extremo","3" = "No pobre"))


#########################################
########Manejo de Nulos y Outliers######
########################################
##Como K-means no maneja los Nulos y los Outliers, 
##se eliminan a efectos practicos
df[df == 0] <- NA
for (i in 1:10) {
  outlierKD(df, ingreso_principal)
  outlierKD(df, ingreso_per_capita)
  df<-na.omit(df)
}


############################
library(feather)
write_feather(df, "C:/Users/morel/Documents/nexter/datos/df.feather")


##################################
####Transformacion de los datos###
##################################


df.k<-scale(df[c("ingreso_principal","ingreso_per_capita")])



###############################
#### Grafico de los datos #####
###############################

options(scipen=10000)
library(ggplot2)
library(scales)


ggplot(df, aes(ingreso_principal/6000, ingreso_per_capita/6000
               , color = estatus_pobreza)) + 
  geom_point() + 
  labs(title= "Paraguay : Ingreso por Unidad Familiar"
       ,subtitle = "(USD)"
       ,caption = "Fuente: Direccion general de Estadisticas, Encuestas y Censos - Encuesta Permanente de Hogares - Paraguay 2018"
       ,x = "Ingreso por actividad principal"
       ,y="Ingreso per cápita mensual") +
  labs(color='Estatus') +
  geom_point() + 
  scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5)
        ,plot.subtitle = element_text(hjust = 0.5))




##################################
### Implementacion de K-Means ####
##################################
  

ingresos_clusters <- kmeans(df.k, 3)


################################
### Grafico de los clusters #### 
################################

ggplot(df, aes(ingreso_principal/6000, ingreso_per_capita/6000, color = as.factor(ingresos_clusters$cluster))) + 
  geom_point() + 
  labs(title= "Clusters de Ingreso por Unidad Familiar"
       ,subtitle = "(USD)"
       ,caption = "Fuente: Direccion general de Estadisticas, Encuestas y Censos - Encuesta Permanente de Hogares - Paraguay 2018"
       ,x = "Ingreso por actividad principal"
       ,y="Ingreso per cápita mensual") +
  labs(color='Clusters') +
  scale_color_brewer(palette = "Dark2")+
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5)
        ,plot.subtitle = element_text(hjust = 0.5))


