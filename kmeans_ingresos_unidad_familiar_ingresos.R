options(repos = c(CRAN = "http://cran.rstudio.com"))
library(ggplot2)
library(plyr)

library(naniar)

str(iris)
View(iris)

library(readxl)
unidad_familiar_ingresos <- read_excel("nexter/datos/unidad_familiar_ingresos_2.xlsx")
df<-unidad_familiar_ingresos
df<-as.data.frame(df)


df<-df[c("id","dptorep","área","e01aimde","v19ede","totpers","ingrem","ipcm","pobrezai","pobnopoi")]
str(df)
######################
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  #boxplot(var_name, main="With outliers")
  #hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  #boxplot(var_name, main="Without outliers")
  #hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  #title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  #response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  #if(response == "y" | response == "yes"){
  dt[as.character(substitute(var))] <- invisible(var_name)
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  cat("Outliers successfully removed", "n")
  return(invisible(dt))
  #} else{
  #     cat("Nothing changed", "n")
  #     return(invisible(var_name))
  #}
}



#############################################################
###cambiando los factores por sus correspondientes valores#### 
#############################################################
library(plyr)
df$dptorep<-as.factor(df$dptorep)
df$dptorep<-revalue(df$dptorep, c("0" = "Asuncion","2" = "San Pedro","5" = "Caaguazu","6" = "Caazapa","7" = "Itapua","10" = "Alto Parana","11" = "Central","20" = "Resto"))


df$área<-as.factor(df$área)
df$área<-revalue(df$área, c("1" = "Urbana","6" = "Rural"))

df$pobrezai<-as.factor(df$pobrezai)
df$pobrezai<-revalue(df$pobrezai, c("1"= "Pobre extremo","2" = "Pobre no extremo","3" = "No pobre"))



df$pobnopoi<-as.factor(df$pobnopoi)
df$pobnopoi<-revalue(df$pobnopoi, c("0" = "No pobre","1" = "Pobre"))


####################################
#####graficos#########
###########################
options(scipen=10000)
library(scales)

ggplot(df, aes(e01aimde, ipcm, color = pobrezai)) + 
  geom_point() + 
  labs(title= "Titulo",x = "Ingreso por actividad principal",y="Ingreso per cápita mensual") +
  labs(color='Estatus de pobreza') +
  geom_point() + 
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

########################
######## manejo de nulos
#######################

df[df == 0] <- NA
df<-na.omit(df)

outlierKD(df, e01aimde)
df<-na.omit(df)

outlierKD(df, ipcm)
df<-na.omit(df)


############################
###k-means
#################

##saber cuantos clusters
mydata<-df[c("e01aimde","v19ede")]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
##################################

str(df)
set.seed(20)
ingresos_clusters <- kmeans(df[c("e01aimde","v19ede")], 4, nstart = 20)

ingresos_clusters$cluster <- as.factor(ingresos_clusters$cluster)
ggplot(df, aes(e01aimde, ipcm, color = ingresos_clusters$cluster)) + 
  geom_point() + 
  labs(title= "Clusters de Ingresos por Unidad Familiar",x = "Ingreso por actividad principal",y="Ingreso per cápita mensual") +
  labs(color='Clusters') +
scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

library(factoextra)
fviz_cluster(ingresos_clusters, data = df.k)
#####################3

aggregate(mydata,by=list(ingresos_clusters$cluster),FUN=mean)
aggregate(mydata,by=list(ingresos_clusters$cluster),FUN=length)

