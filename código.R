rm(list=ls())
library(dplyr)
library(labelled)
library(haven)
library(ggplot2)
library(GGally)
library(car)
library(margins)
library(pscl)
library(mixlm)
library(fastDummies)
library(nortest)
library(lmtest)
library(caret)
library(vcd)

#1. Base datos a utilizar
edsam<- read_sav("datasets/EDSA2023_Mujer.sav")
edsav<- read_sav("datasets/EDSA2023_Vivienda.sav")

#se seleccionan solo a las mujeres adultas (mayores de 19 años)
datos<- edsam %>% filter(ms01_0101a>=20)
datos1<-edsav %>% select(folio,qriqueza)
bd1 <- inner_join(datos,datos1,by="folio")


#2.Seleccionando las variables a usar
bd <- bd1 %>% select(ms02_0208,qriqueza,ms08_0809,niv_ed_g,aestudio,
                     ms02_0276,cono_algmet,actmaconcep_cme_m,ms01_0108,
                     ms01_0106,departamento,region,area,ms01_0101a,
                     actuni_m)

#las edades se encuentran entre 20 y 49 años
hist(bd$ms01_0101a)
table(bd$ms01_0101a)


#3. Transformación sobre las variables
#Transformación de la variable idioma originario (idioma_orig)
#1,2,4,5,7 -> 1 (idioma originario en la niñez)
#3,6 -> 0 
bd <- bd %>% mutate(idioma_orig=ifelse(bd$ms01_0106 %in% c(1,2,4,5,7), 1,
                                                ifelse(bd$ms01_0106 %in% c(3,6), 0,NA)))
bd$idioma_orig<-to_factor(bd$idioma_orig)


#Transformación de la variable pertenencia a un grupo indígena (pertenece)
#1 -> 1 (pertenece a un grupo indígena)
#2,3 -> 0 
bd <- bd %>% mutate(pertenece=ifelse(bd$ms01_0108 %in% c(1), 1,
                                       ifelse(bd$ms01_0108 %in% c(2,3), 0,NA)))
bd$pertenece<-to_factor(bd$pertenece)
bd<-bd %>% to_factor()
str(bd)


#4. Modelo Poisson

#modelo inicial
m=glm(ms02_0208~qriqueza+ms08_0809+niv_ed_g+aestudio+
        ms02_0276+cono_algmet+actmaconcep_cme_m+pertenece+
        idioma_orig+departamento+region+area+ms01_0101a+actuni_m, 
       data=bd, family="poisson")
summary(m)

#modelo con el método backward
m1<-step(m,direction = "backward")
summary(m1)
#AIC: 32486

#modelo con las variables más significativas
m2=glm(ms02_0208~ms01_0101a+pertenece+area+actuni_m, 
       data=bd, family="poisson")
summary(m2)


#5. Modelo Logit determinantes de la fecundidad

#Transformación variable dependiente (dicotómica en base a ms02_0208)
#TRUE=Si tiene uno o más hijos
#FALSE=Si no tiene hijos

bd <- bd %>% mutate(hijos=(ms02_0208>=1))
bd$hijos

#modelo inicial
m3=glm(hijos~qriqueza+ms08_0809+niv_ed_g+aestudio+
        ms02_0276+cono_algmet+actmaconcep_cme_m+pertenece+
        idioma_orig+departamento+region+area+ms01_0101a+actuni_m, 
      data=bd, family=binomial(link = "logit"))
summary(m3)

#modelo con el método backward
m4<-step(m3,direction = "backward")
summary(m4)
#AIC=6279.2


#modelo con variables más significativas
m5=glm(hijos~ms01_0101a+aestudio+pertenece+area+cono_algmet+actuni_m, 
       data=bd, family=binomial(link = "logit"))
summary(m5)


##Matriz de confusión modelo m4
predicciones1 = ifelse(test = m4$fitted.values > 0.5, yes = "Si", no = "No")
observados1 = factor(m4$model$hijos, levels = c(TRUE, FALSE), labels = c("Si", "No"))
matriz1 = table(predicciones1,observados1, dnn = c("predicciones", "observaciones"))
matriz1 <- rbind(matriz1[2, ], matriz1[1, ])
rownames(matriz1)<-c("Si","No")
confusionMatrix(matriz1)
#Hay un accuracy de 88.38%



### Mosaico de la tabla de contingencia - modelo m4
mosaic(matriz1,shade = T,colorize = T,
       gp = gpar(fill = matrix(c("#93B0AC", "#EFD1D4", "#EFD1D4", "#93B0AC"), 2, 2)),
       labeling_args = list(set_varnames = c(A = "Predicciones", B = "Observaciones")),
       ylab = "Predicciones",xlab = "Observaciones")


#analisis de la colinealidad con el factor VIF
vif(m4)
#se observa que no existe colinealidad relevante


#6. Modelo logit para la alta fecundidad
#Transformación variable dependiente (dicotómica en base a ms02_0208)
#TRUE=Si tiene cuatro o más hijos
#FALSE=Si tiene menos de cuatro hijos

bd <- bd %>% mutate(a_hijos=(ms02_0208>=4))
bd$a_hijos

#modelo inicial
m6=glm(a_hijos~qriqueza+ms08_0809+niv_ed_g+aestudio+
         ms02_0276+cono_algmet+actmaconcep_cme_m+pertenece+
         idioma_orig+departamento+region+area+ms01_0101a+actuni_m, 
       data=bd, family=binomial(link = "logit"))
summary(m6)

#modelo con el método backward
m7<-step(m6,direction = "backward")
summary(m7)
#AIC=6994.5


#modelo con variables más significativas
m8=glm(a_hijos~qriqueza+ms08_0809+niv_ed_g+aestudio+
         actmaconcep_cme_m+area+ms01_0101a+actuni_m, 
       data=bd, family=binomial(link = "logit"))
summary(m8)


#Matriz de confusión modelo m7
predicciones2 = ifelse(test = m7$fitted.values > 0.5, yes = "Si", no = "No")
observados2 = factor(m7$model$a_hijos, levels = c(TRUE, FALSE), labels = c("Si", "No"))
matriz2 = table(predicciones2,observados2, dnn = c("predicciones", "observaciones"))
matriz2 <- rbind(matriz2[2, ], matriz2[1, ])
rownames(matriz2)<-c("Si","No")
matriz2
confusionMatrix(matriz2)
#Hay un accuracy de 84.91%


### Mosaico de la tabla de contingencia - modelo m7
mosaic(matriz2,shade = T,colorize = T,
       gp = gpar(fill = matrix(c("#93B0AC", "#EFD1D4", "#EFD1D4", "#93B0AC"), 2, 2)),
       labeling_args = list(set_varnames = c(A = "Predicciones", B = "Observaciones")),
       ylab = "Predicciones",xlab = "Observaciones")

#analisis de la colinealidad con el factor VIF
vif(m7)
#se observa que no existe colinealidad relevante



#8. Evaluacion del modelos -> modelo logit y modelo logit (alta fecundidad)
# En R, un objeto glm almacena la "deviance" del modelo, así como la "deviance"
# del modelo nulo. 


#pseudo R2
1- (m1$deviance/m1$null.deviance) #Poisson: 48.88
1- (m4$deviance/m4$null.deviance) #Logit: 47.37
1- (m7$deviance/m7$null.deviance) #Logit alta fecundidad: 31.81


#devianza
m1$deviance #Poisson: 9262.23
m4$deviance #Logit: 6229.16
m7$deviance #Logit alta fecundidad: 6946.55
