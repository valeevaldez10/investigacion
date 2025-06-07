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

edsam<- read_sav("datasets/EDSA2023_Mujer.sav")
edsav<- read_sav("datasets/EDSA2023_Vivienda.sav")
datos<- edsam %>% filter(ms01_0101a>=20)
datos1<-edsav %>% select(folio,qriqueza)
bd1 <- inner_join(datos,datos1,by="folio")

#seleccionando variables a usar
bd <- bd1 %>% select(ms01_0101a,niv_ed,aestudio,
                     qriqueza,ms01_0108,ms01_0106,ms02_0208,ms02_0238,
                     ms02_0276,ms05_0521,departamento,region,area,
                     cono_algmet,actmaconcep_cme_m,actuni_m,ms08_0809)
hist(bd$ms01_0101a)

#1,2,4,5,7 -> 1 (idioma originario en la niñez)
#3,6 -> 0 
bd <- bd %>% mutate(idioma_orig=ifelse(bd$ms01_0106 %in% c(1,2,4,5,7), 1,
                                                ifelse(bd$ms01_0106 %in% c(3,6), 0,NA)))
bd$idioma_orig<-to_factor(bd$idioma_orig)


#1 -> 1 (pertenece a un grupo indígena)
#2,3 -> 0 
bd <- bd %>% mutate(pertenece=ifelse(bd$ms01_0108 %in% c(1), 1,
                                       ifelse(bd$ms01_0108 %in% c(2,3), 0,NA)))
bd$pertenece<-to_factor(bd$pertenece)
bd<-bd %>% to_factor()
str(bd)

m6=glm(ms02_0208~ms01_0101a+niv_ed+aestudio+
       qriqueza+pertenece+ms02_0238+
       ms02_0276+ms05_0521+departamento+region+area+
       cono_algmet+actuni_m+ms08_0809, 
       data=bd, family="poisson")

summary(m6)

m6=glm(ms02_0208~ms01_0101a+pertenece+ms02_0238+area+actuni_m, 
       data=bd, family="poisson")

summary(m6)


#Modelo Logit
#TRUE=Si tiene uno o más hijos
#FALSE=Si no tiene hijos
bd <- bd %>% mutate(hijos=(ms02_0208>=1))
bd$hijos

mod1=glm(hijos~ms01_0101a+aestudio+
          pertenece+ms02_0238+
          area+
          cono_algmet+actuni_m+ms08_0809, 
       data=bd, family=binomial(link = "logit"))

summary(mod1)
#AIC=96.358 


##NOS QUEDAMOS
mod2=glm(hijos~ms01_0101a+
          ms02_0238+area+
          cono_algmet+actuni_m+ms08_0809, 
        data=bd, family=binomial(link = "logit"))

summary(mod2)
#AIC=93.95 


#Matriz de confusión modelo 1
predicciones1 = ifelse(test = mod1$fitted.values > 0.5, yes = "Si", no = "No")
observados1 = factor(mod1$model$hijos, levels = c(TRUE, FALSE), labels = c("Si", "No"))
matriz1 = table(predicciones1,observados1, dnn = c("predicciones", "observaciones"))
matriz1 <- rbind(matriz1[2, ], matriz1[1, ])
rownames(matriz1)<-c("Si","No")
matriz1
confusionMatrix(matriz1)

#Matriz de confusión modelo 2
predicciones2 = ifelse(test = mod2$fitted.values > 0.5, yes = "Si", no = "No")
observados2 = factor(mod2$model$hijos, levels = c(TRUE, FALSE), labels = c("Si", "No"))
matriz2 = table(predicciones,observados, dnn = c("predicciones", "observaciones"))
matriz2 <- rbind(matriz2[2, ], matriz2[1, ])
rownames(matriz2)<-c("Si","No")
matriz2
confusionMatrix(matriz2)


### Mosaico de la tabla de contingencia - modelo logit

#mod 1
mosaic(matriz1, shade = T, colorize = T, gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))
#mod 2
mosaic(matriz2, shade = T, colorize = T, gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))


#analisis de la colinealidad con el factor VIF
vif(mod1)
vif(mod2)

## Evaluacion del modelos--> Diferencia de residuos
# En R, un objeto glm almacena la "deviance" del modelo, así como la "deviance"
# del modelo nulo. 
dif_residuos <- mod1$null.deviance - mod1$deviance
dif_residuos2 <- mod2$null.deviance - mod2$deviance

# Grados libertad
df <- mod1$df.null - mod1$df.residual
df2 <- mod2$df.null - mod2$df.residual

# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)
p_value
paste("Diferencia de residuos:", round(dif_residuos, 4))


#pseudo R2
1- (mod1$deviance/mod1$null.deviance) #80.73
1- (mod2$deviance/mod2$null.deviance) #80.33
