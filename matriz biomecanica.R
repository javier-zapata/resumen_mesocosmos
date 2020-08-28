library(plyr)
library(tidyverse)
library(kableExtra)
library(knitr)
library(Rmisc)


##### Biomecanica Resistencia estructural ####

##Rigidez##

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)
mesocosmos <- rename(mesocosmos, Rigidez = pendiente, Extension.maxima = extensión_max, carga.max = carga_max)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Rigidez != "NA")
mesocosmos_3 <- summarize(mesocosmos_2, Rigidez. = mean(Rigidez), desv = sd(Rigidez))

## GRafico ##
mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Rigidez", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Rigidez, fill=tratamiento)) + 
  geom_bar(stat="identity", position=position_dodge())  +
  scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
  scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
  geom_errorbar(aes(ymin=Rigidez-se, ymax=Rigidez+se), width = 0.2, position =position_dodge(0.9)) + 
  labs(y="Rigidez(N/m2)") + labs(x="Localidades") + labs(fill="Tratamientos")

Rigidez = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
  theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
         axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))

Rigidez


##Extension maxima##

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)
mesocosmos <- rename(mesocosmos, Rigidez = pendiente , Extension.maxima = extensión_max , carga.max = carga_max)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Extension.maxima != "NA")
mesocosmos_3 <- summarize(mesocosmos_2, Extension.maxima. = mean(Extension.maxima), desv = sd(Extension.maxima))

## GRafico ##
mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Extension.maxima", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Extension.maxima, fill=tratamiento)) + 
  geom_bar(stat="identity", position=position_dodge())  +
  scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
  scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
  geom_errorbar(aes(ymin=Extension.maxima-se, ymax=Extension.maxima+se), width = 0.2, position =position_dodge(0.9)) + 
  labs(y="Extension maxima(mm/min)") + labs(x="Localidades") + labs(fill="Tratamientos")

Extension.maxima = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
  theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
         axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))

Extension.maxima


##carga maxima##

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)
mesocosmos <- rename(mesocosmos, Rigidez = pendiente , Extension.maxima = extensión_max , Carga.max = carga_max)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Carga.max != "NA")
mesocosmos_3 <- summarize(mesocosmos_2, Carga.max. = mean(Carga.max), desv = sd(Carga.max))

## GRafico ##
mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Carga.max", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Carga.max, fill=tratamiento)) + 
  geom_bar(stat="identity", position=position_dodge())  +
  scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
  scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
  geom_errorbar(aes(ymin=Carga.max-se, ymax=Carga.max+se), width = 0.2, position =position_dodge(0.9)) + 
  labs(y="Carga maxima(N)") + labs(x="Localidades") + labs(fill="Tratamientos")

Carga.max = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
  theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
         axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))

Carga.max

##### Biomecanica Resistencia material ####


##Modulo elastico##

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)
mesocosmos <- rename(mesocosmos, Modulo.elastico = Modulo_El�.stico , Estres.max = Estr�.s_m�.x , Presion.max = Presión_M�.x)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Modulo.elastico != "NA")
mesocosmos_3 <- summarize(mesocosmos_2, Modulo.elastico. = mean(Modulo.elastico), desv = sd(Modulo.elastico))

## GRafico ##
mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Modulo.elastico", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Modulo.elastico, fill=tratamiento)) + 
  geom_bar(stat="identity", position=position_dodge())  +
  scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
  scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
  geom_errorbar(aes(ymin=Modulo.elastico-se, ymax=Modulo.elastico+se), width = 0.2, position =position_dodge(0.9)) + 
  labs(y="Modulo elastico(mPA)") + labs(x="Localidades") + labs(fill="Tratamientos")

Modulo.elastico = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
  theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
         axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))

Modulo.elastico

##Estres maximo (Esfuerzo)##

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)
mesocosmos <- rename(mesocosmos, Modulo.elastico = Modulo_El�.stico , Estres.max = Estr�.s_m�.x , Presion.max = Presión_M�.x)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Estres.max != "NA")
mesocosmos_3 <- summarize(mesocosmos_2, Estres.max. = mean(Estres.max), desv = sd(Estres.max))

## GRafico ##
mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Estres.max", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Estres.max, fill=tratamiento)) + 
  geom_bar(stat="identity", position=position_dodge())  +
  scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
  scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
  geom_errorbar(aes(ymin=Estres.max-se, ymax=Estres.max+se), width = 0.2, position =position_dodge(0.9)) + 
  labs(y="Esfuerzo maximo(mPa)") + labs(x="Localidades") + labs(fill="Tratamientos")

Estres.max = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
  theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
         axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))

Estres.max


##Presion maxima (Deformacion)##

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)
mesocosmos <- rename(mesocosmos, Modulo.elastico = Modulo_El�.stico , Estres.max = Estr�.s_m�.x , Presion.max = Presión_M�.x)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Presion.max != "NA")
mesocosmos_3 <- summarize(mesocosmos_2, Presion.max. = mean(Presion.max), desv = sd(Presion.max))

## GRafico ##
mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Presion.max", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Presion.max, fill=tratamiento)) + 
geom_bar(stat="identity", position=position_dodge())  + 
scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
geom_errorbar(aes(ymin=Presion.max-se, ymax=Presion.max+se), width = 0.2, position =position_dodge(0.9)) + 
labs(y="Desformacion") + labs(x="Localidades") + labs(fill="Tratamientos")
       
Presion.max = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))
       
Presion.max


##### Biomecanica Nano indentacion ###

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)

mesocosmos <- rename(mesocosmos, Microdureza = Media.individuo)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Microdureza != "NA") 
mesocosmos_3 <- summarize(mesocosmos_2, Microdureza.Mean = mean(Microdureza), desv = sd(Microdureza))

## GRafico Nano indentacion ##
mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Microdureza", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Microdureza, fill=tratamiento)) + 
geom_bar(stat="identity", position=position_dodge()) + scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
geom_errorbar(aes(ymin=Microdureza-se, ymax=Microdureza+se), width = 0.2, position =position_dodge(0.9)) + 
labs(y="Microdureza (mPa)") + labs(x="Localidades") + labs(fill="Tratamientos")
       
Microdureza = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
         theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
                axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))
       
Microdureza


### Variables del mesocosmos ###

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)

mesocosmos <- rename(mesocosmos, pH.Mean.25.C = ph.Promedio..25�.C., Salinidad = Salinidad, 
Temperatura = Temperatura, Alcalinidad.total = Alcalinidad_total..µmol.kg.1., pCO2 = pCO2..µatm.,
Saturacion.Calcita = �.calcita, Saturacion.Aragonita = �.aragonita)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (pH.Mean.25.C != "NA") 
mesocosmos_3 <- summarize(mesocosmos_2, pH.Mean.25.C.Mean = mean(pH.Mean.25.C))

## Salinidad ##

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)


mesocosmos_4 <- rename(mesocosmos, pH.Mean.25.C = ph.Promedio..25�.C., Salinidad = Salinidad, 
                     Temperatura = Temperatura, Alcalinidad.total = Alcalinidad_total..µmol.kg.1., pCO2 = pCO2..µatm.,
                     Saturacion.Calcita = �.calcita, Saturacion.Aragonita = �.aragonita)

mesocosmos_5 <- unite(mesocosmos_4, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_6 <- group_by(mesocosmos_5, Localidad, tratamiento) %>% filter (Salinidad != "NA") 
mesocosmos_7 <- summarize(mesocosmos_6, Salinidad.Mean = mean(Salinidad))

ph.25_salinidad <- full_join(mesocosmos_3, mesocosmos_7)

## Temperatura ###
mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)


mesocosmos_8 <- rename(mesocosmos, pH.Mean.25.C = ph.Promedio..25�.C., Salinidad = Salinidad, 
                       Temperatura = Temperatura, Alcalinidad.total = Alcalinidad_total..µmol.kg.1., pCO2 = pCO2..µatm.,
                       Saturacion.Calcita = �.calcita, Saturacion.Aragonita = �.aragonita)

mesocosmos_9 <- unite(mesocosmos_8, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_10 <- group_by(mesocosmos_9, Localidad, tratamiento) %>% filter (Temperatura != "NA") 
mesocosmos_11 <- summarize(mesocosmos_10, Temperatura.Mean = mean(Temperatura))

ph.25_salinidad_temperatura <- full_join(ph.25_salinidad, mesocosmos_11)

## Alcalinidad ###

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)


mesocosmos_12 <- rename(mesocosmos, pH.Mean.25.C = ph.Promedio..25�.C., Salinidad = Salinidad, 
                       Temperatura = Temperatura, Alcalinidad.total = Alcalinidad_total..µmol.kg.1., pCO2 = pCO2..µatm.,
                       Saturacion.Calcita = �.calcita, Saturacion.Aragonita = �.aragonita)

mesocosmos_13 <- unite(mesocosmos_12, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_14 <- group_by(mesocosmos_13, Localidad, tratamiento) %>% filter (Alcalinidad.total != "NA") 
mesocosmos_15 <- summarize(mesocosmos_14, Alcalinidad.total.Mean = mean(Alcalinidad.total))

ph.25_salinidad_temperatura_alcalinidad <- full_join(ph.25_salinidad_temperatura, mesocosmos_15)


## pCO2 ## 

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)


mesocosmos_16 <- rename(mesocosmos, pH.Mean.25.C = ph.Promedio..25�.C., Salinidad = Salinidad, 
                        Temperatura = Temperatura, Alcalinidad.total = Alcalinidad_total..µmol.kg.1., pCO2 = pCO2..µatm.,
                        Saturacion.Calcita = �.calcita, Saturacion.Aragonita = �.aragonita)

mesocosmos_17 <- unite(mesocosmos_16, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_18 <- group_by(mesocosmos_17, Localidad, tratamiento) %>% filter (pCO2 != "NA") 
mesocosmos_19 <- summarize(mesocosmos_18, pCO2.Mean = mean(pCO2))

ph.25_salinidad_temperatura_alcalinidad_pCO2 <- full_join(ph.25_salinidad_temperatura_alcalinidad, mesocosmos_19)

## Calcita ## 

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)


mesocosmos_20 <- rename(mesocosmos, pH.Mean.25.C = ph.Promedio..25�.C., Salinidad = Salinidad, 
                        Temperatura = Temperatura, Alcalinidad.total = Alcalinidad_total..µmol.kg.1., pCO2 = pCO2..µatm.,
                        Saturacion.Calcita = �.calcita, Saturacion.Aragonita = �.aragonita)

mesocosmos_21 <- unite(mesocosmos_20, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_22 <- group_by(mesocosmos_21, Localidad, tratamiento) %>% filter (Saturacion.Calcita != "NA") 
mesocosmos_23 <- summarize(mesocosmos_22, Saturacion.Calcita.Mean = mean(Saturacion.Calcita))

ph.25_salinidad_temperatura_alcalinidad_pCO2_calcita <- full_join(ph.25_salinidad_temperatura_alcalinidad_pCO2, mesocosmos_23)

## aragonita ##

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)


mesocosmos_24 <- rename(mesocosmos, pH.Mean.25.C = ph.Promedio..25�.C., Salinidad = Salinidad, 
                        Temperatura = Temperatura, Alcalinidad.total = Alcalinidad_total..µmol.kg.1., pCO2 = pCO2..µatm.,
                        Saturacion.Calcita = �.calcita, Saturacion.Aragonita = �.aragonita)

mesocosmos_25 <- unite(mesocosmos_24, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_26 <- group_by(mesocosmos_25, Localidad, tratamiento) %>% filter (Saturacion.Aragonita != "NA") 
mesocosmos_27 <- summarize(mesocosmos_26, Saturacion.Aragonita.Mean = mean(Saturacion.Aragonita))

ph.25_salinidad_temperatura_alcalinidad_pCO2_calcita_aragonita <- full_join(ph.25_salinidad_temperatura_alcalinidad_pCO2_calcita, mesocosmos_27)


### Grafico alcalinidad ###

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)


mesocosmos <- rename(mesocosmos, pH.Mean.25.C = ph.Promedio..25�.C., Salinidad = Salinidad, 
                        Temperatura = Temperatura, Alcalinidad.total = Alcalinidad_total..µmol.kg.1., pCO2 = pCO2..µatm.,
                        Saturacion.Calcita = �.calcita, Saturacion.Aragonita = �.aragonita)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Alcalinidad.total != "NA") 
mesocosmos_3 <- summarize(mesocosmos_2, Alcalinidad.total.Mean = mean(Alcalinidad.total))

mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Alcalinidad.total", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Alcalinidad.total, fill=tratamiento)) + 
geom_bar(stat="identity", position=position_dodge())  +  scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
geom_errorbar(aes(ymin=Alcalinidad.total-se, ymax=Alcalinidad.total+se), width = 0.2, position =position_dodge(0.9)) + 
labs(y="Alcalinidad total(�mol kg-1)") + labs(x="Localidades") + labs(fill="Tratamientos")
       
Alcalinidad.total = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))
       
Alcalinidad.total


### Grafico pCO2 ###

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)
       
       
mesocosmos <- rename(mesocosmos, pH.Mean.25.C = ph.Promedio..25�.C., Salinidad = Salinidad, 
                              Temperatura = Temperatura, Alcalinidad.total = Alcalinidad_total..µmol.kg.1., pCO2 = pCO2..µatm.,
                              Saturacion.Calcita = �.calcita, Saturacion.Aragonita = �.aragonita)
       
mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (pCO2 != "NA") 
mesocosmos_3 <- summarize(mesocosmos_2, pCO2 = mean(pCO2))
       
       
mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="pCO2", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)
       
mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)
       
mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= pCO2, fill=tratamiento)) + 
geom_bar(stat="identity", position=position_dodge())  + 
scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
geom_errorbar(aes(ymin=pCO2-se, ymax=pCO2+se), width = 0.2, position =position_dodge(0.9)) + 
labs(y="Presion parcial CO2(�atm)") + labs(x="Localidades") + labs(fill="Tratamientos")
              
pCO2 = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))
              
pCO2

##  Grafico Calcita ## 

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)


mesocosmos <- rename(mesocosmos, pH.Mean.25.C = ph.Promedio..25�.C., Salinidad = Salinidad, 
                     Temperatura = Temperatura, Alcalinidad.total = Alcalinidad_total..µmol.kg.1., pCO2 = pCO2..µatm.,
                     Saturacion.Calcita = �.calcita, Saturacion.Aragonita = �.aragonita)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Saturacion.Calcita != "NA") 
mesocosmos_3 <- summarize(mesocosmos_2, pCO2 = mean(Saturacion.Calcita))


mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Saturacion.Calcita", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Saturacion.Calcita, fill=tratamiento)) + 
geom_bar(stat="identity", position=position_dodge())  + 
scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
geom_errorbar(aes(ymin=Saturacion.Calcita-se, ymax=Saturacion.Calcita+se), width = 0.2, position =position_dodge(0.9)) + 
labs(y="Saturacion.Calcita") + labs(x="Localidades") + labs(fill="Tratamientos")
       
Saturacion.Calcita = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
                axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))
       
Saturacion.Calcita


## Grafico aragonita ##

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)


mesocosmos <- rename(mesocosmos, pH.Mean.25.C = ph.Promedio..25�.C., Salinidad = Salinidad, 
                     Temperatura = Temperatura, Alcalinidad.total = Alcalinidad_total..µmol.kg.1., pCO2 = pCO2..µatm.,
                     Saturacion.Calcita = �.calcita, Saturacion.Aragonita = �.aragonita)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Saturacion.Aragonita != "NA") 
mesocosmos_3 <- summarize(mesocosmos_2, pCO2 = mean(Saturacion.Aragonita))


mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Saturacion.Aragonita", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Saturacion.Aragonita, fill=tratamiento)) + 
geom_bar(stat="identity", position=position_dodge())  + 
scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
geom_errorbar(aes(ymin=Saturacion.Aragonita-se, ymax=Saturacion.Aragonita+se), width = 0.2, position =position_dodge(0.9)) + 
labs(y="Saturacion Aragonita") + labs(x="Localidades") + labs(fill="Tratamientos")
       
Saturacion.Aragonita = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))
       
Saturacion.Aragonita


##Fsiologia##

#Consumo oxigeno#

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)

mesocosmos <- rename(mesocosmos, Consumo.oxigeno = mgO2.h.g.2, Frecuencia.cardiaca = lat.min)
                     
mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Consumo.oxigeno != "NA") 
mesocosmos_3 <- summarize(mesocosmos_2, Consumo.oxigeno. = mean(Consumo.oxigeno), desv.Consumo.O2 = sd(Consumo.oxigeno))

mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Consumo.oxigeno", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Consumo.oxigeno, fill=tratamiento)) + 
geom_bar(stat="identity", position=position_dodge())  + 
scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
geom_errorbar(aes(ymin=Consumo.oxigeno-se, ymax=Consumo.oxigeno+se), width = 0.2, position =position_dodge(0.9)) + 
labs(y="Consumo oxigeno (mgO2*h*g-1)") + labs(x="Localidades") + labs(fill="Tratamientos")
       
Consumo.oxigeno = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))
       
Consumo.oxigeno


## Frecuencia carddiaca ##

mesocosmos <- read.table("C:/Users/jazap/Desktop/Cambio climatico/Experimentos/Mesocosmos/Analisis preliminares/mesocosmos.txt",header=TRUE,sep="\t", fill = TRUE)

mesocosmos <- rename(mesocosmos, Consumo.oxigeno = mgO2.h.g.2, Frecuencia.cardiaca = lat.min)

mesocosmos_1 <- unite(mesocosmos, tratamiento,c(2:3), sep = ";", remove = T)
mesocosmos_2 <- group_by(mesocosmos_1, Localidad, tratamiento) %>% filter (Frecuencia.cardiaca != "NA") 
mesocosmos_4 <- summarize(mesocosmos_2, Frecuencia.cardiaca. = mean(Frecuencia.cardiaca), desv.Frecuencia.cardiaca = sd(Frecuencia.cardiaca))



mesocosmos_3_mesocosmos_4  <- full_join(mesocosmos_3, mesocosmos_4)


mesocosmos_4 <- summarySE(mesocosmos_2, measurevar="Frecuencia.cardiaca", groupvars = c("Localidad", "tratamiento"))
str(mesocosmos_4)

mesocosmos_4$Localidad<-factor(mesocosmos_4$Localidad,levels=c("Quintay","Chiloé","Talcaruca"),ordered=FALSE)
mesocosmos_4$tratamiento<-factor(mesocosmos_4$tratamiento,levels=c("10°C;500ppm","10°C;1500ppm","15°C;500ppm","15°C;1500ppm"),ordered=FALSE)

mesocosmos_5<-ggplot(data=mesocosmos_4, aes(x=Localidad, y= Frecuencia.cardiaca, fill=tratamiento)) + 
geom_bar(stat="identity", position=position_dodge())  + 
scale_fill_manual(values=c("grey95", "grey80","grey60","grey33")) + theme_test() + 
scale_x_discrete(labels=c("10°C;500ppm" = "10°C;500ppm" ,"10°C;1500ppm" = "10°C;1500ppm", "15°C;500ppm" = "15°C;500ppm","15°C;1500ppm" = "15°C;1500ppm")) + 
geom_errorbar(aes(ymin=Frecuencia.cardiaca-se, ymax=Frecuencia.cardiaca+se), width = 0.2, position =position_dodge(0.9)) + 
labs(y="Frecuencia cardiaca (lat/min)") + labs(x="Localidades") + labs(fill="Tratamientos")
       
Frecuencia.cardiaca = mesocosmos_5 + theme (axis.title = element_text(face="bold", colour="black", size=rel(1.0))) +
theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),
                axis.text.y = element_text(face="bold", colour="black", size=rel(1), angle=90, hjust=0.5))
       
Frecuencia.cardiaca
