library(data.table)
library(tidyverse)
library(MASS)
library(ggplot2)

carpeta <- "C:/Users/Usuario/Desktop/Doctorado SP/Segundo semestre 2021/Investigación CMM-ESP/Serie A/"

serieA2017 <- fread(paste0(carpeta, "SerieA2017.txt"), sep = ";", dec = ",")
setDT(serieA2017)
prenatal2017 <- serieA2017[CodigoPrestacion == "01010201" | CodigoPrestacion == "01010203"]
prenatal2017 <- prenatal2017[complete.cases(prenatal2017[ , c('Col01')]), ] 
rm(serieA2017)

serieA2018 <- fread(paste0(carpeta, "SerieA2018.txt"), sep = ";", dec = ",")
setDT(serieA2018)
prenatal2018 <- serieA2018[CodigoPrestacion == "01010201" | CodigoPrestacion == "01010203"]
prenatal2018 <- prenatal2018[complete.cases(prenatal2018[ , c('Col01')]), ] 
rm(serieA2018)

serieA2019 <- fread(paste0(carpeta, "SerieA2019.txt"), sep = ";", dec = ",")
setDT(serieA2019)
prenatal2019 <- serieA2019[CodigoPrestacion == "01010201" | CodigoPrestacion == "01010203"]
prenatal2019 <- prenatal2019[complete.cases(prenatal2019[ , c('Col01')]), ] 
rm(serieA2019)

serieA2020 <- fread(paste0(carpeta, "SerieA2020.txt"), sep = ";", dec = ",")
setDT(serieA2020)
prenatal2020 <- serieA2020[CodigoPrestacion == "01010201" | CodigoPrestacion == "01010203"]
prenatal2020 <- prenatal2020[complete.cases(prenatal2020[ , c('Col01')]), ] 
rm(serieA2020)

prenatal <- rbind(prenatal2017, prenatal2018, prenatal2019, prenatal2020)

prenatal.ag <- prenatal %>% group_by(Ano, Mes) %>% summarise(conteo = sum(Col01))

prenatal.ag$t <- c(1:48)
prenatal.ag$level <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1)
prenatal.ag$slope <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9)
prenatal.ag$Ano <- as.factor(prenatal.ag$Ano)
prenatal.ag$Mes <- as.factor(prenatal.ag$Mes)

modelo_prenatal <- glm.nb(conteo ~ level + t + level:slope + Ano + Mes, data = prenatal.ag)

exp(modelo_prenatal[["coefficients"]][["level"]])
exp(modelo_prenatal[["coefficients"]][["level:slope"]])
round(exp(confint(modelo_prenatal)), digits = 3)

predichos.ag <- predict.glm(modelo_prenatal, type = "response", se.fit = TRUE)
prenatal.ag0 <- prenatal.ag %>% mutate(level = 0, slope = 0)
predichos.ag0 <- predict.glm(modelo_prenatal, newdata = prenatal.ag0, type = "response", se.fit = TRUE)

prenatal.ag <- cbind(prenatal.ag, predichos.ag)
prenatal.ag0 <- cbind(prenatal.ag0, predichos.ag0)
prenatal.ag$id <- "observado"
prenatal.ag0$id <- "contrafactual"

prenatal.contra <- rbind(prenatal.ag, prenatal.ag0)

prenatal.contra$residual.scale <- NULL
prenatal.contra$pred_lb <- prenatal.contra$fit - 1.96*prenatal.contra$se.fit
prenatal.contra$pred_ub <- prenatal.contra$fit + 1.96*prenatal.contra$se.fit

ggplot(data = prenatal.contra, aes(x = t, y = conteo, group = id, color = id)) +
  geom_point() +
  geom_line(aes(x = t, y = fit, group = id, color = id), linetype = "dotted") +
  geom_vline(xintercept = 39, colour = "red") +
  ylab ("N° de controles prenatales") +
  xlab ("Meses") +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(labels = scales::comma) +
  theme(text=element_text(size=30)) +
  theme(axis.text=element_text(size=30)) +
  theme(axis.title=element_text(size=30)) +
  geom_ribbon( aes(ymin = pred_lb, ymax = pred_ub, fill = id, color = NULL), alpha = .15) +
  theme_minimal()

prenatal.contra.sum <- prenatal.contra %>% group_by(id) %>% summarise(fit = sum(fit), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub))

# Controles prenatales no realizados = 195.015 (IC95% 159.583 -230.445) #

# Control niño sano (0 a 5 años) #

carpeta <- "C:/Users/Usuario/Desktop/Doctorado SP/Segundo semestre 2021/Investigación CMM-ESP/Serie A/"

serieA2017 <- fread(paste0(carpeta, "SerieA2017.txt"), sep = ";", dec = ",")
setDT(serieA2017)
controlninosano2017 <- serieA2017[CodigoPrestacion == "02010101" | CodigoPrestacion == "02010201" | CodigoPrestacion == "02010103"]
rm(serieA2017)

serieA2018 <- fread(paste0(carpeta, "SerieA2018.txt"), sep = ";", dec = ",")
setDT(serieA2018)
controlninosano2018 <- serieA2018[CodigoPrestacion == "02010101" | CodigoPrestacion == "02010201" | CodigoPrestacion == "02010103"]
rm(serieA2018)

serieA2019 <- fread(paste0(carpeta, "SerieA2019.txt"), sep = ";", dec = ",")
setDT(serieA2019)
controlninosano2019 <- serieA2019[CodigoPrestacion == "02010101" | CodigoPrestacion == "02010201" | CodigoPrestacion == "02010103"]
rm(serieA2019)

serieA2020 <- fread(paste0(carpeta, "SerieA2020.txt"), sep = ";", dec = ",")
setDT(serieA2020)
controlninosano2020 <- serieA2020[CodigoPrestacion == "02010101" | CodigoPrestacion == "02010201" | CodigoPrestacion == "02010103"]
rm(serieA2020)

controlninosano <- rbind(controlninosano2017, controlninosano2018, controlninosano2019, controlninosano2020)

controlninosano <- controlninosano %>% mutate(conteo = Col04 + Col05 + Col06 + Col07 + Col08 + Col09 + Col10 + Col11 + Col12 + Col13 + Col14 + Col15 + Col16)
controlninosano <- controlninosano[complete.cases(controlninosano[ , c('conteo')]), ] 
controlninosano.ag <- controlninosano %>% group_by(Ano, Mes) %>% summarise(conteo = sum(conteo))

controlninosano.ag$t <- c(1:48)
controlninosano.ag$level <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1)
controlninosano.ag$slope <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9)
controlninosano.ag$Ano <- as.factor(controlninosano.ag$Ano)
controlninosano.ag$Mes <- as.factor(controlninosano.ag$Mes)

modelo_controlninosano <- glm.nb(conteo ~ level + t + level:slope + Ano + Mes, data = controlninosano.ag)

exp(modelo_controlninosano[["coefficients"]][["level"]])
exp(modelo_controlninosano[["coefficients"]][["level:slope"]])
round(exp(confint(modelo_controlninosano)), digits = 3)

predichos.ag <- as.data.frame(predict.glm(modelo_controlninosano, type = "response", se.fit = TRUE))
controlninosano.ag0 <- controlninosano.ag %>% mutate(level = 0, slope = 0)
predichos.ag0 <- as.data.frame(predict.glm(modelo_controlninosano, newdata = controlninosano.ag0, type = "response", se.fit = TRUE))

controlninosano.ag <- cbind(controlninosano.ag, predichos.ag)
controlninosano.ag0 <- cbind(controlninosano.ag0, predichos.ag0)
controlninosano.ag$id <- "observado"
controlninosano.ag0$id <- "contrafactual"

controlninosano.contra <- rbind(controlninosano.ag, controlninosano.ag0)

controlninosano.contra$residual.scale <- NULL
controlninosano.contra$pred_lb <- controlninosano.contra$fit - 1.96*controlninosano.contra$se.fit
controlninosano.contra$pred_ub <- controlninosano.contra$fit + 1.96*controlninosano.contra$se.fit

ggplot(data = controlninosano.contra, aes(x = t, y = conteo, group = id, color = id)) +
  geom_point() +
  geom_line(aes(x = t, y = fit, group = id, color = id), linetype = "dotted") +
  geom_vline(xintercept = 39, colour = "red") +
  ylab ("N° de controles niño sano") +
  xlab ("Meses") +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(labels = scales::comma) +
  theme(text=element_text(size=30)) +
  theme(axis.text=element_text(size=30)) +
  theme(axis.title=element_text(size=30)) +
  geom_ribbon( aes(ymin = pred_lb, ymax = pred_ub, fill = id, color = NULL), alpha = .15) +
  theme_minimal()

controlninosano.contra.sum <- controlninosano.contra %>% group_by(id) %>% summarise(fit = sum(fit), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub))

# Controles niños sano (0 a 5 años) no realizados = 400.228 (IC95% 225.680 - 574.777) #

# Embarazadas en control #

carpeta <- "C:/Users/Usuario/Desktop/Doctorado SP/Segundo semestre 2021/Investigación CMM-ESP/Serie A/"

serieA2017 <- fread(paste0(carpeta, "SerieA2017.txt"), sep = ";", dec = ",")
setDT(serieA2017)
embarazadas2017 <- serieA2017[CodigoPrestacion == "01080008"]
rm(serieA2017)

serieA2018 <- fread(paste0(carpeta, "SerieA2018.txt"), sep = ";", dec = ",")
setDT(serieA2018)
embarazadas2018 <- serieA2018[CodigoPrestacion == "01080008"]
rm(serieA2018)

serieA2019 <- fread(paste0(carpeta, "SerieA2019.txt"), sep = ";", dec = ",")
setDT(serieA2019)
embarazadas2019 <- serieA2019[CodigoPrestacion == "01080008"]
rm(serieA2019)

serieA2020 <- fread(paste0(carpeta, "SerieA2020.txt"), sep = ";", dec = ",")
setDT(serieA2020)
embarazadas2020 <- serieA2017[CodigoPrestacion == "01080008"]
rm(serieA2020)

embarazadas <- rbind(embarazadas2017, embarazadas2018, embarazadas2019, embarazadas2020)

embarazadas.ag <- embarazadas %>% group_by(Ano, Mes) %>% summarise(conteo = sum(Col01))

embarazadas.ag$t <- c(1:48)
embarazadas.ag$level <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1)
embarazadas.ag$slope <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9)
embarazadas.ag$Ano <- as.factor(embarazadas.ag$Ano)
embarazadas.ag$Mes <- as.factor(embarazadas.ag$Mes)

modelo_embarazadas <- glm.nb(conteo ~ level + t + level:slope + Ano + Mes, data = embarazadas.ag)

exp(modelo_embarazadas[["coefficients"]][["level"]])
exp(modelo_embarazadas[["coefficients"]][["level:slope"]])
round(exp(confint(modelo_embarazadas)), digits = 3)

predichos.ag <- as.data.frame(predict.glm(modelo_embarazadas, type = "response", se.fit = TRUE))
embarazadas.ag0 <- embarazadas.ag %>% mutate(level = 0, slope = 0)
predichos.ag0 <- as.data.frame(predict.glm(modelo_embarazadas, newdata = embarazadas.ag0, type = "response", se.fit = TRUE))

embarazadas.ag <- cbind(embarazadas.ag, predichos.ag)
embarazadas.ag0 <- cbind(embarazadas.ag0, predichos.ag0)
embarazadas.ag$id <- "observado"
embarazadas.ag0$id <- "contrafactual"

embarazadas.contra <- rbind(embarazadas.ag, embarazadas.ag0)

embarazadas.contra$residual.scale <- NULL
embarazadas.contra$pred_lb <- embarazadas.contra$fit - 1.96*embarazadas.contra$se.fit
embarazadas.contra$pred_ub <- embarazadas.contra$fit + 1.96*embarazadas.contra$se.fit

ggplot(data = embarazadas.contra, aes(x = t, y = conteo, group = id, color = id)) +
  geom_point() +
  geom_line(aes(x = t, y = fit, group = id, color = id), linetype = "dotted") +
  geom_vline(xintercept = 39, colour = "red") +
  ylab ("N° ingresos al programa prenatal") +
  xlab ("Meses") +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(labels = scales::comma) +
  theme(text=element_text(size=30)) +
  theme(axis.text=element_text(size=30)) +
  theme(axis.title=element_text(size=30)) +
  geom_ribbon( aes(ymin = pred_lb, ymax = pred_ub, fill = id, color = NULL), alpha = .15) +
  theme_minimal()

embarazadas.contra.sum <- embarazadas.contra %>% group_by(id) %>% summarise(fit = sum(fit), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub))

# Recién nacido en control #

carpeta <- "C:/Users/Usuario/Desktop/Doctorado SP/Segundo semestre 2021/Investigación CMM-ESP/Serie A/"

serieA2017 <- fread(paste0(carpeta, "SerieA2017.txt"), sep = ";", dec = ",")
setDT(serieA2017)
reciennacido2017 <- serieA2017[CodigoPrestacion == "05225100"]
rm(serieA2017)

serieA2018 <- fread(paste0(carpeta, "SerieA2018.txt"), sep = ";", dec = ",")
setDT(serieA2018)
reciennacido2018 <- serieA2018[CodigoPrestacion == "05225100"]
rm(serieA2018)

serieA2019 <- fread(paste0(carpeta, "SerieA2019.txt"), sep = ";", dec = ",")
setDT(serieA2019)
reciennacido2019 <- serieA2019[CodigoPrestacion == "05225100"]
rm(serieA2019)

serieA2020 <- fread(paste0(carpeta, "SerieA2020.txt"), sep = ";", dec = ",")
setDT(serieA2020)
reciennacido2020 <- serieA2020[CodigoPrestacion == "05225100"]
rm(serieA2020)

reciennacido <- rbind(reciennacido2017, reciennacido2018, reciennacido2019, reciennacido2020)

reciennacido.ag <- reciennacido %>% group_by(Ano, Mes) %>% summarise(conteo = sum(Col01))

reciennacido.ag$t <- c(1:48)
reciennacido.ag$level <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1)
reciennacido.ag$slope <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9)
reciennacido.ag$Ano <- as.factor(reciennacido.ag$Ano)
reciennacido.ag$Mes <- as.factor(reciennacido.ag$Mes)

modelo_reciennacido <- glm.nb(conteo ~ level + t + level:slope + Ano + Mes, data = reciennacido.ag)

exp(modelo_reciennacido[["coefficients"]][["level"]])
exp(modelo_reciennacido[["coefficients"]][["level:slope"]])
round(exp(confint(modelo_reciennacido)), digits = 3)

predichos.ag <- as.data.frame(predict.glm(modelo_reciennacido, type = "response", se.fit = TRUE))
reciennacido.ag0 <- reciennacido.ag %>% mutate(level = 0, slope = 0)
predichos.ag0 <- as.data.frame(predict.glm(modelo_reciennacido, newdata = reciennacido.ag0, type = "response", se.fit = TRUE))

reciennacido.ag <- cbind(reciennacido.ag, predichos.ag)
reciennacido.ag0 <- cbind(reciennacido.ag0, predichos.ag0)
reciennacido.ag$id <- "observado"
reciennacido.ag0$id <- "contrafactual"

reciennacido.contra <- rbind(reciennacido.ag, reciennacido.ag0)

reciennacido.contra$residual.scale <- NULL
reciennacido.contra$pred_lb <- reciennacido.contra$fit - 1.96*reciennacido.contra$se.fit
reciennacido.contra$pred_ub <- reciennacido.contra$fit + 1.96*reciennacido.contra$se.fit

ggplot(data = reciennacido.contra, aes(x = t, y = conteo, group = id, color = id)) +
  geom_point() +
  geom_line(aes(x = t, y = fit, group = id, color = id), linetype = "dotted") +
  geom_vline(xintercept = 39, colour = "red") +
  ylab ("N° recién nacidos ingresados") +
  xlab ("Meses") +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(labels = scales::comma) +
  theme(text=element_text(size=30)) +
  theme(axis.text=element_text(size=30)) +
  theme(axis.title=element_text(size=30)) +
  geom_ribbon( aes(ymin = pred_lb, ymax = pred_ub, fill = id, color = NULL), alpha = .15) +
  theme_minimal()

reciennacidos.contra.sum <- reciennacido.contra %>% group_by(id) %>% summarise(fit = sum(fit), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub))
