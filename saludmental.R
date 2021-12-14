library(data.table)
library(tidyverse)
library(MASS)
library(ggplot2)

carpeta <- "C:/Users/Usuario/Desktop/Doctorado SP/Segundo semestre 2021/Investigación CMM-ESP/Serie A/"

serieA2017 <- fread(paste0(carpeta, "SerieA2017.txt"), sep = ";", dec = ",")
setDT(serieA2017)
saludmental2017 <- serieA2017[CodigoPrestacion == "06020201" | CodigoPrestacion == "06020208" | CodigoPrestacion == "06020202" | CodigoPrestacion == "06020602" | CodigoPrestacion == "06020206" | CodigoPrestacion == "06200400" | CodigoPrestacion == "06904900" | CodigoPrestacion == "06200500" | CodigoPrestacion == "06200501" | CodigoPrestacion == "06905912"]
saludmental2017 <- saludmental2017[complete.cases(saludmental2017[ , c('Col01')]), ] 
rm(serieA2017)

serieA2018 <- fread(paste0(carpeta, "SerieA2018.txt"), sep = ";", dec = ",")
setDT(serieA2018)
saludmental2018 <- serieA2018[CodigoPrestacion == "06020201" | CodigoPrestacion == "06020208" | CodigoPrestacion == "06020202" | CodigoPrestacion == "06020602" | CodigoPrestacion == "06020206" | CodigoPrestacion == "06200400" | CodigoPrestacion == "06904900" | CodigoPrestacion == "06200500" | CodigoPrestacion == "06200501" | CodigoPrestacion == "06905912"]
saludmental2018 <- saludmental2018[complete.cases(saludmental2018[ , c('Col01')]), ] 
rm(serieA2018)

serieA2019 <- fread(paste0(carpeta, "SerieA2019.txt"), sep = ";", dec = ",")
setDT(serieA2019)
saludmental2019 <- serieA2019[CodigoPrestacion == "06020201" | CodigoPrestacion == "06020208" | CodigoPrestacion == "06020202" | CodigoPrestacion == "06020602" | CodigoPrestacion == "06020206" | CodigoPrestacion == "06200400" | CodigoPrestacion == "06904900" | CodigoPrestacion == "06200500" | CodigoPrestacion == "06200501" | CodigoPrestacion == "06905912"]
saludmental2019 <- saludmental2019[complete.cases(saludmental2019[ , c('Col01')]), ] 
rm(serieA2019)

serieA2020 <- fread(paste0(carpeta, "SerieA2020.txt"), sep = ";", dec = ",")
setDT(serieA2020)
saludmental2020 <- serieA2020[CodigoPrestacion == "06020201" | CodigoPrestacion == "06020208" | CodigoPrestacion == "06020202" | CodigoPrestacion == "06020602" | CodigoPrestacion == "06020206" | CodigoPrestacion == "06200400" | CodigoPrestacion == "06904900" | CodigoPrestacion == "06200500" | CodigoPrestacion == "06200501" | CodigoPrestacion == "06905912"]
saludmental2020 <- saludmental2020[complete.cases(saludmental2020[ , c('Col01')]), ] 
rm(serieA2020)

saludmental <- rbind(saludmental2017, saludmental2018, saludmental2019, saludmental2020)

saludmental.ag <- saludmental %>% group_by(Ano, Mes) %>% summarise(conteo = sum(Col01))

saludmental.ag$t <- c(1:48)
saludmental.ag$level <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1)
saludmental.ag$slope <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9)
saludmental.ag$Ano <- as.factor(saludmental.ag$Ano)
saludmental.ag$Mes <- as.factor(saludmental.ag$Mes)

modelo_saludmental <- glm.nb(conteo ~ level + t + level:slope + Ano + Mes, data = saludmental.ag)

exp(modelo_saludmental[["coefficients"]][["level"]])
exp(modelo_saludmental[["coefficients"]][["level:slope"]])
round(exp(confint(modelo_saludmental)), digits = 4)

predichos.ag <- as.data.frame(predict.glm(modelo_saludmental, type = "response", se.fit = TRUE))
saludmental.ag0 <- saludmental.ag %>% mutate(level = 0, slope = 0)
predichos.ag0 <- as.data.frame(predict.glm(modelo_saludmental, newdata = saludmental.ag0, type = "response", se.fit = TRUE))

saludmental.ag <- cbind(saludmental.ag, predichos.ag)
saludmental.ag0 <- cbind(saludmental.ag0, predichos.ag0)
saludmental.ag$id <- "observado"
saludmental.ag0$id <- "contrafactual"

saludmental.contra <- rbind(saludmental.ag, saludmental.ag0)

saludmental.contra$residual.scale <- NULL
saludmental.contra$pred_lb <- saludmental.contra$fit - 1.96*saludmental.contra$se.fit
saludmental.contra$pred_ub <- saludmental.contra$fit + 1.96*saludmental.contra$se.fit

ggplot(data = saludmental.contra, aes(x = t, y = conteo, group = id, color = id)) +
  geom_point() +
  geom_line(aes(x = t, y = fit, group = id, color = id), linetype = "dotted") +
  geom_vline(xintercept = 39, colour = "red") +
  ylab ("N° de controles de salud mental") +
  xlab ("Meses") +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(labels = scales::comma) +
  theme(text=element_text(size=30)) +
  theme(axis.text=element_text(size=30)) +
  theme(axis.title=element_text(size=30)) +
  geom_ribbon( aes(ymin = pred_lb, ymax = pred_ub, fill = id, color = NULL), alpha = .15) +
  theme_minimal()

saludmental.contra.sum <- saludmental.contra %>% group_by(id) %>% summarise(fit = sum(fit), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub))

# Disminución absoluta de controles de salud mental = 2.248.277 (IC95% 1.525.882 - 2.970.672) #

# Disminución relativa de controles de salud mental = 15.8% (IC95% 13.2% - 17.6%) #

## Incluye controles de salud mental por llamadas telefónicas en contexto de pandemia ##

carpeta <- "C:/Users/Usuario/Desktop/Doctorado SP/Segundo semestre 2021/Investigación CMM-ESP/Serie A/"

serieA2017 <- fread(paste0(carpeta, "SerieA2017.txt"), sep = ";", dec = ",")
setDT(serieA2017)
saludmental2017 <- serieA2017[CodigoPrestacion == "06020201" | CodigoPrestacion == "06020208" | CodigoPrestacion == "06020202" | CodigoPrestacion == "06020602" | CodigoPrestacion == "06020206" | CodigoPrestacion == "06200400" | CodigoPrestacion == "06904900" | CodigoPrestacion == "06200500" | CodigoPrestacion == "06200501" | CodigoPrestacion == "06905912"]
saludmental2017 <- saludmental2017[complete.cases(saludmental2017[ , c('Col01')]), ] 
rm(serieA2017)

serieA2018 <- fread(paste0(carpeta, "SerieA2018.txt"), sep = ";", dec = ",")
setDT(serieA2018)
saludmental2018 <- serieA2018[CodigoPrestacion == "06020201" | CodigoPrestacion == "06020208" | CodigoPrestacion == "06020202" | CodigoPrestacion == "06020602" | CodigoPrestacion == "06020206" | CodigoPrestacion == "06200400" | CodigoPrestacion == "06904900" | CodigoPrestacion == "06200500" | CodigoPrestacion == "06200501" | CodigoPrestacion == "06905912"]
saludmental2018 <- saludmental2018[complete.cases(saludmental2018[ , c('Col01')]), ] 
rm(serieA2018)

serieA2019 <- fread(paste0(carpeta, "SerieA2019.txt"), sep = ";", dec = ",")
setDT(serieA2019)
saludmental2019 <- serieA2019[CodigoPrestacion == "06020201" | CodigoPrestacion == "06020208" | CodigoPrestacion == "06020202" | CodigoPrestacion == "06020602" | CodigoPrestacion == "06020206" | CodigoPrestacion == "06200400" | CodigoPrestacion == "06904900" | CodigoPrestacion == "06200500" | CodigoPrestacion == "06200501" | CodigoPrestacion == "06905912"]
saludmental2019 <- saludmental2019[complete.cases(saludmental2019[ , c('Col01')]), ] 
rm(serieA2019)

serieA2020 <- fread(paste0(carpeta, "SerieA2020.txt"), sep = ";", dec = ",")
setDT(serieA2020)
saludmental2020 <- serieA2020[CodigoPrestacion == "06020201" | CodigoPrestacion == "06020208" | CodigoPrestacion == "06020202" | CodigoPrestacion == "06020602" | CodigoPrestacion == "06020206" | CodigoPrestacion == "06200400" | CodigoPrestacion == "06904900" | CodigoPrestacion == "06200500" | CodigoPrestacion == "06200501" | CodigoPrestacion == "06905912"]
saludmental2020 <- saludmental2020[complete.cases(saludmental2020[ , c('Col01')]), ] 
rm(serieA2020)

serieF2020 <- fread(paste0(carpeta, "SerieF2020.txt"), sep = ";", dec = ",")
setDT(serieF2020)
saludmental2020a <- serieF2020[CodigoPrestacion == "01950712V" | CodigoPrestacion == "01950713V" | CodigoPrestacion == "01950714V" | CodigoPrestacion == "01950715V" | CodigoPrestacion == "01950716V" | CodigoPrestacion == "01950717V" | CodigoPrestacion == "01950718V" | CodigoPrestacion == "01950719V" | CodigoPrestacion == "01950720V" | CodigoPrestacion == "01950721V" | CodigoPrestacion == "01950723V" | CodigoPrestacion == "01950724V" | CodigoPrestacion == "01950725V" | CodigoPrestacion == "01950726V" | CodigoPrestacion == "01950727V" | CodigoPrestacion == "01950728V" | CodigoPrestacion == "01950729V" | CodigoPrestacion == "01950730V" | CodigoPrestacion == "01950731V" | CodigoPrestacion == "01950732V"]
saludmental2020a <- saludmental2020a[complete.cases(saludmental2020a[ , c('Col01')]), ] 
rm(serieF2020)

saludmental <- rbind(saludmental2017, saludmental2018, saludmental2019, saludmental2020, saludmental2020a)

saludmental.ag <- saludmental %>% group_by(Ano, Mes) %>% summarise(conteo = sum(Col01))

saludmental.ag$t <- c(1:48)
saludmental.ag$level <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1)
saludmental.ag$slope <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9)
saludmental.ag$Ano <- as.factor(saludmental.ag$Ano)
saludmental.ag$Mes <- as.factor(saludmental.ag$Mes)

modelo_saludmental <- glm.nb(conteo ~ level + t + level:slope + Ano + Mes, data = saludmental.ag)

exp(modelo_saludmental[["coefficients"]][["level"]])
exp(modelo_saludmental[["coefficients"]][["level:slope"]])
round(exp(confint(modelo_saludmental)), digits = 4)

predichos.ag <- as.data.frame(predict.glm(modelo_saludmental, type = "response", se.fit = TRUE))
saludmental.ag0 <- saludmental.ag %>% mutate(level = 0, slope = 0)
predichos.ag0 <- as.data.frame(predict.glm(modelo_saludmental, newdata = saludmental.ag0, type = "response", se.fit = TRUE))

saludmental.ag <- cbind(saludmental.ag, predichos.ag)
saludmental.ag0 <- cbind(saludmental.ag0, predichos.ag0)
saludmental.ag$id <- "observado"
saludmental.ag0$id <- "contrafactual"

saludmental.contra <- rbind(saludmental.ag, saludmental.ag0)

saludmental.contra$residual.scale <- NULL
saludmental.contra$pred_lb <- saludmental.contra$fit - 1.96*saludmental.contra$se.fit
saludmental.contra$pred_ub <- saludmental.contra$fit + 1.96*saludmental.contra$se.fit

ggplot(data = saludmental.contra, aes(x = t, y = conteo, group = id, color = id)) +
  geom_point() +
  geom_line(aes(x = t, y = fit, group = id, color = id), linetype = "dotted") +
  geom_vline(xintercept = 39, colour = "red") +
  ylab ("N° de controles de salud mental") +
  xlab ("Meses") +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(labels = scales::comma) +
  theme(text=element_text(size=30)) +
  theme(axis.text=element_text(size=30)) +
  theme(axis.title=element_text(size=30)) +
  geom_ribbon( aes(ymin = pred_lb, ymax = pred_ub, fill = id, color = NULL), alpha = .15) +
  theme_minimal()

saludmental.contra.sum <- saludmental.contra %>% group_by(id) %>% summarise(fit = sum(fit), pred_lb = sum(pred_lb), pred_ub = sum(pred_ub))

# Disminución absoluta de controles de salud mental = 1.358.658 (IC95% 637.855 - 2.079.462) #

# Disminución relativa de controles de salud mental = 9.5% (IC95% 5.9% - 11.7%) #
