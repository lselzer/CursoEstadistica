list()
rm()

netbook <- c("C:\\Documents and Settings\\Mariano\\My Documents\\My Dropbox\\MMAEC\\Clases\\Clase 18\\Horseshoecrab.csv")
netbook <- c("Desktop\\Horseshoecrab.csv")
crab <- read.csv(netbook, sep = ";", dec = ",")

attach(crab)

plot(Satélites ~ Ancho)
plot(Satélites ~ Peso)


# Ajuste de la regresión de poisson con el log link ?glm
crab_ancho <- glm(formula = Satélites ~ Ancho, family = poisson(link = log), data = crab)
crab_ancho
summary(crab_ancho)
print(anova(crab_ancho, test = "Chisq"))
anova(crab_ancho)
coef(crab_ancho)
model.matrix(crab_ancho)
exp(crab_ancho$coef[2])
AIC(crab_ancho)
plot(fitted.values(crab_ancho) ~ Ancho, type = "p")
ind <- order(crab$Ancho)
lines(x = crab$Ancho[ind], y = crab_ancho$fitted.values[ind])
points(Satélites ~ Ancho, col = "red")

# Ajuste de la regresión de poisson con el identity link

id.crab_ancho <- glm(formula = Satélites ~ 1 + Ancho, family = poisson(link = identity), 
    data = crab, start = coef(crab_ancho))
id.crab_ancho
summary(id.crab_ancho)
AIC(id.crab_ancho)
print(anova(id.crab_ancho, test = "Chisq"))

# Comparación entre las lineas ajustadas con las distintas link para regresión de
# Poisson GLM (log vs. identity links)
plot(fitted.values(crab_ancho) ~ Ancho)
axis(2, at = 0:5)
axis(1, at = seq(20, 34, 2))
ind <- order(crab$Ancho)
lines(x = crab$Ancho[ind], y = crab_ancho$fitted.values[ind])
lines(x = crab$Ancho[ind], y = id.crab_ancho$fitted.values[ind], col = "blue")
points(Satélites ~ Ancho, col = "red")
text(x = 23.5, y = 3, "Log Link")
text(x = 29.75, y = 2.9, "Identity Link", col = "blue")



# Ahora vamos a probar distintos modelos con el link Log(es el default para la familia
# poisson)

crab_null <- glm(formula = Satélites ~ 1, family = poisson, data = crab)


plot(fitted.values(crab_null) ~ Ancho)
print(anova(crab_null, test = "Chisq"))
AIC(crab_null)

##################################################################################### Sólo los predictores contínuos - Aditivo
crab_cont.1 <- glm(formula = Satélites ~ Ancho + Peso, family = poisson, data = crab)
print(anova(crab_cont.1, test = "Chisq"))
summary(crab_cont.1)
coef(crab_cont.1)
plot(fitted.values(crab_cont.1) ~ Ancho, col = "green")
ind <- order(crab$Ancho)
lines(x = crab$Ancho[ind], y = crab_cont.1$fitted.values[ind], col = "green")
points(Satélites ~ Ancho, col = "red")
1 - pchisq(crab_cont.1$deviance, crab_cont.1$df.residual)
AIC(crab_cont.1)

##################################################################################### Sólo los predictores contínuos - Interacción

crab_cont.2 <- glm(formula = Satélites ~ Ancho * Peso, family = poisson, data = crab)
print(anova(crab_cont.2, test = "Chisq"))
summary(crab_cont.2)
coef(crab_cont.2)
plot(fitted.values(crab_cont.2) ~ Ancho)
ind <- order(crabs$Ancho)
lines(x = crab$Ancho[ind], y = crab_cont.2$fitted.values[ind])
points(Satélites ~ Ancho, col = "red")
1 - pchisq(crab_cont.2$deviance, crab_cont.2$df.residual)
AIC(crab_cont.2)

# Evalúo la deviance para estos dos modelos anidados
1 - pchisq(crab_cont.1$deviance - crab_cont.2$deviance, crab_cont.1$df.residual - crab_cont.2$df.residual)

##################################################################################### UTILIZO PREDICTORES CONTINUOS Y CATEGÓRICOS
crab_saturated <- glm(formula = Satélites ~ Color * Espina * Ancho * Peso, family = poisson, 
    data = crab)
print(anova(crab_saturated, test = "Chisq"))
summary(crab_saturated)

plot(fitted.values(crab_saturated) ~ Ancho)
ind <- order(crab$Ancho)
lines(x = crab$Ancho[ind], y = crab_saturated$fitted.values[ind])
points(Satélites ~ Ancho, col = "red")
1 - pchisq(crab_saturated$deviance, crab_saturated$df.residual)
AIC(crab_saturated)
#################################################################################### 
crab_aditivo <- glm(formula = Satélites ~ Color + Espina + Ancho + Peso, family = poisson, 
    data = crab)
print(anova(crab_aditivo, test = "Chisq"))
coef(crab_aditivo)
plot(fitted.values(crab_aditivo) ~ Ancho)
ind <- order(crab$Ancho)
lines(x = crab$Ancho[ind], y = crab_aditivo$fitted.values[ind])
points(Satélites ~ Ancho, col = "red")
1 - pchisq(crab_aditivo$deviance, crab_aditivo$df.residual)
AIC(crab_aditivo)
########################################################################################### 
id.crab_aditivo <- glm(formula = Satélites ~ Color + Espina + Ancho + Peso, family = poisson(link = identity), 
    start = coef(crab_aditivo), data = crab)
print(anova(id.crab_aditivo, test = "Chisq"))

plot(fitted.values(id.crab_aditivo) ~ Ancho)
ind <- order(crab$Ancho)
lines(x = crab$Ancho[ind], y = id.crab_aditivo$fitted.values[ind])
points(Satélites ~ Ancho, col = "red")
1 - pchisq(id.crab_aditivo$deviance, crab_aditivo$df.residual)
AIC(id.crab_aditivo)
################################################################################## 

# Rutina de selección automática basada en AIC

selecauto <- step(crab_saturated, direction = "backward")
anova(selecauto, test = "Chisq")
summary(selecauto)
anova(selecauto)

plot(fitted.values(selecauto) ~ Ancho)
ind <- order(crab$Ancho)
lines(x = crab$Ancho[ind], y = selecauto$fitted.values[ind])
points(Satélites ~ Ancho, col = "red")


# Comparaciónde AIC crudos
AIC(crab_saturated, selecauto, crab_aditivo)

# Comparación de la deviance

anova(crab_saturated, selecauto, crab_aditivo)

deviance(crab_saturated) - deviance(crab_aditivo)
df(crab_saturated) - df(crab_aditivo)

################################################### 

# Modelo Logit con un predictor
crab_log <- glm(formula = Logit ~ 1 + Ancho, family = binomial(link = logit), data = crab)

plot(Logit ~ Ancho)
plot(Logit ~ Peso)
crab_log
summary(crab_log)
anova(crab_log)
coef(crab_log)
model.matrix(crab_log)

AIC(crab_log)
plot(fitted.values(crab_log) ~ Ancho, ylim = c(0, 1))
ind <- order(crab$Ancho)
lines(x = crab$Ancho[ind], y = crab_log$fitted.values[ind])
points(Logit ~ Ancho, col = "red")
######################################### Modelo Logit con varios predictores
crab_log_sat <- glm(formula = Logit ~ Ancho * Color * Peso * Espina, family = binomial(link = logit), 
    data = crab)

crab_log_sat
summary(crab_log_sat)
anova(crab_log_sat)
coef(crab_log_sat)
model.matrix(crab_log_sat)

AIC(crab_log_sat)
plot(fitted.values(crab_log_sat) ~ Ancho)
ind <- order(crab$Ancho)
lines(x = crab$Ancho[ind], y = crab_log_sat$fitted.values[ind])
points(Logit ~ Ancho, col = "red")


# Rutina de selección automática basada en AIC para el modelo logit

selecauto.logit <- step(crab_log_sat, direction = "backward")
anova(selecauto.logit, test = "Chisq")
summary(selecauto.logit)
anova(selecauto.logit)

plot(fitted.values(selecauto.logit) ~ Ancho)
ind <- order(crab$Ancho)
lines(x = crab$Ancho[ind], y = selecauto.logit$fitted.values[ind])
points(Satélites ~ Ancho, col = "red")

##################################################################### 

# MODELO LOG-LINEAR

tabla.cont <- table(Log.lin, Espina, Color)
ftable(tabla.cont)
tablafrec <- as.data.frame(tabla.cont)

# Modelo aditivo (sin interacciones) también llamado el modelo de independencia
aditivo <- glm(Freq ~ Color + Espina + Log.lin, family = poisson, data = tablafrec)
anova(aditivo, test = "Chisq")
1 - pchisq(aditivo$deviance, aditivo$df.residual)
summary(aditivo)

# Ninguno de los dactoes por sí solo ajustan

# Modelo saturado
saturado <- glm(Freq ~ Color * Espina * Log.lin, family = poisson, data = tablafrec)
anova(saturado, test = "Chisq")
summary(saturado)
1 - pchisq(saturado$deviance, saturado$df.residual)

# Utilizamos la rutina de selección automática
selecauto <- step(saturado, direction = "backward")
anova(selecauto, test = "Chisq")
summary(selecauto)
anova(selecauto)

# Comparaciónde AIC crudos
AIC(saturado, aditivo, selecauto)

# Comparación de la deviance

anova(saturado, aditivo, selecauto)

# R2
1 - (selecauto$dev/selecauto$null)

selecauto$fit

plot(fitted.values(selecauto) ~ tablafrec$Freq, type = "p")
ind <- order(tablafrec$Freq)
lines(x = tablafrec$Freq[ind], y = selecauto$fitted.values[ind]) 
