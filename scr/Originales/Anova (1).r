# Ejercicio Nº 1

path <- c("C:\\Users\\cosca_000\\Dropbox\\Curso Hipótesis Modelos y Datos  UNPSJB-CENPAT 2014\\Datos y scripts\\anova.csv")


molerat <- read.csv(path, header = TRUE, sep = ";")
molerat
molerat <- stack(molerat)

colnames(molerat) <- c("n.vivos", "colonia")
attach(molerat)
respuesta = n.vivos
fcat = colonia
boxplot(respuesta ~ fcat)
plot(fcat, respuesta)
summary(respuesta)


library(lawstat)
levene.test(respuesta, group = fcat)


# Como los efectos fuesen fijos

anov <- lm(respuesta ~ fcat)  #Con esta hago explícito que utilizo al primer grupo como regerencia para calcular la ordenada al origen (es decir el mu del primer grupo)
anova(anov)
# para ver el resto de las medias de b,c,d sumo el intercepto a la media de a
summary(anov)
effects(anov)
qqnorm(residuals(anov))
plot(rstudent(anov))

qqplot(fitted(anov), rstudent(anov))
model.matrix(anov)
summary(aov(respuesta ~ fcat))
model.matrix(lm(respuesta ~ -1 + fcat))  # saco el intercepto
summary(lm(respuesta ~ -1 + fcat)) 
