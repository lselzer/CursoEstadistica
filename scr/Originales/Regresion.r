path <- c("E:\\Datos y scripts\\Regresión.csv")
arboles <- read.csv(path, header = TRUE, sep = ";")
attach(arboles)

# MODELO DE REGRESIÓN LINEAL SIMPLE

Toneladas <- lm(Ton.CO2 ~ ñire, data = arboles, )
# b1 es el coef de ñire y el intercepto es la ord a la origen
anova(Toneladas)
# ñire como predictor explica regresion xq es signifi
summary(Toneladas)
plot(Ton.CO2 ~ Ñire)
AIC(Toneladas)
coef(Toneladas)
model.matrix(Toneladas)




plot(Ton.CO2, rstudent(Toneladas))

plot(rstudent(Toneladas))

plot(Toneladas$fitted.values ~ Proyecto)
ind <- order(Proyecto)
lines(x = Proyecto[ind], y = Toneladas$fitted.values[ind])
points(Ton.CO2 ~ Proyecto, col = "red")


## MODELO CON MÚLTIPLES REGRESORES

Ton.mult <- lm(Ton.CO2 ~ Ñire * Alerce * Ciprés * Oregon, data = arboles)
anova(Ton.mult)
summary(Ton.mult)
plot(Ton.CO2 ~ Ñire + Ñire)


AIC(Ton.mult)
coef(Ton.mult)
model.matrix(Ton.mult)



plot(Ton.CO2, rstudent(Ton.mult))

plot(rstudent(Ton.mult))


abline(Toneladas, lty = 1, col = "red")

# Rutina de selección automática basada en AIC

selecauto <- step(Ton.mult, direction = "backward")
anova(selecauto, test = "Chisq")
summary(selecauto)
anova(selecauto)
coef(selecauto)

plot(fitted.values(selecauto) ~ Proyecto)
ind <- order(Proyecto)
lines(x = Proyecto[ind], y = selecauto$fitted.values[ind])
points(Ton.CO2 ~ Proyecto, col = "red")

# modelo a probar
Ton <- lm(Ton.CO2 ~ Ñire * Alerce + Ciprés:Oregon, data = arboles)
anova(Ton.CO2)
summary(Ton)
AIC(Ton)
# 
Ton <- lm(Ton.CO2 ~ Ñire + +Ciprés + Oregon + Ñire:Ciprés + Ciprés:Oregon, data = arboles)
anova(Ton.CO2)
summary(Ton)
AIC(Ton) 
