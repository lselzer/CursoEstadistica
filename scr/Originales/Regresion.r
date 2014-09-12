path <- c("E:\\Datos y scripts\\Regresi�n.csv")
arboles <- read.csv(path, header = TRUE, sep = ";")
attach(arboles)

# MODELO DE REGRESI�N LINEAL SIMPLE

Toneladas <- lm(Ton.CO2 ~ �ire, data = arboles, )
# b1 es el coef de �ire y el intercepto es la ord a la origen
anova(Toneladas)
# �ire como predictor explica regresion xq es signifi
summary(Toneladas)
plot(Ton.CO2 ~ �ire)
AIC(Toneladas)
coef(Toneladas)
model.matrix(Toneladas)




plot(Ton.CO2, rstudent(Toneladas))

plot(rstudent(Toneladas))

plot(Toneladas$fitted.values ~ Proyecto)
ind <- order(Proyecto)
lines(x = Proyecto[ind], y = Toneladas$fitted.values[ind])
points(Ton.CO2 ~ Proyecto, col = "red")


## MODELO CON M�LTIPLES REGRESORES

Ton.mult <- lm(Ton.CO2 ~ �ire * Alerce * Cipr�s * Oregon, data = arboles)
anova(Ton.mult)
summary(Ton.mult)
plot(Ton.CO2 ~ �ire + �ire)


AIC(Ton.mult)
coef(Ton.mult)
model.matrix(Ton.mult)



plot(Ton.CO2, rstudent(Ton.mult))

plot(rstudent(Ton.mult))


abline(Toneladas, lty = 1, col = "red")

# Rutina de selecci�n autom�tica basada en AIC

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
Ton <- lm(Ton.CO2 ~ �ire * Alerce + Cipr�s:Oregon, data = arboles)
anova(Ton.CO2)
summary(Ton)
AIC(Ton)
# 
Ton <- lm(Ton.CO2 ~ �ire + +Cipr�s + Oregon + �ire:Cipr�s + Cipr�s:Oregon, data = arboles)
anova(Ton.CO2)
summary(Ton)
AIC(Ton) 
