path <- c("C:\\Users\\cosca_000\\Dropbox\\Curso Hip?tesis Modelos y Datos  UNPSJB-CENPAT 2014\\Datos y scripts\\ancova.csv")

# E:\\Datos y scripts\\ancova.csv 
colest <- read.csv ("../dat/ancova.csv", header=TRUE, sep=';')


attach(colest)

boxplot(colesterol ~ 1 + edad, group = "region")
plot(colesterol ~ edad)
points(colesterol ~ edad, subset(colest, region == 1), type = "p", col = "red")
points(colesterol ~ edad, subset(colest, region == 2), type = "p", col = "blue")
summary(colest)



# Supuestos de ANCOVA Homocedacia
library(lawstat)
levene.test(colesterol, group = region)


# Existencia de regresi?n para cada nivel de la variable categ?rica

region1 <- lm(colesterol ~ edad, data = colest, region == 1)
anova(region1)
summary(region1)

region2 <- lm(colesterol ~ edad, data = colest, region == 2)
anova(region2)
summary(region2)

# PRUEBA DE PARALELISMO

paralelismo <- lm(colesterol ~ region * edad)
anova(paralelismo)
summary(paralelismo)

# AJUSTE DEL MODELO DE ANCOVA
ancova <- lm(colesterol ~ edad + region)
anova(ancova)
summary(ancova)
coef(ancova)
fitted(ancova)
plot(colesterol, rstudent(ancova))
model.matrix(ancova)

# GR?FICO DE LOS RESULTADOS

plot(colesterol ~ edad, data = colest, type = "n")
points(colesterol ~ edad, subset(colest, region == 1), type = "p", col = "red")
points(colesterol ~ edad, subset(colest, region == 2), type = "p", col = "blue")
abline(region1, lty = 1, col = "red")
abline(region2, lty = 1, col = "blue")
legend("bottomright", c("Region 1", "Region 2"), lty = c(1, 1), col = c("red", "blue")) 
