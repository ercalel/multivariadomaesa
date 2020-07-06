library(readr)
library(tidyverse)
library(MVN)
library(biotools)
library(MASS)
library(dplyr)
library(pROC)


boston <- read.csv("boston-housing-classification.csv")
head(boston, 3)

normalidad<-function(var){
  shapiro.test(var)$p
}

# Verificamos normalidad para cada grupo 
tb1 <- boston %>%
  group_by(MEDV_CAT) %>% 
  summarise_all(normalidad)

View(tb1)

####Normalidad Multivariada
mvn(boston[-14], mvnTest = "mardia")

# prueba de box
# Ho: matriz de covarianza es constante
boxM(data = boston[-14], grouping = boston[,14])

# ser rechaza  la Ho

# LDA
modelo <- lda(MEDV_CAT~.,data=boston)
modelo

prediccion <- predict(modelo,boston[-14])
m_confusion <- table(boston$MEDV_CAT, 
                     prediccion$class,
                     dnn=c("Real","Predicho"))
m_confusion

mosaicplot(m_confusion,col=2:4)

precision <- mean(boston$MEDV_CAT == prediccion$class)
precision

error <- 1 - precision
error

# ----------------------------------------------------------------------------

modelo2 <- qda(MEDV_CAT~.,data = boston)
prediccion2 <- predict(modelo2,boston[-14])
m_confusion2 <- table(boston$MEDV_CAT,
                      prediccion2$class,
                      dnn=c("Real","predicho"))
m_confusion2

mosaicplot(m_confusion2,col=2:4)

precision2 <- mean(boston$MEDV_CAT == prediccion2$class)
precision2

error2 <- 1 - precision2
error2

# ----------------------------------------------------------------------------

glimpse(boston)

# explorando variable respuesta
prop.table(table(boston$MEDV_CAT))

# fabricando el modelo
mod1 <- glm(MEDV_CAT~.,
            data = boston,
            family = binomial)
mod1

#calculando las  variable respuesta 
pred_p <- predict(mod1,type="response")

# convertimos la probabilidad de clasificación
pred <- ifelse(pred_p>0.05,1,0)
sum(pred)

# evaluamos la precisión del modelo
mean(boston$MEDV_CAT==pred)

conf <- table(boston$MEDV_CAT, pred, dnn=c("actual","predicho"))

mosaicplot(conf,col=2:4)

ROC <- roc(boston$MEDV_CAT,pred)
plot(ROC)

auc(ROC)
