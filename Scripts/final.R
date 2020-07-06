library(readxl)
library(ggpubr)
library(MVN)
library(biotools)
library(car)
library(lsr)
#######
library(readr)
library(tidyverse)
library(ggcorrplot)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(pROC)

# P1

Base_entrenamiento <- read_csv("Base_entrenamiento.csv")
summary(Base_entrenamiento)
str(Base_entrenamiento)

Base_entrenamiento <- Base_entrenamiento %>% 
  select(-cp_inicial_menos_saldo,
         -cp_inicial_menos_saldo,
         -cp_cuotas_falta,
         -cp_nro_cuota,
         -cp_valor_inicial,
         -cp_cuota_sobre_saldo,
         -cp_saldo_sobre_inicial,
         -cp_esta_cuota_otro,
         -cp_porc_valorcuot_ing,
         -cp_saldo,
         -cp_cuota_sobre_inicial,
         -cp_porc_saldo_ing,
         -cp_valor_cuota,
         -banca_completa,
         -segmentoestructural,
         -subsegmentoestructural
         -y_auto_cura,
         -llave)

Base_entrenamiento.data <- scale(Base_entrenamiento %>% 
        select(-y_auto_cura,-llave))

Base_entrenamiento.data <- cbind(Base_entrenamiento.data, Base_entrenamiento[, 107:108])


ggcorrplot(cor(Base_entrenamiento.data %>% 
                 select(-y_auto_cura,-llave)), hc.order = T, type = "lower")

pca_Base_entrenamiento <- PCA(Base_entrenamiento.data %>% 
                                select(-y_auto_cura,-llave), graph = F)

var <- get_pca_var(pca_Base_entrenamiento)

corrplot(var$cos2, is.corr = F)

fviz_screeplot(pca_Base_entrenamiento, addlabels = T, ylim =c (0, 16))

fviz_contrib(pca_Base_entrenamiento, choice = "var", axes = 5)

# -------


# explorando variable respuesta
prop.table(table(Base_entrenamiento$y_auto_cura))

# fabricando el modelo
modelo <- glm(Base_entrenamiento$y_auto_cura~nro_gestiones+
                gestiones_eficaces+
                mejor_gestion+
                gsm_prom_dias_gest+
                gsm_mejor_gestion_3m+
                gsm_mejor_gestion+
                gestiones_prod+
                pc_cant_moras_60_ult_12_meses+
                pc_cant_moras_90_ult_12_meses+
                pc_cant_mora90_ult_12m_total+
                pc_cant_moras_30_ult_12_meses+
                pc_cant_moras_60_ult_3_meses+
                pc_cant_moras_30_ult_3_meses,
              data = Base_entrenamiento.data,
              family = binomial)

#calculando las  variable respuesta 
pred_p <- predict(modelo, type="response")
pred_p

# convertimos la probabilidad de clasificación
pred <- ifelse(pred_p > 0.5,1,0)
sum(pred)

# evaluamos la precisión del modelo
mean(Base_entrenamiento$y_auto_cura == pred)

conf <- table(Base_entrenamiento$y_auto_cura, pred, dnn = c("actual", "predicho"))
conf

mosaicplot(conf, col = 2:4)

#Base_entrenamiento.data$pred <- pred

#View(Base_entrenamiento.data)

ROC <- roc(Base_entrenamiento$y_auto_cura, pred)
plot(ROC)

auc(ROC)




Base_prueba <- read_csv("Base_prueba.csv")

Base_prueba <- Base_prueba %>%
  select("nro_gestiones",
         "gestiones_eficaces",
         "mejor_gestion",
         "gsm_prom_dias_gest",
         "gsm_mejor_gestion_3m",
         "gsm_mejor_gestion",
         "gestiones_prod",
         "pc_cant_moras_60_ult_12_meses",
         "pc_cant_moras_90_ult_12_meses",
         "pc_cant_mora90_ult_12m_total",
         "pc_cant_moras_30_ult_12_meses",
         "pc_cant_moras_60_ult_3_meses",
         "pc_cant_moras_30_ult_3_meses",
         "llave")

pred_p <- predict(modelo, Base_prueba[,-14])
pred <- ifelse(pred_p > 0.5,1,0)
Base_prueba$probabilidad  <- pred_p
Base_prueba$auto_cura <- pred

write_csv(Base_prueba[, 14:16], "Base_prueba_evaluado.csv")

# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------

# P2

escarab <- read_excel("escarab.xlsx")
head(escarab, 3)

# Visualización
ggboxplot(escarab, x = "Sexo",y = c("X1", "X2", "X3", "X4", "X5"), merge = T, palette = "jco")

ggboxplot(escarab, x = "Localidad",y = c("X1", "X2", "X3", "X4", "X5"), merge = T, palette = "jco")

# Cálculo estadístico
aggregate(escarab[,1:5], list(escarab$Sexo, escarab$Localidad), mean)

aggregate(escarab[,1:5], list(escarab$Sexo, escarab$Localidad), length)

aggregate(escarab[,1:5], list(escarab$Sexo, escarab$Localidad), sd)

# Normalidad univariada
normal <- function(vec){
  shapiro.test(vec)$p
}
aggregate(escarab[,1:5], list(escarab$Sexo, escarab$Localidad), normal)

# Normalidad Multiple
mvn(escarab[ , c(1,2,3,4,5)], mvnTest = "hz")

# Pruebas de varianzas y covarianzas
boxM(data = escarab[,c(1,2,3,4,5)], grouping = escarab$Sexo)

boxM(data = escarab[,c(1,2,3,4,5)], grouping = escarab$Localidad)

leveneTest(X1~Sexo, escarab)
leveneTest(X1~Localidad, escarab)

leveneTest(X2~Sexo, escarab)
leveneTest(X2~Localidad, escarab)

leveneTest(X3~Sexo, escarab)
leveneTest(X3~Localidad, escarab)

leveneTest(X4~Sexo, escarab)
leveneTest(X4~Localidad, escarab)

leveneTest(X5~Sexo, escarab)
leveneTest(X5~Localidad, escarab)

# Modelo Manova
mod1 <- manova(cbind(X1, X2, X3, X4, X5)~Sexo+Localidad, data = escarab)
summary(mod1)

## modelo con interacción
mod2 <- manova(cbind(X1, X2, X3, X4, X5)~Sexo*Localidad, data = escarab)
summary(mod2)

# Modelos univariados
m1 <- aov(X1~Localidad, data=escarab)
summary(m1)
m2 <- aov(X2~Localidad, data=escarab)
summary(m2)
m3 <- aov(X3~Localidad, data=escarab)
summary(m3)
m4 <- aov(X4~Localidad, data=escarab)
summary(m4)
m5 <- aov(X5~Localidad, data=escarab)
summary(m5)

m6 <- aov(X1~Localidad*Sexo, data=escarab)
summary(m6)
m7 <- aov(X2~Localidad*Sexo, data=escarab)
summary(m7)
m8 <- aov(X3~Localidad*Sexo, data=escarab)
summary(m8)
m9 <- aov(X2~Localidad*Sexo, data=escarab)
summary(m9)
m10 <- aov(X3~Localidad*Sexo, data=escarab)
summary(m10)

# análisis post hoc
etaSquared(m1)
etaSquared(m2)
etaSquared(m3)
etaSquared(m4)
etaSquared(m5)
etaSquared(m6)
etaSquared(m7)
etaSquared(m8)
etaSquared(m9)
etaSquared(m10)

post <- TukeyHSD(m1)
post

plot(post)
# -------------------------------------------------------------------------------------------
