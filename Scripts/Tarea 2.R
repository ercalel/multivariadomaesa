library(mvtnorm)
library(ggplot2)
library(GGally)
library(MVN)
library(FactoMineR)
library(factoextra)
library(ggcorrplot)
library(corrplot)
library(psych)
library(polycor)
library(GPArotation)
# --------------------

semilla <- set.seed(34)

vector_medias <- c(1,2,-5)
matriz_covarianzas <- matrix(c(1,1,0,
                               1,2,0,
                               0,0,5), ncol = 3, byrow = F)
dist_mult_norm <- rmvnorm(1000, mean = vector_medias, sigma = matriz_covarianzas)

colMeans(dist_mult_norm)

cov(dist_mult_norm)

dist_mult_norm.v1 <- shapiro.test(dist_mult_norm[,1])
dist_mult_norm.v1

dist_mult_norm.v2 <- shapiro.test(dist_mult_norm[,2])
dist_mult_norm.v2

dist_mult_norm.v3 <- shapiro.test(dist_mult_norm[,3])
dist_mult_norm.v3

ggpairs(as.data.frame(dist_mult_norm), title = "dist_mult_norm", diag = NULL, upper = NULL)

mvn(dist_mult_norm, mvnTest = "mardia", multivariatePlot = "qq")

# --------------------

frets

ggpairs(frets, title = "frets", diag = NULL, upper = NULL)

mvn(frets, multivariatePlot = "qq")

mvn(frets, mvnTest = "royston", univariateTest = "Lillie", desc = TRUE, univariatePlot = "histogram")

cor(frets)
pairs(frets, panel=panel.smooth, cex.labels = 1, font.labels=1)

modelo <- lm(frets$l1~frets$l2 + frets$b2)
cor(frets$l1,modelo$fitted.values)

cor(frets$b1,modelo$fitted.values)

x = frets[,1:2]
y = frets[,3:4]
x
CCorA(x,y)

# --------------------

head(USArrests)
str(USArrests)

pca_usarrest <- PCA(USArrests, graph = F)

var <- get_pca_var(pca_usarrest)
ind <- get_pca_ind(pca_usarrest)

fviz_screeplot(pca_usarrest,addlabels=T,ylim=c(0,75))

pca_usarrest$eig

corrplot(var$cos2,is.corr = F)

ggcorrplot(cor(iris[,-5]),hc.order = T,type="lower")

fviz_contrib(pca_usarrest,choice = "var",axes = 1)
fviz_contrib(pca_usarrest,choice = "var",axes = 2)

fviz_pca_var(pca_usarrest,col.var="contrib",
             gradient.col="npg",repel = T)+ggtitle("Gráfico de variables")+theme_minimal()

fviz_pca_ind(pca_usarrest,geom.ind = "point",
             col.ind="cos2",gradient.col="jco") + ggtitle("Grafico de  los individuos")

fviz_pca_biplot(pca_usarrest,geom.ind="point",
                addEllipses = T,
                palette =  "uchicago")+ggtitle("Variables e individuos")


fviz_pca_biplot(pca_usarrest,var="contrib",
                palette =  "uchicago" ,repel = T)+ggtitle("Variables e individuos")+theme_minimal()

# --------------------

str(estilos)
head(estilos)
esitlos_f <- estilos[,1:36]

mat_cor <- hetcor(esitlos_f)$correlations
ggcorrplot(mat_cor,type="lower",hc.order = T)

cortest.bartlett(mat_cor)->p_esf
p_esf$p

KMO(mat_cor)

scree(mat_cor)
fa.parallel(mat_cor,n.obs=200,fa="fa",fm="minres")

modelo <- fa(estilos[,33:36], nfactors = 4, rotate = "varimax", fm="minres")
modelo

c1 <- sort(modelo$communality,decreasing = T)
head(cbind(c1))

u1 <- sort(modelo$uniquenesses,decreasing = T)
head(cbind(u1))

biplot.psych(fa(estilos[,33:36], nfactors = 2, fm="minres", rotate = "varimax"), main = "Biplot con rotación varimax", 
             col = c(2,3,4), pch = c(21,18), group = c(1,2))

modelo_varimax<-fa(mat_cor,nfactors = 5,rotate = "varimax", fa="minres")
fa.diagram(modelo_varimax)
