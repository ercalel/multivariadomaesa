I = diag(5)
J = matrix(1, 5, 5)
H = I - J / 5 
H
# -------------------------------------------

H <- function(n) {
  I = diag(n)
  J = matrix(1, n, n)
  H = I - J / n 
  H
}

H(5)
# ------------------------------------------------------------------------------------------

A = H(150)
colSums(eigen(A)$vectors)
# -----------------------------------------------------------------------

X = as.matrix(iris[,1:4])
X_ = H(150)%*%X
X_
# ---------------------------------------------------------------------------------

S = (t(X)%*%H(150)%*%X)/150 
S
# ----------------------------------------------------------------------------------------------------------------
varianza_generalizada = prod(eigen(S)$values)
varianza_generalizada

variacion_total = sum(eigen(S)$values)
variacion_total

coeficiente_dependencia = 1 - det(cor(X))
coeficiente_dependencia
# ----------------------------------------------------------------------------------------

pairs(iris)
# ----------------------------------------------------------------------------------

pairs(iris, col = iris$Species)
# -------------------------------------------------------------------------

hh = as.matrix(mtcars)
hh

xih = hh['Mazda RX4',]
xjh = hh['Datsun 710',]
dE = sqrt(sum((xih - xjh)^2))
dE
# -------------------------------------------------------------------------

xih = hh['Fiat 128',]
xjh = hh['Maserati Bora',]
shh<-diag(cov(hh))
dP = sqrt(sum((xih - xjh)^2/shh))
dP
# -------------------------------------------------------------------------

xi = hh['Duster 360',]
xj = hh['Honda Civic',]
S = cov(hh)
I = diag(1,ncol(hh))
dM = sqrt((matrix((xi - xj), nrow = 1, byrow = FALSE))%*%solve(S,I)%*%t(matrix((xi - xj), nrow = 1, byrow = FALSE)))
dM
# -------------------------------------------------------------------------

dist_euclidiana <- function(hh, n, m) {
  xih = hh[n,]
  xjh = hh[m,]
  dE = sqrt(sum((xih - xjh)^2))
  dE
}

dist_euclidiana(hh, 'Mazda RX4', 'Datsun 710')
# -------------------------------------------------------------------------

dist_pearson <- function(hh, n, m) {
  xih = hh[n,]
  xjh = hh[m,]
  shh<-diag(cov(hh))
  dP = sqrt(sum((xih - xjh)^2/shh))
  dP
}

dist_pearson(hh, 'Fiat 128', 'Maserati Bora')
# -------------------------------------------------------------------------

dist_mahalanobis <- function(hh, n, m) {
  xi = hh[n,]
  xj = hh[m,]
  S = cov(hh)
  I = diag(1,ncol(hh))
  dM = sqrt((matrix((xi - xj), nrow = 1, byrow = FALSE))%*%solve(S,I)%*%t(matrix((xi - xj), nrow = 1, byrow = FALSE)))
  dM
}

dist_mahalanobis(hh, 'Duster 360', 'Honda Civic')
# -------------------------------------------------------------------------

for (i in 1:5) {
  item = dist_mahalanobis(hh, 1, i)
  print(item)
}
# -------------------------------------------------------------------------

A = matrix(c(1,3,2,2,0,1,4,5,6,3,2,1), nrow = 4, byrow = TRUE)
A
D = svd(A)$d
U = svd(A)$u
V = svd(A)$v

D[3] = 0 
A_asterisco = U%*%diag(D)%*%t(V)
A_asterisco
--------------------------------------------------------------------------------------
alcornoques

X_ = colMeans(alcornoques)
X_

S = (t(alcornoques)%*%H(nrow(alcornoques))%*%alcornoques)/nrow(alcornoques)
S

R = cor(alcornoques)
R

pairs(alcornoques, panel=panel.smooth, diag.panel = panel.hist, cex.labels = 1, font.labels=1)

alcornoques

y1 = alcornoques%*%c(1,-1,1,-1)
y1_media = mean(y1)
y1_media
y1_varianza = (t(y1)%*%H(nrow(y1))%*%y1)/nrow(y1)
y1_varianza

y2 = alcornoques%*%c(1,0,-1,0)
y2_media = mean(y2)
y2_media
y2_varianza = (t(y2)%*%H(nrow(y2))%*%y2)/nrow(y2)
y2_varianza

y3 = alcornoques%*%c(0,1,0,-1)
y3_media = mean(y3)
y3_media
y3_varianza = (t(y3)%*%H(nrow(y3))%*%y3)/nrow(y3)
y3_varianza

Z1 = y1/2
Z1
Z1_media = mean(Z1)
Z1_media
Z1_varianza = (t(Z1)%*%H(nrow(Z1))%*%Z1)/nrow(Z1)
Z1_varianza

Z2 = y2/sqrt(2)
Z2
Z2_media = mean(Z2)
Z2_media
Z2_varianza = (t(Z2)%*%H(nrow(Z2))%*%Z2)/nrow(Z2)
Z2_varianza

Z3 = y3/sqrt(2)
Z3
Z3_media = mean(Z3)
Z3_media
Z3_varianza = (t(Z3)%*%H(nrow(Z3))%*%Z3)/nrow(Z3)
Z3_varianza


#Ejemplo 2
familias

x <- familias[1:4,1:2]
y <- familias[1:4,3:4]
x2 = familias[,1:2]
y2 = familias[,3:4]

H <- function(mat) {
  diag(nrow(mat)) - matrix(1,nrow(mat),nrow(mat))/nrow(mat)
}

CC <- function(mat) {
  H(mat)%*%mat
}

tr <- function(mat) {
  sum(diag(mat))
}

b <- function(x, y) {
  xc <- CC(x)
  yc <- CC(y)
  M <-sqrt(tr(t(xc)%*%yc%*%t(yc)%*%xc))
  P <- tr(t(xc)%*%xc)
  return(M/P)
}

TT <- function(x, y) {
  xc <- CC(x)
  yc <- CC(y)
  fs <- svd(t(xc)%*%yc)
  return(fs$u%*%t(fs$v))
}

c <- function(x, y) {
  colMeans(y) - b(x,y) * colMeans(x)%*%TT(x,y)
}

y_est <- b(x,y)*x%*%TT(x,y)+matrix(1,nrow(x),1)%*%c(x,y)
cbind(y,y_est)

