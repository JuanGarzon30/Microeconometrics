#creación de la matriz psi
diag_psi <- c(0.34,0.45,0.56)

psi <- function (diag_psi,num_col) {
  matriz_psiId <- matrix(0,nrow=num_col,ncol=num_col)
  for (i in 1:num_col) {
    matriz_psiId[i,i] <- 1
  }
  matriz_psi <- diag_psi*matriz_psiId
  return(matriz_psi)
}

prueba_psi <- psi(diag_psi,3)

#creación matriz psi inversa
psi_inv <- function(matriz_psi,num_col) {
  matriz_psiId <- matrix(0,nrow=num_col,ncol=num_col)
  for (i in 1:num_col) {
    matriz_psiId[i,i] <- 1
  }
  matriz_psi_inv <- matriz_psiId/matriz_psi
  matriz_psi_inv[is.na(matriz_psi_inv)] <- 0
  return(matriz_psi_inv)
}

prueba_psi_inv <- psi_inv(prueba_psi,3)

#creación de la matriz p
p <- function(matriz_psi_inv, num_col) {
  matriz <- matrix(0,nrow=num_col,ncol=num_col)
  for (i in 1:num_col) {
    matriz[i,i] <- sqrt(matriz_psi_inv[i,i])
  }
  matriz_p <- matriz
  return(matriz_p)
}

prueba_p <- p(prueba_psi_inv,3)

#creación de y* - y estrella

y_estrella <- function(matriz_p,matriz_y) {
  if (dim(matriz_p)[2] == dim(matriz_y)[1]) {
    y <- matriz_p%*%matriz_y
  } else {
    print("La matriz p no es conformable con la matriz y")
  }
}

y <- matrix(c(1.26,3.5,2.22),nrow=3,ncol=1,byrow=TRUE)

prueba_y <- y_estrella(matriz_p = prueba_p, matriz_y = y)

#creación x* - x estrella

x_estrella <- function(matriz_p,xdiseno) {
  if (dim(matriz_p)[2] == dim(xdiseno)[1]) {
    x <- matriz_p%*%xdiseno
  } else {
    print("La matriz p no es conformable con la matriz de diseño")
  }
}

xdiseno <- matrix(c(1,7.33,0.29,1,4.33,1.78,1,9.5,8.4),nrow=3,ncol=3,byrow=TRUE)

prueba_x <- x_estrella(matriz_p = prueba_p, xdiseno = xdiseno)

#creación e* - e estrella

e_estrella <- function(matriz_p,matriz_e) {
  if (dim(matriz_p)[2] == dim(matriz_e)[1]) {
    e <- matriz_p%*%matriz_e
  } else {
    print("La matriz p no es conformable con la matriz e")
  }
}

e <- matrix(c(0.46,12.45,5.55),nrow=3,ncol=1,byrow=TRUE)

prueba_e <- e_estrella(matriz_p = prueba_p, matriz_e = e)

#creación beta doble moño - GLS

beta_doble_gorro <- function(x_estrella,y_estrella) {
  beta_2gorro <- solve(t(x_estrella)%*%x_estrella)%*%t(x_estrella)%*%y_estrella
}

prueba_beta.doble.gorro <- beta_doble_gorro(x_estrella = xdiseno, y_estrella = y)

#con psi_inv

beta_doble_gorro.psi <- function(psi_inv,xdiseno,matriz_y) {
  beta_2gorro <- solve(t(xdiseno)%*%psi_inv%*%xdiseno)%*%t(xdiseno)%*%psi_inv%*%matriz_y
}

prueba_beta.doble.gorro.psi <- beta_doble_gorro.psi(psi_inv = prueba_psi_inv,xdiseno = xdiseno, matriz_y = y)

#creación FGLS

N <- 1000
media <- 0
sd1 <- 10
sd2 <- 20

e1 <- as.matrix(rnorm(n=N/2,mean=media,sd=sd1))
e2 <- as.matrix(rnorm(n=N/2,mean=media,sd=sd2))
eT <- rbind(e1,e2)

g_eT <- plot(eT,ylab="Error",xlab="Observaciones", main="Simulación heterocedasticidad",col="blue")

xdiseno_fgls <- rf(N,2,100)

plot(xdiseno_fgls,eT^2) #no hay homocedasticidad

#Creación Breusch-Pagan

#creación de datos
n = 500
set.seed(40)
x1 <- rf(n=n,df1=1,df2=497)
x2 <- rbinom(n,10,0.4)
ei <- rnorm(n,0,x2+0.1)
y <- -3 + 2 * x1 + 4 * x2 + ei
m1<- cbind(y,x1,x2)
df <- data.frame(y,x1,x2)

#cd para probar
mod <- lm(y ~ x1 + x2, data=df)

#Hallar los coeficientes
ols.matr<-function(xdiseno,y){
  x<-xdiseno
  betas<-solve(t(x)%*%x)%*%t(x)%*%y
  return(betas)
}

xd_bp <- cbind(1,m1[,2],m1[,3])

betas <- ols.matr(xd_bp,m1[,1])

#error estimado

ei_est <- y - xd_bp%*%betas

#regresión para los errores 

beta_ei <- ols.matr(xd_bp,ei^2)

#R^2
#Actualizada
R2 <- function(xdiseno, y) {
  x <- xdiseno
  k <- ncol(x)
  n <- nrow(x)
  betas <- ols.matr(x, y)
  yest <- x %*% betas
  resid.est <- y - yest
  STC <- sum((y - media(y))^2)
  SRC <- sum(resid.est^2)
  R2 <- 1 - SRC / STC
  return(R2)
}

media<-function(x){
  n<-length(x)
  media<-sum(x)/n
  return(media)
}

r_cuadrado <- R2(xd_bp,ei^2)

#Número de variables

num_variables <- function(xdiseno) {
  k <- ncol(xd_bp)-1  
}

k <- num_variables(xdiseno_xd_bp)

estadistico_bp <- function(N,r2) {
 estadistico <- N*r2 
}

prueba_estadistico_bp <- estadistico_bp(N=n,r2=r_cuadrado)

pvalue_bp <- function(estadistico,num_variables) {
  pvalue <- pchisq(q=estadistico,df=num_variables,lower.tail=FALSE)
  cbind(estadistico,pvalue)
}

prueba_pvalue.bp <- pvalue_bp(estadistico=prueba_estadistico_bp,num_variables=k)

#comprobando mediante comando directo
library(lmtest)
bptest(mod)

