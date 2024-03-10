library(readxl)

#leer excel
penn <- read_xlsx(file.choose())

#definir variables
ycol <- as.matrix(penn$rgdpcol)
lcol <- as.matrix(penn$empcol)
kcol <- as.matrix(penn$cscol)

#matriz diseño
xdiseno <- cbind(1,log(kcol),log(lcol))
N <- as.numeric(nrow(xdiseno))
k <- as.numeric(ncol(xdiseno))

#beta ols
beta_ols <- solve(t(xdiseno)%*%xdiseno)%*%t(xdiseno)%*%log(ycol)

#Y estimado
y_est <- xdiseno%*%beta_ols
plot(log(ycol))
lines(y_est)

#error estimado
e_est <- log(ycol) - y_est
mean(e_est)

#probar normalidad - jb
#jarque-bera
j_b <- function(x){
  N <- length(x)
  varianza <- sum(x^2)/N
  sd <- sqrt(varianza)
  asimetria <- sum(x^3)/N
  curtosis <- sum(x^4)/N
  simetria <- asimetria/sd^3
  exceso_curtosis <- (curtosis-3*sd^4)/(sd^4)
  jb <- N*((simetria^2/6)+(exceso_curtosis^2/24))
}

jb <- j_b(e_est)

#varianza de las perturbaciones - sigma^2 estimado
sigma2 <- as.numeric((t(e_est)%*%e_est)/(N-k))

#varcov del beta estimado
varcov <- sigma2*solve(t(xdiseno)%*%xdiseno)

#sd de la diagonal de la varcov
sd_diag_varcov <- sqrt(diag(varcov))

#ph
#Ho = B1+B2=0
R <- t(as.matrix(c(0,1,1)))
r <- as.matrix(c(1))
j <- nrow(R)

#beta restringido
beta_estrella <- beta_ols+inv%*%t(R)%*%solve(R%*%inv%*%t(R))%*%(r-R%*%beta_ols)

#lambda 1 sin función

inv <- solve(t(xdiseno)%*%xdiseno)
numerador <- as.numeric(t(R%*%beta_ols-r)%*%solve(R%*%inv%*%t(R))%*%(R%*%beta_ols-r))
denominador <- sigma2*j
lambda1 <- numerador/denominador

#lambda 1 - función

lambda_1 <- function(x, R, r, y) {
  x = xdiseno
  N <- as.numeric(nrow(x))
  k <- as.numeric(ncol(xdiseno))
  j <- nrow(R)
  beta_ols <- solve(t(xdiseno)%*%xdiseno)%*%t(xdiseno)%*%y
  y_est <- xdiseno%*%beta_ols
  e_est <- y - y_est
  sigma2 <- as.numeric((t(e_est)%*%e_est)/(N-k))
  inv <- solve(t(xdiseno)%*%xdiseno)
  numerador <- as.numeric(t(R%*%beta_ols-r)%*%solve(R%*%inv%*%t(R))%*%(R%*%beta_ols-r))
  denominador <- sigma2*j
  lambda1 <- numerador/denominador
}

lmda1 <- lambda_1(x=xdiseno,R=R, r=r, y=log(ycol))

#lambda 2 - sin función
#y* = y - XB^~
e_est_estrella <- log(ycol) - xdiseno%*%beta_estrella
numeradorL2 <- as.numeric(t(e_est_estrella)%*%e_est_estrella-t(e_est)%*%e_est)
denominadorL2 <- sigma2*j
lambda2 <- numeradorL2/denominadorL2

#lambda 2 - función
lambda_2 <- function(x, R, r, y) {
  x = xdiseno
  beta_ols <- solve(t(xdiseno)%*%xdiseno)%*%t(xdiseno)%*%y
  beta_estrella <- beta_ols+inv%*%t(R)%*%solve(R%*%inv%*%t(R))%*%(r-R%*%beta_ols)
  e_est_estrella <- log(ycol) - xdiseno%*%beta_estrella
  y_est <- xdiseno%*%beta_ols
  e_est <- y - y_est
  numeradorL2 <- as.numeric(t(e_est_estrella)%*%e_est_estrella-t(e_est)%*%e_est)
  j <- nrow(R)
  N <- as.numeric(nrow(x))
  k <- as.numeric(ncol(xdiseno))
  sigma2 <- as.numeric((t(e_est)%*%e_est)/(N-k))
  denominadorL2 <- sigma2*j
  lambda2 <- numeradorL2/denominadorL2
}

lmda2 <- lambda_2(x=xdiseno, R=R, r=r, y=log(ycol))

#lambda 3 - sin función
numeradorL3 <- t(beta_estrella-beta_ols)%*%t(xdiseno)%*%xdiseno%*%(beta_estrella-beta_ols)
denominadorL3 <- sigma2*j
lambda3 <- numeradorL3/denominadorL3

#lambda 3 - función
lambda_3 <- function(x, R, r, y) {
  x = xdiseno
  beta_ols <- solve(t(xdiseno)%*%xdiseno)%*%t(xdiseno)%*%y
  beta_estrella <- beta_ols+inv%*%t(R)%*%solve(R%*%inv%*%t(R))%*%(r-R%*%beta_ols)
  numeradorL3 <- t(beta_estrella-beta_ols)%*%t(xdiseno)%*%xdiseno%*%(beta_estrella-beta_ols)
  denominadorL3 <- sigma2*j
  lambda3 <- numeradorL3/denominadorL3
}

lmda3 <- lambda_3(x=xdiseno, R=R, r=r, y=log(ycol))
