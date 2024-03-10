# Media -------------------------------------------------------------------
  media<-function(x){
  n<-length(x)
  media<-sum(x)/n
  return(media)
}


# Varianza, desviación y covarianza ---------------------------------------

varianza<-function(x){
  n<-length(x)
  varianza<-(sum((x-media(x))^2))/(n)
  return(varianza)
}

esta.desv<-function(x){
  sqrt(varianza(x))
}

covarianza<-function(x,y){
  nx<-length(x)
  ny<-length(y)
  if(nx!=ny){
    return("La longitud de x y de y es distinta")
  }else{if (anyNA(x) || anyNA(y))
    {return("Hay algún NA en x o en y")}
    covarianza<-(sum((x-media(x))*(y-media(y))))/(nx)
  return(covarianza)
    }
}




# Correlación de pearson --------------------------------------------------

corr.pearson<-function(x,y){
  corr.pearson<-covarianza(x,y)/(esta.desv(x)*esta.desv(y))
  return(corr.pearson)
}




# Asimetria, Kurtosis y sus derivados -------------------------------------

asimetria<-function(x){
  n<-length(x)
  asimetria<-(sum((x-media(x))^3))/n
  return(asimetria)
}

coef.asimetria<-function(x){
  coef.asimetria<-asimetria(x)/(esta.desv(x)^3)
  return(coef.asimetria)
}

curtosis<-function(x){
  n<-length(x)
  curtosis<-(sum((x-media(x))^4))/n
  return(curtosis)
}

coef.curtosis<-function(x){
  coef.curtosis<-curtosis(x)/(esta.desv(x)^4)
  return(coef.curtosis)
}




# Estadística descriptiva -------------------------------------------------

cuartiles<-function(x){
  #Calcular el minimo y los 4 cuartiles
  temporal<-quantile(x,prob=c(0,0.25,0.5,0.75,1), na.rm=TRUE)
  return(temporal)
}

festdes <- function(lista){
  #Esta función de estadística descriptiva junta varias medida de estadistica descriptiva sobre un objeto, entre ellas cantidad de datos, media, varianza, desviación estándar y los cuartiles
  #Para funcionar necesita una lista previamente creada que este conformada por data frames individuales que sean cada una un vector de datos (es más fácil crear el data frame con todos los datos, extraer cada columna como df$columna y luego juntarlos en una lista)
  n <- length(lista)
  festdes <- data.frame(matrix(NA, nrow = n, ncol = 9))
  colnames(festdes) <- c("Obs", "Mean", "Var", "SD", "Min(0%)", "Q1(25%)", "Med(50%)", "Q3(75%)", "Max(100%)")

  for (i in 1:n) {
    x <- na.omit(lista[[i]])
    festdes[i, 1] <- length(x)
    festdes[i, 2] <- mean(x)
    festdes[i, 3] <- var(x)
    festdes[i, 4] <- sd(x)
    festdes[i, 5:9] <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1))
  }
  rownames(festdes) <- names(lista)
  return(festdes)
}

# OLS matricial y estimación ----------------------------------------------

##La función de OLS matricial necesita como input la matriz de diseño de x, por lo que hay que programarla antes

ols.matr<-function(xdiseno,y){
  x<-xdiseno
  #Esta función necesita de inputs la matriz de diseño y el vector de variable explicada
  betas<-solve(t(x)%*%x)%*%t(x)%*%y
  return(betas)
}

estimacion<-function(xdiseno,y){
  #Calcular el número de variables explicativas como el número de columnas de la matriz de diseño quitandole 1 para no tener en cuenta el vector de 1 que pertenece al intercepto
  x<-xdiseno
  k<-ncol(x)
  n<-nrow(x)
  #Calcular los regresores usando la función ols.matr programada arriba
  betas<-ols.matr(x,y)
  #Calcular las predicciones y los residuales estimados, además de estadística descriptiva alrededor de los errores estimados (R2, R2 ajustado, media, entre otros)
  yest<-x%*%betas
  resid.est<-y-yest
    STC<-t(y)%*%y
    SEC<-t(yest)%*%yest
    SRC<-t(resid.est)%*%resid.est
    media.resid.est<-media(resid.est)
    R2<-1-SRC/STC
    R2ajust<-1-(1-R2)*(n-1)/(n-k-1)
    df.esti<-data.frame(cbind(STC,SEC,SRC,media.resid.est,R2,R2ajust))
    names(df.esti)<-c("STC","SEC","SRC","media.resid.est","R2","R2ajust")
  return(df.esti)
}

varcove<-function(xdiseno, y){
  beta<-ols.matr(xdiseno=xdiseno,y=y)
  e<-y-xdiseno%*%beta
  varcove<-e%*%t(e)
  return(varcove)
}

varcovb<-function(xdiseno,y){
  x<-xdiseno
  beta<-ols.matr(xdiseno=xdiseno, y=y)
  e<-y-xdiseno%*%beta
  N<-nrow(e)
  K<-ncol(xdiseno)
  sigma2<-as.numeric((t(e)%*%e)/(N-K))
  varcovb<-sigma2*solve(t(x)%*%x)
  return(varcovb)
}

SRC<-function(xdiseno,y){
  x<-xdiseno
  betas<-ols.matr(xdiseno=x,y=y)
  yest<-x%*%betas
  resid.est<-y-yest
  SRC<-sum(resid.est^2)
  return(SRC)
}

STC<-function(y){
  STC<-t(y)%*%y
  return(STC)
}

SEC<-function(xdiseno,y){
  x<-xdiseno
  betas<-ols.matr(x,y)
  yest<-x%*%betas
  SEC<-t(yest)%*%yest
  return(SEC)
}

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

R2ajust<-function(xdiseno,y){
    x<-xdiseno
    k<-ncol(x)
    n<-nrow(x)
    betas<-ols.matr(x,y)
    yest<-x%*%betas
    resid.est<-y-yest
    STC<-t(y)%*%y
    SEC<-t(yest)%*%yest
    SRC<-t(resid.est)%*%resid.est
    R2<-1-SRC/STC
    R2ajust<-1-(1-R2)*(n-1)/(n-k-1)
    Return(R2ajust)
}



# Jarque bera tradicional y versión solo para errores ---------------------

jarque.bera<-function(x){
  n<-length(x) 
  JB<-(n/6)*((coef.asimetria(x)^2)+0.25*(coef.curtosis(x)-3)^2)
  pvalJB<-pchisq(q=JB,df=2,lower.tail=FALSE)
  df.jarque<-data.frame(cbind(JB,pvalJB))
  names(df.jarque)<-c("Estadístico Jarque-Bera","P-Value")
  return(df.jarque)
}

jarque.bera.err<-function(e){
  #Este código tiene los 4 primeros momentos programados sin restarle la media, esto porque se supone media es 0 para los errores, claramente va a diferir del jarque Ber tradicional
  x<-e
  n<-length(x)
  media.x<-sum(x)/n
  var.x<-sum(x^2)/n
  asim.x<-sum(x^3)/n
  kurt.x<-sum(x^4)/n
  desv.x<-var.x^.5
  coef.asim.x<-asim.x/desv.x^3
  coef.kurt.x<-kurt.x/desv.x^4
  ex.kurt.x<-coef.kurt.x-3
  
  jb.x<-n*(coef.asim.x^2/6+(ex.kurt.x^2)/24)
  pvaljb.x<-pchisq(q=jb.x,df=2,lower.tail=FALSE)
  df.jarque<-data.frame(cbind(jb.x,pvaljb.x))
  names(df.jarque)<-c("Estadístico Jarque-Bera","P-Value")
  return(df.jarque)
}



# Inferencia --------------------------------------------------------------

##En esta sección calcularemos todo lo necesario para hacer inferencia estadística y pruebas de hipótesis

var.est<-function(xdiseno,y){#Se calcula la varianza estimada a partir del estimador insesgado como la suma de los errores cuadrados entre el número de observaciones menos el número de regresores (es decir ajustado por grados de libertad)
    x<-xdiseno
    k<-ncol(x)
    n<-nrow(x)
    betas<-ols.matr(x,y)
    yest<-x%*%betas
    resid.est<-y-yest
    var.est<-(t(resid.est)%*%resid.est)/(n-k)
    return(var.est)
}

varcov.gorro<-function(xdiseno,y){#Se estima la matriz de Varianza-Covarianza de los regresores, a partir de la multiplicación de la varianza estimada y la inversa de la matriz t(x) por x
    x<-xdiseno
    k<-ncol(x)
    n<-nrow(x)
    betas<-ols.matr(x,y)
    yest<-x%*%betas
    resid.est<-y-yest
    sigma2.gorro<-as.numeric(var.est(xdiseno=x,y=y))
    varcov<-sigma2.gorro*solve(t(x)%*%x)
    return(varcov)
}

beta.estrella<-function(xdiseno,y, R, r){
  x<-xdiseno
  beta.mono<-ols.matr(x=x,y=y)
  monstruo<-solve(t(x)%*%x)%*%
    (t(R))%*%
    solve(R%*%solve(t(x)%*%x)%*%t(R))%*%
    (r-R%*%beta.mono)
  estrella<-beta.mono+monstruo
  return(estrella)
}

test.lagrange<-function(xdiseno,y, R, r){
  x<-xdiseno
  j<-nrow(R)
  beta.mono<-ols.matr(xdiseno=x, y=y)
  var.est<-as.numeric(var.est(xdiseno=x,y=y))
  lambda1<-(t((R%*%beta.mono-r))%*%
    solve(R%*%solve(t(x)%*%x)%*%t(R))
    %*%(R%*%beta.mono-r))/(j*var.est)
  return(lambda1)
}

test.raz.vero<-function(xdiseno,y,R, r){
  #Usa la diferencia entre la suma de los residuales cuadrados restringido y no restringidos
  x<-xdiseno
  j<-nrow(R)
  beta.mono<-ols.matr(xdiseno=x,y=y)
  beta.estrella<-beta.estrella(xdiseno=x, y=y, R=R, r=r)
  SRC.restr<-t(y-x%*%beta.estrella)%*%(y-x%*%beta.estrella)
  SRC.norestr<-t(y-x%*%beta.mono)%*%(y-x%*%beta.mono)
  var.est<-as.numeric(var.est(xdiseno=x,y=y))
  lambda2<-(SRC.restr-SRC.norestr)/(j*var.est)
  return(lambda2)
}

test.wald<-function(xdiseno,y,R, r){
  #Parte de que tanto se parece beta estrella a beta moño
  x<-xdiseno
  j<-nrow(R)
  beta.mono<-ols.matr(xdiseno=x,y=y)
  beta.estrella<-beta.estrella(xdiseno=x, y=y, R=R, r=r)
  var.est<-as.numeric(var.est(xdiseno=x,y=y))
  resta<-beta.estrella-beta.mono
  lambda3<-(t(resta)%*%t(x)%*%(x)%*%(resta))/(j*var.est)
  return(lambda3)
}




# Detección y corrección de heterocedasticidad -----------------------------------------

difvar.test<-function(xdiseno, y){
  K<-ncol(xdiseno)#Calcular el número de variables explicativas
  betas<-ols.matr(xdiseno=xdiseno, y=y)#Calcular los coeficientes estimados por OLS
  e<-y-xdiseno%*%betas #Calcular los errores estimados
  N<-length(e) #Calcular el largo total del vector de errores estimados
  #Calcular la posición del cuantil 1 y del cuantil 2
  N1<-round(N/4,digits=0)
  N2<-round(3*N/4,digits=0)
  prueba<-as.data.frame(matrix(NA, ncol=5, nrow=N2-N1))
  colnames(prueba)<-c("position", "var1", "var2","stat","pvalue")
   for(i in N1:N2){
    e1<-e[1:i,] #Tomar los errores desde la posición 1 hasta la i
    e2<-e[i:N,] #Tomar los errores desde la posición i hasta N
    #Calcular los grados de libertad de e1 y e2 co mo la cnatidad de datos en cada uno menos las variables explicativas del modelo
    gl1<-length(e1)-K
    gl2<-length(e2)-K
    #Calcular la varianza de cada grupo de errores
    var1<-t(e1)%*%e1/length(e1)
    var2<-t(e2)%*%e2/length(e2)
    #Calcular el estadístico de diferencia de varianzas
    stat<-var1/var2
    #Calcular el pvalue proveniente de una f de fischer con gl1 en el numerador y gl2 en el denominador
    pvalue<-pf(q=stat, df1=gl1, df2=gl2)
    #Llenar el dataframe de prueba con la posición de quiebre, el estadistico de prueba y el p-value asociado
    prueba[i-N1+1,1]<-i
    prueba[i-N1+1,2]<-var1
    prueba[i-N1+1,3]<-var2
    prueba[i-N1+1,4]<-stat
    prueba[i-N1+1,5]<-pvalue
  }
  maxstat<-max(prueba$stat)#Calcular el estadistico máximo como el de "Mayor rechazo"
  filamin<-subset(prueba, stat==maxstat)#Tomar la fila que tiene el mayor estadístico de prueba y retornarla como resultado
  return(filamin)
}

white.test <- function(xdiseno, y) {
  betas <- ols.matr(xdiseno, y) #Encontrar los coeficientes originales de la regresión por OLS
  
  e <- y - xdiseno %*% betas#Calcular los errores estimados
  
  e.2 <- e * e #Errores estimados al cuadrado
  
  df <- data.frame(data = rep(NA, length(e))) #Armar un dataframe vacio que vaya almacenando los datos
  
  for (i in 2:ncol(xdiseno)) { #El for va desde 2 para no tener en cuenta el intercepto de la matriz de diseño
    
    nombre <- paste0("x", i) #Armar un nombre concatenando x con la posición de iteración i
    
    variable <- xdiseno[, i] #Tomar cada columna de la matriz de diseño según la iteración i
  
      df[, nombre] <- variable #Poner las columnas de la matriz de diseño dentro del data frame vacío creado con anterioridad
  }
  
  df.sq <- select(df, -data) #Crear un nuevo data frame (df.sq), que sea el data frame luego de ponerle las columnas del for pero quitando la columna llamada data que sale por error y solo está llena de NA
  
  for (i in 1:ncol(df.sq)) { #Iterar el for en las columnas de df.sq
    
    nombre2 <- paste0("x", i+1, ".sq") #En cada iteración llamar una nueva variable que sea la anterior al cuadrado (i.e.: x2.sq)
    
    df.sq <- mutate(df.sq, !!nombre2 := df.sq[, i] ^ 2) #Usar la función mutar de dplyr para agregar una nueva columna al df.sq cada vez que i cambie y donde cada nueva columna xi.sq sea xi elevada al cuadrado (como lo dicta el test de white)
  }
  df.cr<-df.sq #Poner un data frame llamado df.cr (crossed), que sea igual al df.sq (square)
  
  nvar<-ncol(df.sq)/2+1 #Calcular el número de variables como la mitad de las columnas del df.sq +1 (teniendo en cuenta que en df.sq hay el doble de variables que en df y se le suma 1 para tener en cuenta que comenzamos desde 2)
  
  comb<-combn(2:nvar,m=2) #Calcular las combinaciones posibles desde 2 (columna donde inicia por que le quitamos el intercepto) hasta el número de variables calculadas arriba, tomando solo 2 variables al tiempo cada vez. Esto devuelve una matriz de tamaño m*combinaciones
  
  for (i in 1:ncol(comb)) {#Iterar el for desde 1 hasta el número de columas de toda la combinatoria
    
    #Tomar la fila 1 y 2 de la matriz de combinatorias iterando por el i, teniendo en cuenta que esta matriz siempre va a hacer las combinatorias desde 2 para no tener en cuenta el intercepto
    col1 <- comb[1, i]
    col2 <- comb[2, i]
    
    comb.name<- paste0("x", col1, ".x", col2)#Agregar un nuevo nombre que sea el producto de cada par de variables, i.e.: x2.x3
    
    df.cr <- mutate(df.cr, !!comb.name := df.cr[,col1-1]* df.cr[,col2-1]) #Asignar dentro del data frame cruzado df.cr una nueva columna a través de la función mutate que sea el producto de cada par de columnas seleccionadas a partir de la combinatoria, se le quita 1 para tener en cuenta el 1 que le sumamos al comienzo al empezar a colocar las columnas así, cuando el for caiga por ejemplo en i=1, tomará el par 2:3 pero se calculará el producto de las columnas 1 y 2 (que si es el producto de x2*x3), sino le ponemos el -1, queda como columnas 2*3 (que es x3*x4) que no es lo correcto
  }
  mat <- cbind(1,as.matrix(df.cr)) #Se convierte el data frame cruzado en una matriz de diseño para la regresión auxiliar
  
  R2<-R2(xdiseno=mat, y=e.2) #Se calcula el R2 de la regresión auxiliar teniendo variable dependiente los errores al cuadrado y como variable independiente la matriz de diseño auxiliar (que contiene 1, cada variable, cada variable al cuadrado y cada posible combinación del par de variables)
  
  K<-ncol(df)# Calcular el número de variables explicativas de la regresión auxiliar
  
  N<-nrow(mat) #Calcular el número de observaciones 
  
  stat<-N*R2
    #(R2/K)/((1-R2)/(N-K-1)) Calcular el estadístico de prueba
  
  pvalue<-pchisq(q=stat, df=K) #Calcular el p-value asociado
  
  info<-data.frame(cbind(stat,pvalue)) #Armar un data frame que contenga el estadístico de prueba y el pvalue asociado
  
  colnames(info)<-cbind("stat","pvalue") #Ponerle nombres al data frame
  
  print(info)
  
  return(info) #Devolver el data frame con los resultados
  }

pcholesky.het<-function(xdiseno,y){
  lista<-difvar.test(xdiseno=xdiseno, y=y)
  posi<-as.numeric(lista$position)
  var1<-lista$var1
  var2<-lista$var2
  N<-length(y)
  P<-matrix(data=0, nrow=N, ncol=N)
  for (i in 1: posi){
    P[i,i]<-var1
  }
  for (j in (posi+1):N){
    P[j,j]<-var2
  }
  return(P)
}

# Detección y corrección dede autocorrelación --------------------------------------------
durbinwatson.test<-function(xdiseno,y){
  betas<-ols.matr(xdiseno=xdiseno,y=y)
  e<-y-xdiseno%*%betas
  N<-length(e)
  et<-e[2:N]
  et1<-e[1:N-1]
  rho<-(t(et1)%*%et)/(t(et1)%*%et1)
  dw<-2*(1-rho)
  return(dw)
}

cholesky.auto1<-function(xdiseno,y){#Los argumentos de esta función son la matriz de diseño y el vector de variables explicativas.
  
  betas<-ols.matr(xdiseno=xdiseno, y=y) #Hallar los coeficientes estimados por OLS de la regresión original
  
  e<-y-xdiseno%*%betas #Hallar los errores estimados de la regresión original
  
  N<-nrow(e) #Calcular el número de observaciones
  
  #Tomar el vector de los errores estimados los et como variable explicada y los et-1 como variable explicativa de la regresión auxiliar (et= rho*et-1 +eta)
  et<-e[2:N] 
  et1<-e[1:N-1] 
  
  rho<-ols.matr(xdiseno=et1, y=et) #Hallar el coeficiente rho de la regresión auxiliar
  
  P<-matrix(0,ncol=N, nrow=N) #Crear la matriz P de cholesky vacía
  for (i in 1:N){
    P[1,1]<-sqrt(1-rho^2) #En la primera posición colocar (1-rho^2)^.5
    
    if(i!=1){P[i,i]<-1  #En el resto de posiciones de la diagonal distintas a [1,1], colocar 1
    }
    }
  for(i in 2:N){ #En las posiciones debajo y a la izquierda de la diagonal colocar -rho
    P[i,i-1]<- -rho
  }
  return(P) #Devolver la matriz P de cholesky
}


