#Truncamos una N(40,10) donde c=inf(15,inf)
set.seed(1)


rnormal = function(n, mu, sigma){
  u1 = runif(n/2)
  u2 = runif(n/2)
  x = sqrt(-2*log(u2))*cos(2*pi*u1)
  y = sqrt(-2*log(u2))*sin(2*pi*u1)
  muestra = c(x,y)
  muestra2 = muestra*sigma+mu
  return(muestra2)
}

rnormal_truncada = function(n, mu, sigma){
  muestrat = c()
  if (n%%2 == 1){
    while (length(muestrat) == 0){
    u1 = runif(n)
    u2 = runif(n)
    x = sqrt(-2*log(u2))*cos(2*pi*u1)
    muestra = x
    muestra2 = muestra*sigma+mu
    muestrat=muestra2[muestra2>15]
    }
  }
  else{
    while (length(muetrat) == 0){
    u1 = runif(n/2)
    u2 = runif(n/2)
    x = sqrt(-2*log(u2))*cos(2*pi*u1)
    y = sqrt(-2*log(u2))*sin(2*pi*u1)
    muestra = c(x,y)
    muestra2 = muestra*sigma+mu
    muestrat=muestra2[muestra2>15]
    }
  }
  return(muestrat[n])
}


dnormal = function(x,mu,sigma){
  (1 / (sigma * sqrt(2 * pi))) * exp(-(1/2)*((x - mu) / sigma)^2)
}

muestra=rnormal(1000,40,10)
hist(muestra,freq=FALSE)

#Truncamos en C=(15,Inf)
muestrat=muestra[muestra>15]

par(mfrow=c(1,2))
hist(muestrat,freq=FALSE)
curve(dnormal(x,40,10)/(1-pnorm(15,40,10))*(x>15),add=TRUE,col="red",lwd=2)

plot(ecdf(muestrat))
curve(pnorm(x,mean=40, sd=10), add=TRUE, col='red', lwd=2)

pnormt=function(x){
  (pnorm(x,40,10)-pnorm(15,40,10))/(1-pnorm(15,40,10))*(x>15)
}

ks.test(muestrat,pnormt) #vemos que okey

#########MONTE CARLO#############
#Aplicamos Montecarlo para n =100
set.seed(1)
i = 1
N = 1000
pvalor=numeric(N)
n = 100
mu = 40
sigma = 10


while(i<=N){
  muestra = rnormal(n, mu, sigma)
  muestrat=muestra[muestra>15]
  
  k = ks.test(muestrat, pnormt)
  pvalor[i] = k$p.value
  i = i+1
}
pvalor[pvalor>=0.05] = NA
p=which(is.na(pvalor))
1-(length(p)/N)


ks.test(muestrat,pnormt) #vemos que okey