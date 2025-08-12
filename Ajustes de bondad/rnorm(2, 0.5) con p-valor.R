#Generamos 100 valores de la normal (2, 0.5) vía Box-Muller
set.seed(1)
i = 1
N = 1000
pvalor=numeric(N)
while(i<=N){
  u1 = runif(50)
  u2 = runif(50)
  x = sqrt(-2*log(u2))*cos(2*pi*u1)
  y = sqrt(-2*log(u2))*sin(2*pi*u1)
  muestra = c(x,y)
  mu = 2
  sigma = 0.5
  muestra2 = muestra*sigma+mu
  k = ks.test(muestra2, pnorm, 2, 0.5)
  pvalor[i] = k$p.value
  i = i+1
}
pvalor[pvalor>=0.05] = NA
p=which(is.na(pvalor))
length(p)/N

par(mfrow=c(1,1))
muestra = rnormal(50, 2, 0.5)
hist(muestra, freq=FALSE)
par(mfrow=c(1,1))
curve(dnorm(x,mean=2, sd=0.5), add=TRUE, col='red', lwd=2)
plot(ecdf(muestra))
curve(pnorm(x,mean=2, sd=0.5), add=TRUE, col='red', lwd=2)


rnormal = function(n, mu, sigma){
  if (n%%2 == 1){
    u1 = runif(n)
    u2 = runif(n)
    x = sqrt(-2*log(u2))*cos(2*pi*u1)
    muestra = x
    muestra2 = muestra*sigma+mu
  }else{
    u1 = runif(n/2)
    u2 = runif(n/2)
    x = sqrt(-2*log(u2))*cos(2*pi*u1)
    y = sqrt(-2*log(u2))*sin(2*pi*u1)
    muestra = c(x,y)
    muestra2 = muestra*sigma+mu
  }
  return(muestra2)
}