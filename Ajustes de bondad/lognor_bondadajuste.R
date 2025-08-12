#Primero calculamos el mu y el sigma calculados por los momentos
desv=1.5
med=3


sigma = sqrt( log(1+desv^2/med^2) )
mu = (log(med^2)-sigma^2)/2

#Ahora calculamos una Normal(mu,sigma) por Box-Muller

set.seed(1)
n = 100

u1 = runif(n)
u2 = runif(n)

xN01 = sqrt(-2*log(u2))*sin(2*pi*u1) #N(0,1)
xN = mu + sigma*xN01 #N(mu,sigma)

ks.test(xN,pnorm,mean=mu,sd=sigma)

#Para calcular la LogNormal hacemos la exponencial de la Normal

muestra = exp(xN)

hist(muestra,freq=FALSE)
curve(dlnorm(x,meanlog=mu,sdlog=sigma), add=TRUE, col='red',lwd=2)

plot(ecdf(muestra))
curve(plnorm(x,meanlog=mu,sdlog=sigma), add=TRUE, col='red',lwd=2)

ks.test(muestra, plnorm, meanlog=mu, sdlog=sigma)

#Ahora veamos en mas simulaciones

c = 0
MAX = 1000
cont = 0
while (c<MAX){
  u1 = runif(n)
  u2 = runif(n)
  nor01 = sqrt(-2*log(u2))*sin(2*pi*u1)
  nor = mu + sigma*nor01
  
  muestra = exp(nor)
  
  aux = ks.test(muestra, plnorm, meanlog=mu, sdlog=sigma)$p.value
  
  cont = ifelse(aux<0.05, cont+1, cont)
  
  c = c + 1
}

cont/MAX


rlog_normal = function(n, mu, sigma){
  if (n%%2 == 1){
    u1 = runif(n)
    u2 = runif(n)
    x = sqrt(-2*log(u2))*sin(2*pi*u1)
    muestra2 = x*sigma+mu
    muestra = exp(muestra2)
    
  }else{
    u1 = runif(n/2)
    u2 = runif(n/2)
    x = sqrt(-2*log(u2))*cos(2*pi*u1)
    y = sqrt(-2*log(u2))*sin(2*pi*u1)
    z = c(x,y)
    muestra2 = z*sigma+mu
    muestra = exp(muestra2)
  }
  return (muestra)
}