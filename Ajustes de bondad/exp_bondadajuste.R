random_exp = function(n, lambda){
  u = runif(n)
  muestra = -log(1-u)/lambda
  return(muestra)
}

set.seed(1)
n = 100
lambda1 = 2
lambda2 = 3

muestra1 = random_exp(n,lambda1)
muestra2 = random_exp(n,lambda2)

par(mfrow=c(1,2))
hist(muestra1, freq=FALSE)
curve(dexp(x,2), add=TRUE, col='red', lwd=2)
hist(muestra2, freq=FALSE)
curve(dexp(x,3), add=TRUE, col='red', lwd=2)

par(mfrow=c(1,2))
plot(ecdf(muestra1))
curve(pexp(x,2), add=TRUE, col='red', lwd=2)
plot(ecdf(muestra2))
curve(pexp(x,3), add=TRUE, col='red', lwd=2)

ks.test(muestra1, pexp, rate=2)
ks.test(muestra2, pexp, rate=3)

#MONTECARLO

c = 0
MAX = 1000
cont1 = 0
cont2 = 0
while (c<MAX){
  muestra1 = random_exp(n,lambda1)
  muestra2 = random_exp(n,lambda2)
  
  aux1 = ks.test(muestra1, pexp, rate=2)$p.value
  aux2 = ks.test(muestra2, pexp, rate=3)$p.value
  
  cont1 = ifelse(aux1<0.05, cont1+1, cont1)
  cont2 = ifelse(aux2<0.05, cont2+1, cont2)
  
  c = c + 1
}

cont1/MAX
cont2/MAX
