dtriang = function(x){
  (1/3<=x & x<=2/3)*(9*x-3) + (2/3<x & x<=1)*(9-9*x)
}

ptriang = function(x){
  (1/3<=x & x<=2/3)*((9/2)*x^2 -3*x + 1/2) + (2/3<x & x<=1)*((-9/2)*x^2 + 9*x - 7/2) + 1*(x>1)
}

rtriang = function(n){
  u = runif(n)
  muestra = ifelse(u<1/2, (1/3)*(1 + sqrt(2*u)), 1 - (1/3)*sqrt(2*(1-u)))
  return(muestra)
}

set.seed(1)
n = 100

muestra = rtriang(n)

hist(muestra, freq=FALSE)
curve(dtriang(x), add=TRUE, col='red', lwd=2)

plot(ecdf(muestra))
curve(ptriang(x), add=TRUE, col='red', lwd=2)

ks.test(muestra, ptriang)

#MONTECARLO
c = 0
MAX = 1000
cont = 0
while (c<MAX){
  muestra = rtriang(n)
  
  aux = ks.test(muestra, ptriang)$p.value
  
  cont = ifelse(aux<0.05, cont+1, cont)
  
  c = c + 1
}

cont/MAX