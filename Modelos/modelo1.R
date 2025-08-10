##############Distribuciones utilizadas##############

rtriang = function(n){
  u = runif(n)
  muestra = ifelse(u<1/2, (1/3)*(1 + sqrt(2*u)), 1 - (1/3)*sqrt(2*(1-u)))
  return(muestra)
}

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

r_exp = function(n, lambda){
  u = runif(n)
  muestra = -log(1-u)/lambda
  return(muestra)
}

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





##############Creacción de subrutinas##############

LlegadaMI = function(N11, N12, N13, N2, SUMA11, SUMA12, SUMA13, SUMA2, TM, TANT, TSMI,total_llegadas11){
  N11 = N11 + 1
  total_llegadas11=total_llegadas11+1
  
  if(N11 == 1){
    aux=-1
    while(aux<0){
      aux=rnormal(1,meanmic,sdmic)
    }
    DSMI = aux
    TSMI = TM + DSMI
  }
  
  SUMA11 = SUMA11 + (N11-1)*(TM-TANT)
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  SUMA2 = SUMA2 + N2*(TM-TANT)
  
  DLMI = r_exp(1,lambdamic)
  TLMI = TM + DLMI
  
  TANT = TM
  
  return(c(N11, SUMA11, SUMA12, SUMA13, SUMA2, TANT, TSMI, TLMI,total_llegadas11))
}


LlegadaCA = function(N11, N12, N13, N2, SUMA11, SUMA12, SUMA13, SUMA2, TM, TANT, TSME, TSBA,total_llegadas12,total_llegadas13){
  u = runif(1)
  pedido = 1*(u<=0.55) + 2*(u>0.55) #1 menú, 2 bocata
  
  if (pedido ==1){
    N12 = N12 + 1
    total_llegadas12=total_llegadas12+1
    
    if (N12 == 1){
      DSME = rtriang(1)
      TSME = TM + DSME
    }
    
    SUMA12 = SUMA12 + (N12-1)*(TM-TANT)
    SUMA13 = SUMA13 + N13*(TM-TANT)
  }
  else if (pedido ==2){
    N13 = N13 + 1
    total_llegadas13=total_llegadas13+1
    
    if (N13 == 1){
      DSBA = rlog_normal(1,mubar,sigmabar)
      TSBA = TM + DSBA
    }
    
    SUMA12 = SUMA12 + N12*(TM-TANT)
    SUMA13 = SUMA13 + (N13 - 1)*(TM-TANT)
  }
  
  DLCA = r_exp(1,lambdacaf)
  TLCA = TM + DLCA
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA2 = SUMA2 + N2*(TM-TANT)
  
  TANT = TM
  return(c(N12,N13, SUMA11, SUMA12, SUMA13, SUMA2, TANT, TSME, TSBA, TLCA,total_llegadas12,total_llegadas13))
}


ServicioMI = function(N11, N12, N13, N2, SUMA11, SUM12, SUMA13, SUMA2, TSCO, TM, TANT, Asientos,total_servicio11,total_llegadas2){
  N11 = N11-1
  N2 = N2 + 1
  total_servicio11=total_servicio11+1
  total_llegadas2=total_llegadas2+1
  
  aux=-1
  while(aux<0){
    aux=rnormal(1,meanmic,sdmic)
  }
  DSMI = aux
  TSMI = ifelse(N11 == 0, Inf, TM + DSMI)
  if(any(is.na(Asientos))){
    
    DSCO = rnormal_truncada(1, mucomida, sigmacomida)
    posiciones=which(is.na(Asientos))
    Asientos[posiciones[1]]=TM+DSCO
    TSCO=min(Asientos,na.rm = TRUE)
  }
  
  SUMA11 = SUMA11 + (N11+1)*(TM-TANT)
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  SUMA2 = SUMA2 + (N2-1)*(TM-TANT)
  
  
  TANT = TM
  return(list(N11,N2, SUMA11, SUMA12, SUMA13, SUMA2, TSCO, TSMI, TANT,Asientos,total_servicio11,total_llegadas2))
}


ServicioME = function(N11, N12, N13, N2, SUMA11, SUM12, SUMA13, SUMA2, TSCO, TM, TANT, Asientos,total_servicio12,total_llegadas2){
  N12 = N12-1
  N2 = N2 + 1
  total_servicio12=total_servicio12+1
  total_llegadas2=total_llegadas2+1
  
  DSME=rtriang(1)
  TSME = ifelse(N12 == 0, Inf, TM + DSME)
  
  if(any(is.na(Asientos))){
    
    
    DSCO = rnormal_truncada(1, mucomida, sigmacomida)
    posiciones=which(is.na(Asientos))
    Asientos[posiciones[1]]=TM+DSCO
    TSCO=min(Asientos,na.rm = TRUE)
  }
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA12 = SUMA12 + (N12+1)*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  SUMA2 = SUMA2 + (N2-1)*(TM-TANT)
  
  
  TANT = TM
  return(list(N12,N2, SUMA11, SUMA12, SUMA13, SUMA2, TSCO, TSME, TANT, Asientos,total_servicio12,total_llegadas2))
}


ServicioBA = function(N11, N12, N13, N2, SUMA11, SUM12, SUMA13, SUMA2, TSCO, TM, TANT, Asientos,total_servicio13,total_llegadas2){
  N13 = N13-1
  N2 = N2 + 1
  total_servicio13=total_servicio13+1
  total_llegadas2=total_llegadas2+1
  
  DSBA = rlog_normal(1,mubar,sigmabar)
  TSBA = ifelse(N13 == 0, Inf, TM + DSBA)
  
  if(N2 >= 1 & any(is.na(Asientos))){
    
    
    DSCO = rnormal_truncada(1, mucomida, sigmacomida)
    posiciones=which(is.na(Asientos))
    Asientos[posiciones[1]]=TM+DSCO
    TSCO=min(Asientos,na.rm = TRUE)
  }
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + (N13+1)*(TM-TANT)
  SUMA2 = SUMA2 + (N2-1)*(TM-TANT)
  
  
  TANT = TM
  
  return(list(N13,N2, SUMA11, SUMA12, SUMA13, SUMA2, TSCO, TSBA, TANT, Asientos,total_servicio13,total_llegadas2))
}


ServicioCO = function(N11, N12, N13, N2, SUMA11, SUMA12, SUMA13, SUMA2, TM, TANT, Asientos,total_servicio2){
  N2 = N2 - 1
  total_servicio2=total_servicio2+1
  
  Asientos[Asientos<=TM ]=NA
  
  if (length(which(is.na(Asientos)))>=1 & N2 - length(which(!is.na(Asientos)))>0){
    DSCO = rnormal_truncada(1, mucomida, sigmacomida)
    posiciones=which(is.na(Asientos))
    Asientos[posiciones[1]]=TM+DSCO
  }
  TSCO=min(Asientos,na.rm = TRUE)
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  SUMA2 = SUMA2 + (N2+1)*(TM-TANT)
  
  TANT = TM
  return(list(N2, SUMA11, SUMA12, SUMA13, SUMA2, TSCO, TANT, Asientos,total_servicio2))
}


##############NOTACIÓN##############

#           Etapas
#   1.
#     1.1 Servicio microondas
#     1.2 Servicio menú
#     1.3 Servicio barra

#   2. Comedor
   
#En este problema tendremos en cuenta el aforo de la cafeteria
#Consideraremos: 1 microondas que no se estropea, 1 camarero y no tardamos 
#tiempo en sentarnos.

#N11 = Número de personas en la etapa 1.1
#N12 = Número de personas en la etapa 1.2
#N13 = Número de personas en la la etapa 1.3
#N2 = Número de personas en la la etapa 2

#TM = instante en el tiempo de simulación
#TANT = instante anterior
#Tmax = tiempo máximo de simulación

#DLMI = Distribución de tiempos de llegada al microondas
#DLCA = Distribución de tiempos de llegada a la cafetería

#DSMI = Distribución tiempos de servicio del microondas
#DSME = Distribución tiempos de servicio del menú
#DSBA = Distribución tiempos de servicio de la barra
#DSCO = Distribución tiempos de servicio del comedor

#TLMI = Instante de la próxima llegada al microondas
#TLCA = Instante de la próxima llegada a la cafetería

#TSMI = Instante de la próxima finalización del servicio del microondas
#TSME = Instante de la próxima finalización del servicio del menú
#TSBA = Instante de la próxima finalización del servicio de la barra
#TSCO = Instante de la próxima finalización del servicio del comedor

#Asientos = Vector de asientos que indica si está vacio el asiento y 
#           si no el próximo fin de comida de ese asiento

#SUMA11 = Contador suma acumulada etapa 1.1
#SUMA12 = Contador suma acumulada etapa 1.2
#SUMA13 = Contador suma acumulada etapa etapa 1.3
#SUMA2 = Contador suma acumulada etapa etapa 2

#total_llegadas11 = número de personas que llegan a la etapa 1.1(microondas)
#total_llegadas12 = número de personas que llegan a la etapa 1.2(menú)
#total_llegadas13 = número de personas que llegan a la etapa 1.3(cafetería)
#total_llegadas = número de personas que llegan a la etapa 2(comedor)

#total_servicio11 = número de personas que han sido atendidas en el mircoondas
#total_servicio12 = número de personas que han sido atendidas en el menú
#total_servicio13 = número de personas que han sido atendidas en la barra
#total_servicio2 = número de personas que se han sentado

##############PARÁMETROS##############

lambdamic = 2 #personas que llegan por minuto al microondas
meanmic = 2 #media de tiempo que tarda el microondas
sdmic = 0.5 #desviacion del tiempo del microondas

lambdacaf = 3 #personas que llegan por minuto a la cafeteria
desv=1.5
med=3
sigmabar = sqrt( log(1+desv^2/med^2) )#valor sigma para la lognormal del servicio de la barra
mubar = (log(med^2)-desv^2)/2 #valor media para la lognormal del servicio de la barra


aforomax = 140
mucomida = 40 #valor mu para la normal truncada del tiempo en comer
sigmacomida = 10 #valor sigma para la normal truncada del tiempo en comer


##############INICIALIZACIÓN##############
N11 = 0
N12 = 0
N13 = 0
N2 = 0

TM = 0
TANT = 0
Tmax = 180

SUMA11 = 0
SUMA12 = 0
SUMA13 = 0
SUMA2 = 0


total_llegadas11=0
total_llegadas12=0
total_llegadas13=0
total_llegadas2=0

total_servicio11=0
total_servicio12=0
total_servicio13=0
total_servicio2=0


Asientos = rep(NA, aforomax)

#Generamos las primeras llegadas
set.seed(1)
DLMI = r_exp(1,lambdamic)
DLCA = r_exp(1,lambdacaf)
TLMI = DLMI
TLCA = DLCA


TSMI = Inf
TSME = Inf
TSBA = Inf
TSCO = Inf


##############PROGRAMA PRINCIPAL##############
while(TM<Tmax){
  
  TM = min(TLMI, TLCA, TSMI, TSME, TSBA, TSCO)
  Estado = 1*(TM == TLMI) + 2*(TM == TLCA) + 3*(TM == TSMI) + 4*(TM == TSME) + 5*(TM == TSBA) + 6*(TM == TSCO)
  
  if (Estado == 1){
    k = LlegadaMI(N11, N12, N13, N2, SUMA11, SUMA12, SUMA13, SUMA2, TM, TANT, TSMI,total_llegadas11)
    N11 = k[1]
    SUMA11 = k[2]
    SUMA12 = k[3]
    SUMA13 = k[4]
    SUMA2 = k[5]
    TANT = k[6]
    TSMI = k[7]
    TLMI = k[8]
    total_llegadas11=k[9]
  }
  else if (Estado == 2){
    k = LlegadaCA(N11, N12, N13, N2, SUMA11, SUMA12, SUMA13, SUMA2, TM, TANT, TSME, TSBA,total_llegadas12,total_llegadas13)
    N12 = k[1]
    N13 = k[2]
    SUMA11 = k[3]
    SUMA12 = k[4]
    SUMA13 = k[5]
    SUMA2 = k[6]
    TANT = k[7]
    TSME = k[8]
    TSBA = k[9]
    TLCA = k[10]
    total_llegadas12=k[11]
    total_llegadas13=k[12]
  }
  else if (Estado == 3){
    k = ServicioMI(N11, N12, N13, N2, SUMA11, SUM12, SUMA13, SUMA2, TSCO, TM, TANT, Asientos,total_servicio11,total_llegadas2)
    N11 = k[[1]]
    N2 = k[[2]]
    SUMA11 = k[[3]]
    SUMA12 = k[[4]]
    SUMA13 = k[[5]]
    SUMA2 = k[[6]]
    TSCO = k[[7]]
    TSMI = k[[8]]
    TANT = k[[9]]
    Asientos = k[[10]]
    total_servicio11=k[[11]]
    total_llegadas2=k[[12]]
  }
  else if (Estado == 4){
    k = ServicioME(N11, N12, N13, N2, SUMA11, SUM12, SUMA13, SUMA2, TSCO, TM, TANT, Asientos,total_servicio12,total_llegadas2)
    N12 = k[[1]]
    N2 = k[[2]]
    SUMA11 =k[[3]]
    SUMA12 =k[[4]]
    SUMA13 = k[[5]]
    SUMA2 = k[[6]]
    TSCO = k[[7]]
    TSME = k[[8]]
    TANT = k[[9]]
    Asientos = k[[10]]
    total_servicio12 = k[[11]]
    total_llegadas2 = k[[12]]
  }
  else if (Estado == 5){
    k = ServicioBA(N11, N12, N13, N2, SUMA11, SUM12, SUMA13, SUMA2, TSCO, TM, TANT, Asientos,total_servicio13,total_llegadas2)
    N13 =k[[1]]
    N2 = k[[2]]
    SUMA11 = k[[3]]
    SUMA12 = k[[4]]
    SUMA13 = k[[5]]
    SUMA2 = k[[6]]
    TSCO = k[[7]]
    TSBA = k[[8]]
    TANT = k[[9]]
    Asientos = k[[10]]
    total_servicio13 = k[[11]]
    total_llegadas2 = k[[12]]
  }
  else if (Estado == 6){
    k = ServicioCO(N11, N12, N13, N2, SUMA11, SUMA12, SUMA13, SUMA2, TM, TANT, Asientos,total_servicio2)
    N2 = k[[1]]
    SUMA11 = k[[2]]
    SUMA12 = k[[3]]
    SUMA13 = k[[4]]
    SUMA2 = k[[5]]
    TSCO = k[[6]]
    TANT = k[[7]]
    Asientos = k[[8]]
    total_servicio2 = k[[9]]
  }
  #print(paste("Estado", Estado, "SUMA2", SUMA2, "N11", N11, "N12", N12, "N13", N13, "N2", N2, "Nsent", length(Asientos)-length(which(is.na(Asientos)))))
}
print("Número de personas en cola en la cafetería")
N12+N13
print("Número de personas en la cola de los microondas")
N11
print("Número medio de personas en cada servicio:")
print("Número medio de personas en el microondas")
SUMA11/TM
print("Número medio de personas en menú")
SUMA12/TM
print("Número medio de personas en barra")
SUMA13/TM
print("Número medio de personas en el comedor")
SUMA2/TM

TM
