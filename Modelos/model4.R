##############Triangular##############

rtriang = function(n){
  u = runif(n)
  sample = ifelse(u<1/2, (1/3)*(1 + sqrt(2*u)), 1 - (1/3)*sqrt(2*(1-u)))
  return(sample)
}

rnormal = function(n, mu, sigma){
  if (n%%2 == 1){
    u1 = runif(n)
    u2 = runif(n)
    x = sqrt(-2*log(u2))*cos(2*pi*u1)
    sample = x
    sample2 = sample*sigma+mu
  }else{
    u1 = runif(n/2)
    u2 = runif(n/2)
    x = sqrt(-2*log(u2))*cos(2*pi*u1)
    y = sqrt(-2*log(u2))*sin(2*pi*u1)
    sample = c(x,y)
    sample2 = sample*sigma+mu
  }
  return(sample2)
}

r_exp = function(n, lambda){
  u = runif(n)
  sample = -log(1-u)/lambda
  return(sample)
}

rlog_normal = function(n, mu, sigma){
  if (n%%2 == 1){
    u1 = runif(n)
    u2 = runif(n)
    x = sqrt(-2*log(u2))*sin(2*pi*u1)
    sample2 = x*sigma+mu
    sample = exp(sample2)
    
  }else{
    u1 = runif(n/2)
    u2 = runif(n/2)
    x = sqrt(-2*log(u2))*cos(2*pi*u1)
    y = sqrt(-2*log(u2))*sin(2*pi*u1)
    z = c(x,y)
    sample2 = z*sigma+mu
    sample = exp(sample2)
  }
  return (sample)
}
rnormal_truncada = function(n, mu, sigma){
  samplet = c()
  if (n%%2 == 1){
    while (length(samplet) == 0){
      u1 = runif(n)
      u2 = runif(n)
      x = sqrt(-2*log(u2))*cos(2*pi*u1)
      sample = x
      sample2 = sample*sigma+mu
      samplet=sample2[sample2>15]
    }
  }
  else{
    while (length(samplet) == 0){
      u1 = runif(n/2)
      u2 = runif(n/2)
      x = sqrt(-2*log(u2))*cos(2*pi*u1)
      y = sqrt(-2*log(u2))*sin(2*pi*u1)
      sample = c(x,y)
      sample2 = sample*sigma+mu
      samplet=sample2[sample2>15]
    }
  }
  return(samplet[n])
}

##############Creating subroutines##############

LlegadaMI = function(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TM, TANT, TSMI, Microondas, PMI, AsientosSA, AsientosCaf, total_llegadas11,total_llegadas21, total_llegadas22,f){
  N11 = N11 + 1
  total_llegadas11 = total_llegadas11 + 1
  DLMI = r_exp(1,lambdamic)
  TLMI = TM + DLMI
  
  u=runif(1)
  
  if(N11 > PIMI & u<=0.2){
    N11 = N11 - 1
    f=f+1
    #Va directo a comer al comedor, tenemos que ver cual de los los comedores (sala azul o cafetería están con algún asiento libre)
    N21 = N21 + 1
    
    if(N21 >= 1 & any(is.na(AsientosSA))){
      total_llegadas21 = total_llegadas21 + 1
      DSCO_SA = rnormal_truncada(1, mucomida, sigmacomida)
      positions = which(is.na(AsientosSA))
      AsientosSA[positions[1]] = TM+DSCO_SA
      TSCO_SA = min(AsientosSA,na.rm = TRUE)
      SUMA21 = SUMA21 + (N21-1)*(TM-TANT)
      SUMA22 = SUMA22 + (N22)*(TM-TANT)
    }
    
    else{
      N21 = N21 - 1
      N22 = N22 + 1
      if(N22 >= 1 & any(is.na(AsientosCaf))){
        total_llegadas22 = total_llegadas22 + 1
        DSCO_Caf = rnormal_truncada(1, mucomida, sigmacomida)
        positions = which(is.na(AsientosCaf))
        AsientosCaf[positions[1]] = TM+DSCO_Caf
        TSCO_Caf = min(AsientosCaf,na.rm = TRUE)
      }
      SUMA21 = SUMA21 + (N21)*(TM-TANT)
      SUMA22 = SUMA22 + (N22-1)*(TM-TANT)
    }
    SUMA11 = SUMA11 + N11*(TM - TANT)
  }
  else{
    if(any(is.na(Microondas))){
      aux=-1
      while(aux<0){
        aux=rnormal(1,meanmic,sdmic)
      }
      DSMI = aux
      positions=which(is.na(Microondas))
      Microondas[positions[1]]=TM+DSMI
      TSMI=min(Microondas,na.rm = TRUE)
    }
    SUMA11 = SUMA11 + (N11 - 1)*(TM - TANT)
    SUMA21 = SUMA21 + N21*(TM - TANT)
    SUMA22 = SUMA22 + N22*(TM - TANT)
  }
  
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  
  TANT = TM
  return(list(N11, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TANT, TSMI, TLMI, Microondas, AsientosSA, AsientosCaf, TSCO_SA, TSCO_Caf, N21, N22,total_llegadas11,total_llegadas21, total_llegadas22,f))
}

LlegadaCA = function(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TM, TANT, TSME, TSBA,total_llegadas12, total_llegadas13, total_llegadas21, total_llegadas22, Camareros, abandono){
  
  u = runif(1)
  pedido = 1*(u<=0.55) + 2*(u>0.55) #1 menú, 2 barra
  DLCA = r_exp(1,lambdacaf)
  TLCA = TM + DLCA
  
  if (pedido ==1){
    
    N12 = N12 + 1
    total_llegadas12 = total_llegadas12 + 1
    
    if(N12 == 1){
      DSME = rtriang(1)
      TSME=TM+DSME
    }
    
    SUMA12 = SUMA12 + (N12-1)*(TM-TANT)
    SUMA13 = SUMA13 + N13*(TM-TANT)
    
  }
  
  else if (pedido ==2){
    
    N13 = N13 + 1
    total_llegadas13 = total_llegadas13 + 1
    
    u = runif(1)
    
    if(N13 > PIBARRA & u<=1/2){
      N13 = N13 - 1
      abandono = abandono + 1
      SUMA13 = SUMA13 + N13*(TM - TANT)
    }
    
    else{
      if(any(is.na(Camareros))){
        DSBA = rlog_normal(1,mubar,sigmabar)
        positions=which(is.na(Camareros))
        Camareros[positions[1]]=TM+DSBA
        TSBA=min(Camareros,na.rm = TRUE)
      }
      SUMA13 = SUMA13 + (N13 - 1)*(TM-TANT)
    }
    SUMA12 = SUMA12 + N12*(TM-TANT)
  }
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA21 = SUMA21 + N21*(TM-TANT)
  SUMA22 = SUMA22 + N22*(TM-TANT)
  
  TANT = TM
  
  return (list(N12, N13, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TANT, TSME, TSBA, TLCA, Camareros, total_llegadas12, total_llegadas13, total_llegadas21, total_llegadas22, abandono))
}

ServicioMI = function(N11, N12, N13, N21, N22, SUMA11, SUM12, SUMA13, SUMA21, SUMA22, TSCO_SA, TSCO_Caf, TM, TANT, AsientosSA, AsientosCaf, Microondas, total_servicio11, total_llegadas21, total_llegadas22){
  
  N11 = N11-1
  N21 = N21 + 1
  total_servicio11=total_servicio11+1
  total_llegadas21=total_llegadas21+1
  
  Microondas[Microondas<=TM ]=NA
  
  if (length(which(is.na(Microondas)))>=1 & N11 - length(which(!is.na(Microondas)))>0){
    
    aux=-1
    while(aux<0){
      aux=rnormal(1,meanmic,sdmic)
    }
    
    DSMI = aux
    positions=which(is.na(Microondas))
    Microondas[positions[1]]=TM+DSMI
  }
  
  TSMI=min(Microondas,na.rm = TRUE)
  
  
  if(any(is.na(AsientosSA))){
    DSCO_SA = rnormal_truncada(1, mucomida, sigmacomida)
    positions = which(is.na(AsientosSA))
    AsientosSA[positions[1]] = TM+DSCO_SA
    TSCO_SA = min(AsientosSA,na.rm = TRUE)
    SUMA21 = SUMA21 + (N21-1)*(TM-TANT)
    SUMA22 = SUMA22 + (N22)*(TM-TANT)
  }
  
  else{
    total_llegadas21 = total_llegadas21 - 1
    total_llegadas22 = total_llegadas22+1
    N21 = N21 - 1
    N22 = N22 + 1
    
    if(any(is.na(AsientosCaf))){
      DSCO_Caf = rnormal_truncada(1, mucomida, sigmacomida)
      positions = which(is.na(AsientosCaf))
      AsientosCaf[positions[1]] = TM+DSCO_Caf
      TSCO_Caf = min(AsientosCaf,na.rm = TRUE)
    }
    
    SUMA21 = SUMA21 + (N21)*(TM-TANT)
    SUMA22 = SUMA22 + (N22-1)*(TM-TANT)
    
  }
  
  SUMA11 = SUMA11 + (N11+1)*(TM-TANT)
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  
  
  TANT = TM
  
  return (list(N11, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TSCO_SA, TSCO_Caf, TSMI, TANT, AsientosSA, AsientosCaf, Microondas, total_servicio11, total_llegadas21, total_llegadas22))
}

ServicioME = function(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TSCO_SA, TSCO_Caf, TM, TANT, AsientosSA, AsientosCaf, total_servicio12, total_llegadas21, total_llegadas22){
  
  N12 = N12-1
  N22 = N22 + 1
  total_servicio12=total_servicio12+1
  total_llegadas22=total_llegadas22+1
  
  DSME=rtriang(1)
  TSME = ifelse(N12 == 0, Inf, TM + DSME)
  
  if(any(is.na(AsientosCaf))){
    DSCO_Caf = rnormal_truncada(1, mucomida, sigmacomida)
    positions=which(is.na(AsientosCaf))
    AsientosCaf[positions[1]]=TM+DSCO_Caf
    TSCO_Caf=min(AsientosCaf,na.rm = TRUE)
    
    SUMA22 = SUMA22 + (N22-1)*(TM-TANT)
    SUMA21 = SUMA21 + (N21)*(TM-TANT)
  }
  else{
    
    N22 = N22 - 1
    N21 = N21 +1
    total_llegadas22 = total_llegadas22 - 1
    total_llegadas21 = total_llegadas21+1
    
    if(any(is.na(AsientosSA))){
      DSCO_SA = rnormal_truncada(1, mucomida, sigmacomida)
      positions=which(is.na(AsientosSA))
      AsientosSA[positions[1]]=TM+DSCO_SA
      TSCO_SA=min(AsientosSA,na.rm = TRUE)
    }
    
    SUMA22 = SUMA22 + (N22)*(TM-TANT)
    SUMA21 = SUMA21 + (N21-1)*(TM-TANT)
  }
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA12 = SUMA12 + (N12+1)*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  
  
  TANT = TM
  
  return (list(N12, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TSCO_SA, TSCO_Caf, TSME, TANT, AsientosSA, AsientosCaf,total_servicio12, total_llegadas21, total_llegadas22 ))
}

ServicioBA = function(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TSCO_SA, TSCO_Caf, TM, TANT, AsientosSA, AsientosCaf, Camareros, total_servicio13, total_llegadas21, total_llegadas22){
  
  N13 = N13-1
  N22 = N22 + 1
  total_servicio13=total_servicio13+1
  total_llegadas22=total_llegadas22+1
  
  Camareros[Camareros<=TM ]=NA
  
  if (length(which(is.na(Camareros)))>=1 & N13 - length(which(!is.na(Camareros)))>0){
    
    DSBA = rlog_normal(1,mubar,sigmabar)
    positions=which(is.na(Camareros))
    Camareros[positions[1]]=TM+DSBA
  }
  
  TSBA=min(Camareros,na.rm = TRUE)
  
  
  if(N2 >= 1 & any(is.na(AsientosCaf))){
    DSCO_Caf = rnormal_truncada(1, mucomida, sigmacomida)
    positions=which(is.na(AsientosCaf))
    AsientosCaf[positions[1]]=TM+DSCO_Caf
    TSCO_Caf=min(AsientosCaf,na.rm = TRUE)
    
    SUMA22 = SUMA22 + (N22-1)*(TM-TANT)
    SUMA21 = SUMA21 + (N21)*(TM-TANT)
  }
  
  else{
    
    N22 = N22 - 1
    N21 = N21 +1
    total_llegadas22 = total_llegadas22 - 1
    total_llegadas21 = total_llegadas21+1
    
    if(any(is.na(AsientosSA))){
      DSCO_SA = rnormal_truncada(1, mucomida, sigmacomida)
      positions=which(is.na(AsientosSA))
      AsientosSA[positions[1]]= TM + DSCO_SA 
      TSCO_SA=min(AsientosSA,na.rm = TRUE)
    }
    
    SUMA22 = SUMA22 + (N22)*(TM-TANT)
    SUMA21 = SUMA21 + (N21-1)*(TM-TANT)
    
  }
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + (N13+1)*(TM-TANT)
  
  
  TANT = TM
  
  return (list(N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TSCO_SA, TSCO_Caf, TSBA, TANT, AsientosSA, AsientosCaf, Camareros, total_servicio13, total_llegadas21, total_llegadas22))
}

ServicioCO_SA = function(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TM, TANT, AsientosSA, total_servicio21){
  
  N21 = N21 - 1
  total_servicio21 = total_servicio21 + 1
  AsientosSA[AsientosSA<=TM ]=NA
  
  if (length(which(is.na(AsientosSA)))>=1 & N21 - length(which(!is.na(AsientosSA)))>0){
    
    DSCO_SA = rnormal_truncada(1, mucomida, sigmacomida)
    positions=which(is.na(AsientosSA))
    AsientosSA[positions[1]]=TM+DSCO_SA
  }
  TSCO_SA=min(AsientosSA,na.rm = TRUE)
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  SUMA21 = SUMA21 + (N21+1)*(TM-TANT)
  SUMA22 = SUMA22 + (N22)*(TM-TANT)
  
  TANT = TM
  
  return (list(N21, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TSCO_SA, TANT, AsientosSA, total_servicio21))
}

ServicioCO_Caf = function(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TM, TANT, AsientosCaf, total_servicio22){
  
  N22 = N22 - 1
  total_servicio22 = total_servicio22+1
  
  AsientosCaf[AsientosCaf<=TM ]=NA
  
  if (length(which(is.na(AsientosCaf)))>=1 & N22 - length(which(!is.na(AsientosCaf)))>0){
    
    DSCO_Caf = rnormal_truncada(1, mucomida, sigmacomida)
    positions=which(is.na(AsientosCaf))
    AsientosCaf[positions[1]]=TM+DSCO_Caf
  }
  
  TSCO_Caf=min(AsientosCaf,na.rm = TRUE)
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  SUMA21 = SUMA21 + (N21)*(TM-TANT)
  SUMA22 = SUMA22 + (N22+1)*(TM-TANT)
  
  TANT = TM
  
  return (list(N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TSCO_Caf, TANT, AsientosCaf, total_servicio22))
}


##############NOTATION##############

#           Stages
#   1.
#     1.1 Microwave service
#     1.2 Menu service
#     1.3 Bar service

#   2. 
#     2.1 Blue room service (microwave service is in this room)
#     2.2 Cafeteria service (menu and bar services are in the cafeteria)

#In this problem, we'll consider the capacity of the cafeteria and the blue room.
#We'll consider: 4 microwaves that don't break down, 2 waiters, and it doesn't take long to be seated.

#N11 = Number of people on the stage 1.1
#N12 = Number of people on the stage 1.2
#N13 = Number of people on the stage 1.3
#N21 = Number of people on the stage 2.1
#N22 = Number of people on the stage 2.2

#TM = instant in simulation time
#TANT = previous instant
#Tmax = maximum time of simulation

#DLMI = Distribution of arrival times at the microwave
#DLCA = Distribution of arrival times at the cafeteria

#DSMI = Distribution of microwave service times
#DSME = Distribution of menu service times
#DSBA = Distribution of bar service times
#DSCO_SA = Distribution of blue room service times
#DSCO_Caf = Distribution of cafeteria service times

#TLMI = Instant of the next arrival at the microwave
#TLCA = Instant of the next arrival at the cafeteria

#TSMI = Instante de la próxima finalización del servicio del microondas
#TSME = Instante de la próxima finalización del servicio del menú
#TSBA = Instante de la próxima finalización del servicio de la barra
#TSCO_SA = Instante de la próxima finalización del servicio de la sala azul
#TSCO_Caf = Instante de la próxima finalización del servicio de la cafetería


#AsientosSA = Vector de asientos de la sala azul que indica si está vacio el asiento y 
#           si no el próximo fin de comida de ese asiento
#AsientosCaf = Vector de asientos de la cafetería que indica si está vacio el asiento y 
#           si no el próximo fin de comida de ese asiento
#Microondas= Vector que indica qué microondas están libres con NA u ocupados
#            con el próximo fin de servicio
#Camareros= Vector que indica qué camareros están libres con NA u ocupados con
#           el próximo fin de servicio

#SUMA11 = Contador suma acumulada etapa 1.1
#SUMA12 = Contador suma acumulada etapa 1.2
#SUMA13 = Contador suma acumulada etapa etapa 1.3
#SUMA21 = Contador suma acumulada etapa etapa 2.1
#SUMA22 = Contador suma acumulada etapa etapa 2.2


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
N21 = 0
N22 = 0

TM = 0
TANT = 0
Tmax = 180

SUMA11 = 0
SUMA12 = 0
SUMA13 = 0
SUMA21 = 0
SUMA22 = 0

PIBARRA = 10
PIMI = 10
f = 0
abandono= 0

aforomax_Caf = 80
aforomax_SA = 60

AsientosCaf = rep(NA, aforomax_Caf)
AsientosSA = rep(NA, aforomax_SA)
Microondas = rep(NA, 4)
Camareros = rep(NA, 2)

#Generamos las primeras llegadas
set.seed(1)
DLMI = rexp(1,rate=lambdamic)
DLCA = rexp(1,rate=lambdacaf)
TLMI = DLMI
TLCA = DLCA

TSMI = Inf
TSME = Inf
TSBA = Inf
TSCO_SA = Inf
TSCO_Caf = Inf

total_llegadas11 = 0
total_llegadas12 = 0
total_llegadas13 = 0
total_llegadas21 = 0
total_llegadas22 = 0

total_servicio11 = 0
total_servicio12 = 0
total_servicio13 = 0
total_servicio21 = 0
total_servicio22 = 0

##############PROGRAMA PRINCIPAL##############
while(TM<Tmax){
  TM = min(TLMI, TLCA, TSMI, TSME, TSBA, TSCO_SA, TSCO_Caf)
  Estado = 1*(TM == TLMI) + 2*(TM == TLCA) + 3*(TM == TSMI) + 4*(TM == TSME) + 5*(TM == TSBA) + 6*(TM == TSCO_SA) + 7*(TM == TSCO_Caf)
  
  if (Estado == 1){
    k = LlegadaMI(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TM, TANT, TSMI, Microondas, PMI, AsientosSA, AsientosCaf, total_llegadas11,total_llegadas21, total_llegadas22,f)
    N11 = k[[1]]
    SUMA11 = k[[2]]
    SUMA12 = k[[3]]
    SUMA13 = k[[4]]
    SUMA21 = k[[5]]
    SUMA22 = k[[6]]
    TANT= k[[7]]
    TSMI = k[[8]]
    TLMI  = k[[9]]
    Microondas  = k[[10]]
    AsientosSA = k[[11]]
    AsientosCaf = k[[12]]
    TSCO_SA = k[[13]]
    TSCO_Caf = k[[14]]
    N21 = k[[15]]
    N22 = k[[16]]
    total_llegadas11 = k[[17]]
    total_llegadas21 = k[[18]]
    total_llegadas22 = k[[19]]
    f
  }
  
  else if (Estado == 2){
    k = LlegadaCA(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TM, TANT, TSME, TSBA,total_llegadas12, total_llegadas13, total_llegadas21, total_llegadas22, Camareros, abandono)
    N12 = k[[1]]
    N13= k[[2]]
    SUMA11 = k[[3]]
    SUMA12= k[[4]]
    SUMA13 = k[[5]]
    SUMA21= k[[6]]
    SUMA22 = k[[7]]
    TANT= k[[8]]
    TSME = k[[9]]
    TSBA= k[[10]]
    TLCA= k[[11]]
    Camareros= k[[12]]
    total_llegadas12= k[[13]]
    total_llegadas13= k[[14]]
    total_llegadas21= k[[15]]
    total_llegadas22= k[[16]]
    abandono= k[[17]]
  }
  
  else if (Estado == 3){
    k = ServicioMI(N11, N12, N13, N21, N22, SUMA11, SUM12, SUMA13, SUMA21, SUMA22, TSCO_SA, TSCO_Caf, TM, TANT, AsientosSA, AsientosCaf, Microondas, total_servicio11, total_llegadas21, total_llegadas22)
    N11 = k[[1]]
    N21 = k[[2]]
    N22 = k[[3]]
    SUMA11  = k[[4]]
    SUMA12 = k[[5]]
    SUMA13 = k[[6]]
    SUMA21 = k[[7]]
    SUMA22 = k[[8]]
    TSCO_SA = k[[9]]
    TSCO_Caf = k[[10]]
    TSMI = k[[11]]
    TANT = k[[12]]
    AsientosSA = k[[13]]
    AsientosCaf = k[[14]]
    Microondas = k[[15]]
    total_servicio11 = k[[16]]
    total_llegadas21 = k[[17]]
    total_llegadas22 = k[[18]]
    
  }
  
  else if (Estado == 4){
    k = ServicioME(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TSCO_SA, TSCO_Caf, TM, TANT, AsientosSA, AsientosCaf, total_servicio12, total_llegadas21, total_llegadas22)
    N12 = k[[1]]
    N21  = k[[2]]
    N22 = k[[3]]
    SUMA11 = k[[4]]
    SUMA12 = k[[5]]
    SUMA13 = k[[6]]
    SUMA21 = k[[7]]
    SUMA22 = k[[8]]
    TSCO_SA = k[[9]]
    TSCO_Caf = k[[10]]
    TSME = k[[11]]
    TANT = k[[12]]
    AsientosSA = k[[13]]
    AsientosCaf = k[[14]]
    total_servicio12 = k[[15]]
    total_llegadas21 = k[[16]]
    total_llegadas22 = k[[17]]
    
  }
  
  else if (Estado == 5){
    k = ServicioBA(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TSCO_SA, TSCO_Caf, TM, TANT, AsientosSA, AsientosCaf, Camareros, total_servicio13, total_llegadas21, total_llegadas22)
    N13 = k[[1]]
    N21= k[[2]]
    N22= k[[3]]
    SUMA11= k[[4]]
    SUMA12= k[[5]]
    SUMA13= k[[6]]
    SUMA21= k[[7]]
    SUMA22= k[[8]]
    TSCO_SA= k[[9]]
    TSCO_Caf= k[[10]]
    TSBA= k[[11]]
    TANT= k[[12]]
    AsientosSA= k[[13]]
    AsientosCaf = k[[14]]
    Camareros= k[[15]]
    total_servicio13= k[[16]]
    total_llegadas21= k[[17]]
    total_llegadas22= k[[18]]
    
  }
  
  else if (Estado == 6){
    k = ServicioCO_SA(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TM, TANT, AsientosSA, total_servicio21)
    N21 = k[[1]]
    SUMA11 = k[[2]]
    SUMA12 = k[[3]]
    SUMA13 = k[[4]]
    SUMA21 = k[[5]]
    SUMA22 = k[[6]]
    TSCO_SA = k[[7]]
    TANT = k[[8]]
    AsientosSA = k[[9]]
    total_servicio21 = k[[10]]
  }
  
  else if (Estado == 7){
    k = ServicioCO_Caf(N11, N12, N13, N21, N22, SUMA11, SUMA12, SUMA13, SUMA21, SUMA22, TM, TANT, AsientosCaf,total_servicio22)
    N22 = k[[1]]
    SUMA11 = k[[2]]
    SUMA12 = k[[3]]
    SUMA13 = k[[4]]
    SUMA21 = k[[5]]
    SUMA22 = k[[6]]
    TSCO_Caf = k[[7]]
    TANT = k[[8]]
    AsientosCaf = k[[9]]
    total_servicio22 = k[[10]]
    
  }
  
  
}
print("Número de personas en la cola de la cafetería")
N12+N13
print("Número de personas en la cola de los microondas")
N11
print("Número medio de personas en cada servicio:")
print("Número medio de personas en el microondas")
SUMA11/TM
print("Número medio de personas en menú")
SUMA12/TM
print("Número medio de personas en la barra")
SUMA13/TM
print("Número medio de personas en la sala azul")
SUMA21/TM
print("Número medio de personas en la cafetería")
SUMA22/TM

TM








