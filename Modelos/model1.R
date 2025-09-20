##############Distributions used##############

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





##############Creating subroutines##############

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


##############NOTATION##############

#           Stages
#   1.
#     1.1 Microwave service
#     1.2 Menu service
#     1.3 Bar service

#   2. dining room
   
#In this problem we will take into account the capacity of the cafeteria
#We will consider: 1 microwave that does not break down, 1 waiter and we will not take long
#time to sit down.

#N11 = Number of people on the stage 1.1
#N12 = Number of people on the stage 1.2
#N13 = Number of people on the stage 1.3
#N2 = Number of people on the stage 2

#TM = instant in simulation time
#TANT = previous moment
#Tmax = maximum simulation time

#DLMI = Distribution of arrival times at the microwave
#DLCA = Distribution of arrival times at the cafeteria

#DSMI = Microwave Service Time Distribution
#DSME = Menu Service Time Distribution
#DSBA = Bar Service Time Distribution
#DSCO = Dining Room Service Time Distribution

#TLMI = Next Microwave Arrival Time
#TLCA = Next Cafeteria Arrival Time

#TSMI = Microwave next service end time
#TSME = Menu next service end time
#TSBA = Bar next service end time
#TSCO = Dining room next service end time

#Asientos = Seat vector indicating whether the seat is empty and, if not, the next meal end for that seat.

#SUMA11 = Counter cumulative sum stage 1.1
#SUMA12 = Counter cumulative sum stage 1.2
#SUMA13 = Counter cumulative sum stage 1.3
#SUMA2 = Counter cumulative sum stage 2

#total_llegadas11 = number of people who reach the stage 1.1(microwave)
#total_llegadas12 = number of people who reach the stage 1.2(menu)
#total_llegadas13 = number of people who reach the stage 1.3(bar)
#total_llegadas = number of people who reach the stage 2(dinning room)

#total_servicio11 = number of people who have been served at the microwave
#total_servicio12 = number of people who have been served at the manu
#total_servicio13 = number of people who have been served at the bar
#total_servicio2 = number of people seated

##############PARAMETERS##############

lambdamic = 2 #people arriving at the microwave per minute
meanmic = 2 #average time taken by the microwave
sdmic = 0.5 #deviation of microwave time

lambdacaf = 3 #people arriving at the bar per minute
desv=1.5
med=3
sigmabar = sqrt( log(1+desv^2/med^2) )#sigma value for the lognormal of the bar service
mubar = (log(med^2)-desv^2)/2 #mean value for the lognormal of the bar service


aforomax = 140
mucomida = 40 #mu value for the truncated normal eating time
sigmacomida = 10 #sigma value for the truncated normal eating time


##############INICIALIZATION##############
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


##############PRINCIPAL PROGRAM##############
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
print("Number of people in line at the cafeteria")
N12+N13
print("Number of people in the microwave queue")
N11
print("Average number of people in each service:")
print("Average number of people in the microwave")
SUMA11/TM
print("Average number of people in the menu")
SUMA12/TM
print("Average number of people in the bar")
SUMA13/TM
print("Average number of people in the dinning room")
SUMA2/TM

TM

