##############Distributions##############

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
  n = n + 1
  if (n%%2 == 1){
    u1 = runif(n)
    u2 = runif(n)
    x = sqrt(-2*log(u2))*cos(2*pi*u1)
    sample = x
    sample2 = sample*sigma+mu
    samplet=sample2[sample2>15]
  }
  else{
    u1 = runif(n/2)
    u2 = runif(n/2)
    x = sqrt(-2*log(u2))*cos(2*pi*u1)
    y = sqrt(-2*log(u2))*sin(2*pi*u1)
    sample = c(x,y)
    sample2 = sample*sigma+mu
    samplet=sample2[sample2>15]
  }
  return(samplet[n])
}


##############Creating subroutines##############

LlegadaMI = function(N11, N12, N13, N2, SUMA11, SUMA12, SUMA13, SUMA2, TM, TANT, TSMI, Microondas,total_llegadas11){
  N11 = N11 + 1
  total_llegadas11=total_llegadas11+1
  
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
  
  SUMA11 = SUMA11 + (N11-1)*(TM-TANT)
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  SUMA2 = SUMA2 + N2*(TM-TANT)
  
  DLMI = r_exp(1,lambdamic)
  TLMI = TM + DLMI
  
  
  TANT = TM
  return(list(N11, SUMA11, SUMA12, SUMA13, SUMA2, TANT, TSMI, TLMI, Microondas,total_llegadas11))
}


LlegadaCA = function(N11, N12, N13, N2, SUMA11, SUMA12, SUMA13, SUMA2, TM, TANT, TSME, TSBA, Camareros,total_llegadas12,total_llegadas13){
  
  u = runif(1)
  order = 1*(u<=0.55) + 2*(u>0.55) #1 menu, 2 sandwich
  
  if (order ==1){
    N12 = N12 + 1
    total_llegadas12=total_llegadas12+1
    
    if(N12 == 1){
      DSME = rtriang(1)
      TSME=TM+DSME
    }
    
    SUMA12 = SUMA12 + (N12-1)*(TM-TANT)
    SUMA13 = SUMA13 + N13*(TM-TANT)
    
  }
  
  else if (order ==2){
    N13 = N13 + 1
    total_llegadas13=total_llegadas13+1
    
    if(any(is.na(Camareros))){
      DSBA = rlog_normal(1,mubar,sigmabar)
      positions=which(is.na(Camareros))
      Camareros[positions[1]]=TM+DSBA
      TSBA=min(Camareros,na.rm = TRUE)
    }
    
    SUMA12 = SUMA12 + N12*(TM-TANT)
    SUMA13 = SUMA13 + (N13 - 1)*(TM-TANT)
    
  }
  
  DLCA = r_exp(1,lambdacaf)
  TLCA = TM + DLCA
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA2 = SUMA2 + N2*(TM-TANT)
  
  
  TANT = TM
  return(list(N12,N13, SUMA11, SUMA12, SUMA13, SUMA2, TANT, TSME, TSBA, TLCA, Camareros,total_llegadas12,total_llegadas13))
}

ServicioMI = function(N11, N12, N13, N2, SUMA11, SUM12, SUMA13, SUMA2, TSCO, TM, TANT, Asientos, Microondas,total_servicio11,total_llegadas2){
  N11 = N11-1
  N2 = N2 + 1
  total_servicio11=total_servicio11+1
  total_llegadas2=total_llegadas2+1
  
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
  
  
  if(any(is.na(Asientos))){
    DSCO = rnormal_truncada(1, mucomida, sigmacomida)
    positions=which(is.na(Asientos))
    Asientos[positions[1]]=TM+DSCO
    TSCO=min(Asientos,na.rm = TRUE)
  }
  
  SUMA11 = SUMA11 + (N11+1)*(TM-TANT)
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  SUMA2 = SUMA2 + (N2-1)*(TM-TANT)
  
  
  TANT = TM
  return(list(N11,N2, SUMA11, SUMA12, SUMA13, SUMA2, TSCO, TSMI, TANT,Asientos, Microondas,total_servicio11,total_llegadas2))
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
    positions=which(is.na(Asientos))
    Asientos[positions[1]]=TM+DSCO
    TSCO=min(Asientos,na.rm = TRUE)
  }
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA12 = SUMA12 + (N12+1)*(TM-TANT)
  SUMA13 = SUMA13 + N13*(TM-TANT)
  SUMA2 = SUMA2 + (N2-1)*(TM-TANT)
  
  
  TANT = TM
  return(list(N12,N2, SUMA11, SUMA12, SUMA13, SUMA2, TSCO, TSME, TANT, Asientos,total_servicio12,total_llegadas2))
}

ServicioBA = function(N11, N12, N13, N2, SUMA11, SUM12, SUMA13, SUMA2, TSCO, TM, TANT, Asientos, Camareros,total_servicio13,total_llegadas2){
  N13 = N13-1
  N2 = N2 + 1
  total_servicio13=total_servicio13+1
  total_llegadas2=total_llegadas2+1
  
  Camareros[Camareros<=TM ]=NA
  
  if (length(which(is.na(Camareros)))>=1 & N13 - length(which(!is.na(Camareros)))>0){
    
    DSBA = rlog_normal(1,mubar,sigmabar)
    positions=which(is.na(Camareros))
    Camareros[positions[1]]=TM+DSBA
  }
  
  TSBA=min(Camareros,na.rm = TRUE)
  
  
  if(N2 >= 1 & any(is.na(Asientos))){
    DSCO = rnormal_truncada(1, mucomida, sigmacomida)
    positions=which(is.na(Asientos))
    Asientos[positions[1]]=TM+DSCO
    TSCO=min(Asientos,na.rm = TRUE)
  }
  
  SUMA11 = SUMA11 + N11*(TM-TANT)
  SUMA12 = SUMA12 + N12*(TM-TANT)
  SUMA13 = SUMA13 + (N13+1)*(TM-TANT)
  SUMA2 = SUMA2 + (N2-1)*(TM-TANT)
  
  
  TANT = TM
  return(list(N13,N2, SUMA11, SUMA12, SUMA13, SUMA2, TSCO, TSBA, TANT, Asientos, Camareros,total_servicio13,total_llegadas2))
}

ServicioCO = function(N11, N12, N13, N2, SUMA11, SUMA12, SUMA13, SUMA2, TM, TANT, Asientos,total_servicio2){
  N2 = N2 - 1
  total_servicio2=total_servicio2+1
  
  Asientos[Asientos<=TM ]=NA
  
  if (length(which(is.na(Asientos)))>=1 & N2 - length(which(!is.na(Asientos)))>0){
    print(1)
    DSCO = rnormal_truncada(1, mucomida, sigmacomida)
    positions=which(is.na(Asientos))
    Asientos[positions[1]]=TM+DSCO
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

#   2. Dining room

#In this problem we will take into account the capacity of the cafeteria
#We'll consider: 1 microwave that doesn't break down, 1 waiter, and it doesn't take long to get seated.

#N11 = Number of people on the stage 1.1
#N12 = Number of people on the stage 1.2
#N13 = Number of people on the stage 1.3
#N2 = Number of people on the stage 2

#TM = instant in simulation time
#TANT = previous moment
#Tmax = maximum simulation time

#DLMI = Distribution of arrival times at the microwave
#DLCA = Distribution of arrival times at the cafeteria

#DSMI = Distribution of microwave service times
#DSME = Distribution of menu service times
#DSBA = Distribution of bar service times
#DSCO = Distribution of dining room service times

#TLMI = Instant of the next arrival at the microwave
#TLCA = Snapshot of the next arrival at the cafeteria

#TSMI = Instant of the next microwave service completion
#TSME = Instant of the next menu service completion
#TSBA = Instant of the next bar service completion
#TSCO = Instant of the next dining room service completion

#Asientos = Seat vector indicating whether the seat is empty and 
#           if not the next meal end for that seat
#Microondas= Vector indicating which microwaves are free with NA or busy
#            with the next end of service
#Camareros= Vector indicating which waiters are free with NA or busy with
#           the next end of service

#SUMA11 = Counter cumulative sum stage 1.1
#SUMA12 = Counter cumulative sum stage 1.2
#SUMA13 = Counter cumulative sum stage 1.3
#SUMA2 = Counter cumulative sum stage 2

#total_llegadas11 = number of people who reach the stage 1.1(microwave)
#total_llegadas12 = number of people who reach the stage 1.2(menu)
#total_llegadas13 = number of people who reach the stage 1.3(cafeteria)
#total_llegadas = number of people who reach the stage 2(dining room)

#total_servicio11 = number of people who have been served at the microwave
#total_servicio12 = number of people who have been served at the menu
#total_servicio13 = number of people who have been served at the bar
#total_servicio2 = number of people seated

##############PARAMETERS##############
lambdamic = 2 #people who come to the microwave per minute
meanmic = 2 #average time it takes to use the microwave
sdmic = 0.5 #microwave time deviation

lambdacaf = 3 #people arriving at the cafeteria per minute
desv=1.5
med=3
sigmabar = sqrt( log(1+desv^2/med^2) )#sigma value for the lognormal of the bar service
mubar = (log(med^2)-desv^2)/2 #mean value for the lognormal of the bar service


aforomax = 140
mucomida = 40 #mu value for the truncated normal eating time
sigmacomida = 10 #sigma value for the truncated normal eating time

##############INICIALIZATION##############
set.seed(1)
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
Microondas = rep(NA, 4)
Camareros = rep(NA, 2)

#Generamos las primeras llegadas

DLMI = r_exp(1,lambdamic)
DLCA = r_exp(1,lambdacaf)
TLMI = DLMI
TLCA = DLCA


TSMI = Inf
TSME = Inf
TSBA = Inf
TSCO = Inf


##############MAIN PROGRAM##############
while(TM<Tmax){
  TM = min(TLMI, TLCA, TSMI, TSME, TSBA, TSCO)
  Estado = 1*(TM == TLMI) + 2*(TM == TLCA) + 3*(TM == TSMI) + 4*(TM == TSME) + 5*(TM == TSBA) + 6*(TM == TSCO)
  
  if (Estado == 1){
    k = LlegadaMI(N11, N12, N13, N2, SUMA11, SUMA12, SUMA13, SUMA2, TM, TANT, TSMI, Microondas,total_llegadas11)
    N11 = k[[1]]
    SUMA11 = k[[2]]
    SUMA12 = k[[3]]
    SUMA13 = k[[4]]
    SUMA2 = k[[5]]
    TANT = k[[6]]
    TSMI = k[[7]]
    TLMI = k[[8]]
    Microondas = k[[9]]
    total_llegadas11=k[[10]]
  }
  else if (Estado == 2){
    k = LlegadaCA(N11, N12, N13, N2, SUMA11, SUMA12, SUMA13, SUMA2, TM, TANT, TSME, TSBA, Camareros,total_llegadas12,total_llegadas13)
    N12 = k[[1]]
    N13 = k[[2]]
    SUMA11 = k[[3]]
    SUMA12 = k[[4]]
    SUMA13 = k[[5]]
    SUMA2 = k[[6]]
    TANT = k[[7]]
    TSME = k[[8]]
    TSBA = k[[9]]
    TLCA = k[[10]]
    Camareros = k[[11]]
    total_llegadas12=k[[12]]
    total_llegadas13=k[[13]]
  }
  else if (Estado == 3){
    k = ServicioMI(N11, N12, N13, N2, SUMA11, SUM12, SUMA13, SUMA2, TSCO, TM, TANT, Asientos, Microondas,total_servicio11,total_llegadas2)
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
    Microondas = k[[11]]
    total_servicio11 = k[[12]]
    total_llegadas2 = k[[13]]
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
    k = ServicioBA(N11, N12, N13, N2, SUMA11, SUM12, SUMA13, SUMA2, TSCO, TM, TANT, Asientos, Camareros,total_servicio13,total_llegadas2)
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
    Camareros = k[[11]]
    total_servicio13 = k[[12]]
    total_llegadas2 = k[[13]]
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
}
print("Number of people in line at the cafeteria")
N12+N13
print("Number of people in line at the microwave")
N11
print("Average number of people in each service:")
print("Average number of people in the microwave")
SUMA11/TM
print("Average number of people in menu")
SUMA12/TM
print("Average number of people in the bar")
SUMA13/TM
print("Average number of people in the dining room")
SUMA2/TM

TM







