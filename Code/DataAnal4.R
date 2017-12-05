rm(list = ls()) # remove all existing objects
graphics.off() # close all open graphics devices
setwd("/Users/admin/thermalresponse/Data/")
mydata <- read.csv("../Data/CollatedDatasetC.csv")
mydata$GenusSpecies <- paste(mydata$Genus,mydata$Species,sep = " ")
MySpecies <- as.matrix(unique(mydata$GenusSpecies))

pisum<-subset(mydata,mydata$Species=="pacificus")
brass<-subset(mydata,mydata$Citation=="Satar et al, 2005")
jsur<-subset(pisum,pisum$TraitName=="Egg/Female/Day") 

jsur<-subset(mydata,mydata$Unq_ID=="239") 
TRAIT<-as.numeric(as.vector(jsur$TraitValue))
TIME<-as.numeric(as.vector(jsur$Time))
TEMP<-as.numeric(as.vector(jsur$AmbientTemp))
plot(TIME,log(TRAIT), main="Brevicoryne brassicae ID244 \n Survivorship",ylab="Propotion", xlab="Time (Days)")

library(dplyr)
temp10 <- filter(jsur,AmbientTemp ==28)
trait10<-which.max(temp10$TraitValue) #locate peak trait
temp10<-temp10[4:22,] #indexing
TRAIT<-as.numeric(as.vector(temp10$TraitValue))
TIME<-as.numeric(as.vector(temp10$Time))
TEMP<-as.numeric(as.vector(temp10$AmbientTemp))
lm10 <- lm(log(TRAIT)~TIME)
abline(lm10, col ='blue',lwd=2)
print(lm10$coefficient)

temp22 <- filter(jsur, AmbientTemp == 16.6)
#trait22<-which.max(temp22$TraitValue) #locate peak trait
#temp22<-temp22[3:46,] #indexing
TRAIT<-as.numeric(as.vector(temp22$TraitValue))
TIME<-as.numeric(as.vector(temp22$Time))
TEMP<-as.numeric(as.vector(temp22$AmbientTemp))
lm22 <- lm(log(TRAIT)~TIME)
abline(lm22, col ='purple', lwd=2)

temp28 <- filter(jsur, AmbientTemp == 19.6)
#trait28<-which.max(temp28$TraitValue) #locate peak trait
#temp28<-temp28[4:22,] #indexing
TRAIT<-as.numeric(as.vector(temp28$TraitValue))
TIME<-as.numeric(as.vector(temp28$Time))
TEMP<-as.numeric(as.vector(temp28$AmbientTemp))
lm28<- lm(log(TRAIT)~TIME)
abline(lm28, col ='red', lwd=2)

temp34<- filter(jsur, AmbientTemp == 23.1)
#trait34<-which.max(temp34$TraitValue) #locate peak trait
#temp34<-temp34[:17,] #indexing
TRAIT<-as.numeric(as.vector(temp34$TraitValue))
TIME<-as.numeric(as.vector(temp34$Time))
TEMP<-as.numeric(as.vector(temp34$AmbientTemp))
lm34<- lm(log(TRAIT)~TIME)
abline(lm34, col ='orange', lwd=2)

temp35<- filter(jsur, AmbientTemp == 26.7)
#trait34<-which.max(temp34$TraitValue) #locate peak trait
#temp34<-temp34[:17,] #indexing
TRAIT<-as.numeric(as.vector(temp35$TraitValue))
TIME<-as.numeric(as.vector(temp35$Time))
TEMP<-as.numeric(as.vector(temp35$AmbientTemp))
lm35<- lm(log(TRAIT)~TIME)
abline(lm35, col ='orange', lwd=2)

print(lm15$coefficients)
print(lm22$coefficients)
print(lm28$coefficients)
print(lm34$coefficients)
print(lm35$coefficients)

#Z and K curve#
cal_zj <- function(Time,B0,Tmp){
  return((Tmp*Time)+B0)
}
cal_zj(B0=0.25830783,Tmp=15,Time=0.03943225)
cal_zj(B0=0.4738216	,Tmp=22,Time=0.04027389)
cal_zj(B0=2.3241497,Tmp=28,Time=0.1098056)
cal_zj(B0=1.6907162,Tmp=34,Time=0.9327322)
#cal_zj(B0=1.1781096,Tmp=26.7,Time=0.2348702)

#Ploting mortality and K value
AmTemp<-c(11.9,16.6,19.6,23.1,26.7)
Zj<-c(5.048171,1.776034,3.191571,5.165758,15.63761)#inversed value by divide with Time for z,z_j 
plot(AmTemp,Zj,main="Adult Mortality Rate(Z)\nAcrithosiphon pisum ID263", xlab="Ambient Temperature",ylab="Z(1/day)")
lines(AmTemp,Zj)

#Fecundity equation
cal_bx<-function(bpk,k,alpha,x){
  return((log(bpk,base=exp(1)))+(k*alpha)-(k*x))
}
cal_bx(bpk=1.06,k=0.8497916,x=0.03943225,alpha=34.61)
			-
#bpara<-subset(pisum,pisum$TraitName=="Peak Fecundity")

#Plotbx
AmTemp<-c(15,20,25,30)
bx<-c(4.476471,4.469169,4.0964,3.551483)
#bx<-log(bx,base=exp(1))
plot(AmTemp,bx,main="Fecundity(bx)\nBrevicoryne brassicae",xlab="Ambient Temperature",ylab="bx")
lines(AmTemp,bx)  #absolute

 #lx function
calc_lx<-function(z_j,z,alpha,x){
  return((alpha*((z)-(z_j)))-(z*x))
}
calc_lx(x=0.7749027,z=28.38539,z_j=0.3286721,alpha=7.06)
			
#transform ln(lx) to lx
transform<-function(lx){
  return(log(lx,base=exp(1)))
}
transform(lx=176.0845)


#lxfunction2 with La
calc_lx2<-function(la,z,alpha,x){
  return((log(la,base=exp(1)))-z*(x-alpha))
}
calc_lx2(la=30.18,alpha=7.06,z=28.38539,x=0.7749027)
				
#plot lx
AmTemp<-c(15,22,28,34)
lx<-c(3.46829,2.997977,3.708542,5.170964) #manually exp
plot(AmTemp,lx,main="Survivorship (lx) \nAcrythosiphon pisum",xlab="Ambient temperature",ylab="lx")
lines(AmTemp,lx)

 ##rmax##
rmax <- function(k,bpk,z_j,z,alpha){
  return(((k+z)*((log(bpk/(k+z)))-(alpha*z_j)))/(alpha*(k+z)+1))
}
rmax(k=33.40361,bpk=1.53,z_j=0.3286721,z=28.38539,alpha=7.06)
				
##plotrmax
AmTemp<-c(15,22,28,34)
rm<-c(0.140097,0.3334402,0.5730869,0.8505831)
plot(AmTemp,rm,main="Intrinsic growth rate \nTetranychus pacificus",xlab="Ambient Temperature",ylab="rmax")
lines(AmTemp,rm)

#calculating juvenile mortality rate
cal_zjj<-function(la,alpha){
  return((log(la,base=exp(1)))/alpha)
}
cal_zjj(alpha=9.21,la=92)

