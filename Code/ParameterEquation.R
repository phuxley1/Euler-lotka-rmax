rm(list = ls()) # remove all existing objects
graphics.off() # close all open graphics devices
# setwd("/Users/admin/thermalresponse/Data/")
mydata <- read.csv("../Data/CollatedDatasetC.csv")

mydata$GenusSpecies <- paste(mydata$Genus,mydata$Species,sep = " ")
MySpecies <- as.matrix(unique(mydata$GenusSpecies))
MyTraits <- unique(mydata$TraitName)

# Calculation on pisum second instar stage 
aegypti<-subset(mydata, mydata$Species=='pisum')
jsur<-subset(aegypti, aegypti$TraitName=='Juvenile Survivosrhip')
jsur<-subset(jsur,jsur$Time!="NA")
jsur<-subset(mydata,mydata$Unq_ID=="179")
#devtime<-subset(mydata,mydata$Unq_ID=="179")
sur<-subset(mydata,mydata$Unq_ID=="299")
bpeak<-subset(mydata,mydata$Unq_ID=="300")
ids <- unique(jsur$Unq_ID)

TRAIT<- as.numeric(as.vector(jsur$TraitValue))
TIME<-  as.numeric(as.vector(jsur$Time))
SURVIVE<-as.numeric(as.vector(sur$TraitValue))
EX<-as.numeric(as.vector(sur$Time))
PEAK<-as.numeric(as.vector(bpeak$TraitValue))


calc_zj<-function(la,alpha){
  return (-log(la, base = exp(1))/alpha)
}
calc_zj(la=TRAIT,alpha=TIME)

calc_Z<-function(la,alpha,x,lx){
  return((-calc_zj(la,alpha)*alpha-log(lx,base=exp(1)))/(x-alpha))
}
calc_Z(la=TRAIT,alpha=TIME,x=EX,lx=SURVIVE)

calc_b<-function(bpk,x,k=0.1,alpha){
  return(-log(bpk,base=exp(1))*k*(alpha-x))
}
calc_b(bpk=PEAK,x=EX,alpha=TIME)


surv<-subset(aegypti,aegypti$TraitName=="Survivorship")
temp<-unique(surv$AmbientTemp)
tempj<-unique(juvsur$AmbientTemp)
juvsur<-subset(aegypti,aegypti$TraitName=="Juvenile Survivorship")
fecund<-subset(aegypti,aegypti$TraitName=="Egg/Female/Day")
numb<-unique(mydata$Unq_ID)
sort(numb)
mort<-subset(mydata,mydata$TraitName=="Juvenile Mortality Rate")
