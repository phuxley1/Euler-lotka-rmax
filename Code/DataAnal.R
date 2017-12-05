rm(list = ls()) # remove all existing objects
graphics.off() # close all open graphics devices
mydata <- read.csv("../Data/CollatedDatasetB.csv")

mydata$GenusSpecies <- paste(mydata$Genus,mydata$Species,sep = " ")

MySpecies <- as.matrix(unique(mydata$GenusSpecies))

MyTraits <- unique(mydata$Trait)

#  [1] Juvenile Survivorship     Development Rate         
#  [3] Pre-reproductive period   Longevity                
#  [5] Mortality Rate            Egg/Female               
#  [7] Eggs/Female/Cycle         Eggs/Female/day          
#  [9] Gonotrophic Cycle Length  Egg/Female/Day           
# [11] lifespan                  Eggs/Female              
# [13] Eggs/Female/Day           Juvenile Mortality Rate  
# [15] Range Development Rate    Development (% emerged)  
# [17] Survivorship (% survived) Fecundity                
# [19] Survivorship              Adult mortality rate     
# [21] Age at First Reproduction Egg Rafts(/Female/Day?)  
# [23] Preovipostion Period      survivorship             
# [25] Development time          Juvenile Mortality       
# [27] Reproductive Period       Gonotrophic length       
# [29] Developmental time        Pre-reproductive stage   
# [31] Reproductive period       Pre-reproductive period  

for (i in 1:nrow(MySpecies)){
	
	
	tmpData <- subset(mydata, GenusSpecies == MySpecies[i])
	
	MyID <- unique(tmpData$Unq_ID)
	
	#browser()
	
	
	
	}
