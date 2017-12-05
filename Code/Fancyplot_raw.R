fancyPlot <- function(inputData, IDS, saveName, xLabel="Days", yLabel="Time", mainTitle, lessFancy=FALSE){

  library(scales)

  for (i in IDS) {
    tmpData <- subset(inputData,  Unq_ID == i)
    pdf(paste(saveName, i,".pdf", sep = ''))
    TEMP = as.numeric(as.vector(tmpData$AmbientTemp))
    TIME = as.numeric(as.vector(tmpData$Time))
    if(!lessFancy)
      TRAIT = as.numeric(as.vector(tmpData$TraitValue))
    else
      TRAIT = as.numeric(as.vector(tmpData$OLDTraitValue))
    GENUS = tmpData$GenusSpecies[1]
    m = max(TEMP)
    t = min(TEMP)
    colfunc <- colorRampPalette(c("blue", "red"))
    cols = colfunc(m-t+1)[TEMP-t+1]
    if(lessFancy)
      cols="navyblue"
    plotTitle = sprintf("%s for ID %d\nGenus %s",mainTitle,i,GENUS)  #%s...%s = specificed what in string, \n = new line
    if(!lessFancy)
      plot( TIME, TRAIT, col=cols, ylim=range(TRAIT) + c(0,1), xlim=range(TIME), xlab=xLabel, ylab=yLabel, pch=16, cex=1.5, main=plotTitle, cex.main=1.2 ) 
    else
      plot( TEMP, TRAIT, col=cols, ylim=range(TRAIT), xlim=range(TEMP), xlab=xLabel, ylab=yLabel, pch=16, cex=1.5, main=plotTitle, cex.main=1.2 )
    u = unique(TEMP)
    if(!lessFancy){
      for(x in u){
        ii = ( as.numeric(as.vector(tmpData$AmbientTemp)) == x )
        if(sum(ii) > 1){
          lineColour = alpha(unique(cols[ii]), alpha = 0.5)
          lines( TIME[ii], TRAIT[ii], lwd=3, col=lineColour )
        }
      }
    }
    sortTemp = sort(TEMP, index.return=TRUE)
    sortCols = cols[sortTemp$ix]    #ix obtain from sort temperature by index.return = sort from low to high value
    legendCols = unique( sortCols )
    if(!lessFancy)
      legend(x=min(TIME), y=max(TRAIT)+1, legend=sort(unique(TEMP)),col=legendCols,pch=16,cex=1,ncol=7,bty = "n")
    dev.off()
  } 

}

rm(list = ls()) # remove all existing objects
graphics.off() # close all open graphics devices
setwd("/Users/admin/thermalresponse/Data/")
mydata <- read.csv("../Data/CollatedDatasetC.csv")

mydata$GenusSpecies <- paste(mydata$Genus,mydata$Species,sep = " ")
MySpecies <- as.matrix(unique(mydata$GenusSpecies))
MyTraits <- unique(mydata$TraitName)

aegypti<-subset(mydata, mydata$Species=='pisum')
#Juvenile Survivorship     Survivorship    Development Time        
#Mortality Rate           Juvenile Mortality Rate   Egg/Female/Day                   
#Fancyplot function
jsur<-subset(aegypti, aegypti$TraitName=='Egg/Female/Day')
jsur<-subset(jsur,jsur$Time!="NA")
ids <- unique(jsur$Unq_ID)
fancyPlot(inputData = jsur, IDS = ids, saveName = '../Results/Fecundity', xLabel = "Days", yLabel = "Egg/Female/Day", mainTitle = "Fecundity")

###Less Fancy function ##
jsur<-subset(aegypti, aegypti$OLDTraitName=='Pre-reproductive Period')
jsur$AmbientTemp <-as.numeric(as.vector(jsur$AmbientTemp))
jsur$OLDTraitValue <- as.numeric(as.vector(jsur$OLDTraitValue))
#jsur<-subset(jsur,jsur$TraitValue!="NA")
ids <- unique(jsur$Unq_ID)

fancyPlot(inputData = jsur, IDS = ids, saveName = '../Results/Pre-repro', xLabel = "Ambient Temperature", yLabel = "Day", mainTitle = "Pre-reproductive Period", lessFancy = TRUE)




 for (i in ids) {
  
  tmpData <- subset(jsur,  Unq_ID == i)
  pdf(paste('../Results/fecundity', i,".pdf", sep = ''))
  TEMP = as.numeric(as.vector(tmpData$AmbientTemp))
  TIME = as.numeric(as.vector(tmpData$Time))
  TRAIT = as.numeric(as.vector(tmpData$TraitValue))
  m = max(TEMP)
  t = min(TEMP)
  colfunc <- colorRampPalette(c("blue", "red"))
  cols = colfunc(m-t+1)[TEMP-t+1]
  plot( TIME, TRAIT, col=cols, ylim=c(0,max(TRAIT)+1), xlab="Days", ylab="Egg/Female/Day", pch=16, cex=1.5, main=paste("Fecondity density for ID ",i,sep="") ) 
  u = unique(as.numeric(as.vector(tmpData$AmbientTemp)))
  for(x in u){
       ii = ( as.numeric(as.vector(tmpData$AmbientTemp)) == x )
       if(sum(ii) > 1){
        print(TIME[ii])
        print(TRAIT[ii])
        lineColour = alpha(unique(cols[ii]), alpha = 0.5)
        lines( TIME[ii], TRAIT[ii], lwd=3, col=lineColour )
       }
  }
  #sapply is applying function to element (ie.u)
  sortTemp = sort(TEMP, index.return=TRUE)
  sortCols = cols[sortTemp$ix]
  legendCols = unique( sortCols )
  legend(x=min(TIME), y=max(TRAIT)+1, legend=sort(unique(TEMP)),col=legendCols,pch=16,cex=1,ncol=7,bty = "n")
  dev.off()
} 


#jsur<-subset(aegypti, aegypti$OLDTraitName=='Development Time')
#jsur$AmbientTemp <-as.numeric(as.vector(jsur$AmbientTemp))
#jsur$OLDTraitValue <- as.numeric(as.vector(jsur$OLDTraitValue))
#ids <- unique(jsur$Unq_ID)

#for (i in ids) {
 # tmpData <- subset(jsur,  Unq_ID == i)
  #pdf(paste('../Results/Development', i,".pdf", sep = ''))
  #TEMP = tmpData$AmbientTemp
  #TRAIT = tmpData$OLDTraitValue
  #plot(TEMP, TRAIT, xlim=range(TEMP), ylim=range(TRAIT), xlab="Ambient Temperature" ,ylab="Development Time", main=paste("Development time for ID ",i,sep=""), pch=16, cex=1.5)
  #dev.off()
#}

