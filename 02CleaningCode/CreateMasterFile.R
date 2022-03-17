##Create Master File
library(ggplot2)
library(plyr)

options(stringsAsFactors = FALSE)
setwd("C:/Users/rep31/Desktop/Madrone/Madrone5/Madrone3/Basic Datasets") #easier directory to work from

years <- c(12, 13, 14, 15)
sites <- c("BenLomond", "PuyallupHill", "PuyallupValley", "Starker", "Sprague")
sitesshort <- c("BL", "PH", "PV", "SF", "SO")
ecoregiondata <- read.csv("EcoregionData.csv")

pertfiles <- list("BLFulldata.csv", "PHFulldata.csv", "PVFulldata.csv", "SFFulldata.csv", 
                  "SOFulldata.csv", "PYFulldata.csv")
sitesdata <- lapply(pertfiles,read.csv)
names(sitesdata) <- sites

site_ind = 0
totalsitedata = c()

for (site_dat in sitesdata){
  site_ind = site_ind + 1
  
  if (site_ind == 1){
    totalsitedata = site_dat
    
  } else {
    oldtotdat = totalsitedata #so you don't lose the data
    numcols <- length(oldtotdat[1,])
    
    totalsitedata = matrix(nrow = length(oldtotdat[,1]) + length(site_dat[,1]), ncol = numcols)
    colnames(totalsitedata) = colnames(oldtotdat)
    
    for (ind in 1:numcols){
      totalsitedata[,ind] = append(oldtotdat[,ind], site_dat[,ind])
    }
    
    totalsitedata <- as.data.frame(totalsitedata)
  }
  
}

write.csv(totalsitedata, "MasterDataFile.csv")
