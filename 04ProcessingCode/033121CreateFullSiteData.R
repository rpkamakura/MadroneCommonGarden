## Put together all the files for each source
library(ggplot2)
library(plyr)
#source("PerSiteAnalys.R")

options(stringsAsFactors = FALSE)

#Get all the yearly files for each site
filenames <- list.files(path="../Basic Datasets/data_byYears", pattern="*.csv", full.names=TRUE)

## Sort files by data year
filesPH <- c()
filesPV <- c()
filesSF <- c()
filesSO <- c()

for (path in filenames){
  fle <- strsplit(path, "/")[[1]][4] #just get the filename and not the path
  PLsite <- gsub("[^A-Z]", "", fle) #get the sitename
  if (PLsite == "BL"){
    next
  } else if (PLsite == "PH"){
    filesPH <- append(filesPH, path)
  } else if (PLsite == "PV"){
    filesPV <- append(filesPV, path)
  } else if (PLsite == "SF"){
    filesSF <- append(filesSF, path)
  } else if (PLsite == "SO"){
    filesSO <- append(filesSO, path)
  }
}

#Data with different Distances
DistData <- read.csv("../Distance Data/DistancesDir2.csv")

#Data with climatic variables
ClimData <- read.csv("../Climate Data/033121LocClim.csv")

#Set up various variables
#years <- c(12, 13, 14, 15)
years <- c(14, 15)
sites <- c("PuyallupHill", "PuyallupValley", "Starker", "Sprague")
sitesshort <- c("PH", "PV", "SF", "SO")

#Get the ecoregions for each family
ecoregiondata <- read.csv("../Basic Datasets/EcoregionData.csv")


st_ind = 0 #so you can index the sites

#For each Site
for (st in sites){
  if (st == "PuyallupHill"){
    yrfiles = filesPH
    st_ind = 5
  } else if (st == "PuyallupValley"){
    yrfiles = filesPV
    st_ind = 4
  } else if (st == "Starker") {
    yrfiles = filesSF
    st_ind = 3
  } else {
    yrfiles = filesSO
    st_ind = 2
  }
  totalsitedata = c()
  stclim_ind <- match(st, ClimData$Family)
  st_bFFP <- ClimData$bFFP[stclim_ind]
  st_eFFP <- ClimData$eFFP[stclim_ind]
  st_MSP <- ClimData$MSP[stclim_ind]
  st_Asp <- ClimData$Aspect[stclim_ind]
  st_Slo <- ClimData$Slope[stclim_ind]
  st_Hardi <-ClimData$Hardiness[stclim_ind]
  st_DD5 <- ClimData$DD.5_sp[stclim_ind]
  
  #Go through each file
  for (fl in yrfiles){
    yearlocdat <- read.csv(fl) 
    yr <- gsub("[^0-9]", "", fl)
    
    if (!(as.numeric(yr) %in% years)){ #allows you to subset years of data
      next
    }
    
    dist_vec <- c()
    elevdist_vec <- c()
    eco_vec <- c()
    source_vec <- c()
    clim_vec <- list()
    iter <- 0
    
    #odd issue in the SO data
    if ("PF2 " %in% yearlocdat$Name){
      yearlocdat$Name <- replace(yearlocdat$Name, yearlocdat$Name == "PF2 ", "PF2")
    }
    
    for (fam in na.omit(yearlocdat$Name)){

      iter <- iter + 1
      #find the corresponding distance between the source and planting locations
      dist_ind <- match(fam, DistData$X)
      clim_ind <- match(fam, ClimData$Family)
      
      col_ind <- st_ind
      elevcol_ind <- st_ind + 4
      
      
      dist <- DistData[dist_ind, col_ind]
      dist_vec <- append(dist_vec, dist)
      
      elevdist <- DistData[dist_ind, elevcol_ind]
      elevdist_vec <- append(elevdist_vec, elevdist)
      
      if (is.na(dist)){
        print(paste("no distance to planting location found for", fam, "at", st, "in", yr))
      }
      
      #Find the ecoregion and source data
      eco_ind <- match(fam, ecoregiondata$Family)
      srce <- ecoregiondata$Source[eco_ind]
      ecoreg <- ecoregiondata$EcoregionShort[eco_ind]
      source_vec <- append(source_vec, srce)
      eco_vec <- append(eco_vec, ecoreg)
      
      #Get Climate data
      #8 is bFFP, 9 is eFFP and 10 is MSP in the dataset as currently done
      for (clim in 1:8){ 
        clim_col <- clim+8 #appropriate column in the list
        
        clim_val <- ClimData[clim_ind, clim_col] #get the value you want
          
        if (length(clim_vec) < clim){
          clim_vec <- append(clim_vec, c(clim_val))
        } else {
          clim_vec[[clim]] <- append(clim_vec[[clim]], clim_val)
        }
      }
      
    } #end family loop
    columns <- c("Site" ,"Ecoregion", "Source", "Family", "Block", "Tree", "Condition", 
                 "Height", "Leaders", "Distance", "ElevDist", "eFFP", "MSP", "Slope", 
                 "Aspect", "Hardiness", "DD.5_sp")
    
    year_dat <- as.data.frame(matrix(0, ncol = length(columns), nrow = length(yearlocdat$Site)))
    colnames(year_dat) <- columns
    
    #Block, Tree, Cond, Leaders, ht
    year_dat$Block <- yearlocdat$Block[!is.na(yearlocdat$Name)]
    year_dat$Tree <- yearlocdat$Tree[!is.na(yearlocdat$Name)]
    year_dat$Condition <- yearlocdat$cond[!is.na(yearlocdat$Name)]
    year_dat$Leaders <- yearlocdat$leaders[!is.na(yearlocdat$Name)]
    year_dat$Height <- yearlocdat$ht[!is.na(yearlocdat$Name)]
    
    #add distances
    year_dat$Distance <- dist_vec
    year_dat$ElevDist <- elevdist_vec
    
    #add location
    year_dat$Source = source_vec
    year_dat$Ecoregion = eco_vec
    year_dat$Family <- na.omit(yearlocdat$Name)
    year_dat$Site <- rep(yearlocdat$Site[1], length(source_vec))
    #add climate
    #year_dat$bFFP <- clim_vec[[1]] - st_bFFP
    year_dat$eFFP <- clim_vec[[2]] - st_eFFP
    year_dat$MSP <- clim_vec[[3]] - st_MSP
    year_dat$Slope <- clim_vec[[5]] - st_Slo
    year_dat$DD.5_sp <- clim_vec[[8]] - st_DD5
    
    ##Not distance
    year_dat$Hardiness <- clim_vec[[6]] #could make this a distance by subtracting min val or something
    year_dat$Aspect <- clim_vec[[4]]
    
    # fam_iter <- 0
    # year_dat$Aspect <- rep(0, length(clim_vec[[5]])) #initialize the aspect vector
    # 
    # for (fam_asp in clim_vec[[5]]){
    #   fam_iter = fam_iter + 1
    #   if (is.na(fam_asp)){
    #     year_dat$Aspect[fam_iter] <- NA
    #     next
    #   }
    #   if (st_Asp <= 0 && fam_asp <= 0){ ## both negative
    #     year_dat$Aspect[fam_iter] <- st_Asp - fam_asp
    #   } else if (st_Asp >= 0 && fam_asp >= 0){ ## both positive
    #     year_dat$Aspect[fam_iter] <- st_Asp - fam_asp
    #   } else if (st_Asp > 0 && fam_asp < 0){ ##site is positive
    #     if (fam_asp < -pi + st_Asp){
    #       year_dat$Aspect[fam_iter] = -(pi - st_Asp) + (-pi - fam_asp)
    #     } else if (fam_asp > -pi + st_Asp){
    #       year_dat$Aspect[fam_iter] = st_Asp - fam_asp
    #     } else {
    #       year_dat$Aspect[fam_iter] = pi 
    #     }
    #   } else if (st_Asp < 0 && fam_asp > 0){ ## site is negative
    #     if (fam_asp < pi + st_Asp){
    #       year_dat$Aspect[fam_iter] = st_Asp - fam_asp
    #     } else if (fam_asp > pi + st_Asp){
    #       year_dat$Aspect[fam_iter] = -(-pi - st_Asp) + (pi - fam_asp)
    #     } else {
    #       year_dat$Aspect[fam_iter] = pi 
    #     }
    #   } 
    # }
    
    
    if (yr == 14){
      totalsitedata = year_dat
      yearcol = rep(yr, length(year_dat$Distance))
      totalsitedata$Year = yearcol

    } else {
      
      ## a rather convoluted way to add the dataframes together
      oldtotdat = totalsitedata
      totalsitedata = matrix(nrow = length(oldtotdat[,1]) + length(year_dat[,1]), ncol = length(oldtotdat[1,]))
      colnames(totalsitedata) = colnames(oldtotdat)
      
      for (ind in 1:length(year_dat)){ #Need to adjust this
        totalsitedata[,ind] = append(oldtotdat[,ind], year_dat[,ind])
      }
      totalsitedata <- as.data.frame(totalsitedata)
      
      #Year
      newyearcol = rep(yr, length(year_dat$Distance))
      totalsitedata$Year = append(oldtotdat$Year, newyearcol)
      
    }
  }  
    #get rid of data you can't use
    totalsitedata = totalsitedata[totalsitedata$Site != "",]
    totalsitedata = totalsitedata[!is.na(totalsitedata$Site),]
    
 if (st == "PuyallupHill"){
    PHtotdat = totalsitedata
    
  } else if (st == "PuyallupValley"){
    PVtotdat = totalsitedata
    
  } else if (st == "Starker") {
    SFtotdat = totalsitedata
    
  } else if (st == "Sprague") {
    SOtotdat = totalsitedata
  }
  
}

write.csv(PHtotdat, "../Basic Datasets/033121PHFulldata14to15.csv")
write.csv(PVtotdat, "../Basic Datasets/033121PVFulldata14to15.csv")
write.csv(SFtotdat, "../Basic Datasets/033121SFFulldata14to15.csv")
write.csv(SOtotdat, "../Basic Datasets/033121SOFulldata14to15.csv")


