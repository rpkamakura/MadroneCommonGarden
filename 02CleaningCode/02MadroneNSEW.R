###Changing Lat/Long into UTM

library(sp)
library(rgdal)

###############Converting Lat Long to UTM
SeedSourceLoc = read.csv('../01ExampleRawData/02LocationData/SeedSourceLocations.csv')

#Function to convert lat-long to UTM
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

##################### Calculating Distances

##Gives you distance in m
Distance <- function(loc1, loc2){
  x = loc1[1] - loc2[1]
  y = loc1[2] - loc2[2]
  dist = sqrt(x^2 + y^2)
  return(dist)
}

###For the Planting Locations
PlantingLoc = read.csv('../01ExampleRawData/02LocationData/PlantingLocations.csv') 
nsites <- length(PlantingLoc$Planting.Loc)
plats <- PlantingLoc$Latitude #sort out latitudes
plongs <- PlantingLoc$Longitude #sort out longitudes
pUTM <- LongLatToUTM(plongs, plats, 10) #convert them to UTM

#Add the new UTM values to the Planting Location Sheet
PlantingLoc$UTMx <- pUTM[,2]
PlantingLoc$UTMy <- pUTM[,3]

##Distance matrix for all options (total, NS, and EW distances)
unidirmov <- as.data.frame(matrix(nrow = length(SeedSourceLoc$Family), ncol = 8))
rownames(unidirmov) <- SeedSourceLoc$Family

#Column names would need to be adapted with new sites. Dist is overall, NS is 
#North/South and EW is East/West distance.
colnames(unidirmov) <- c("SO_Dist", "SF_Dist", "PV_Dist",  "PH_Dist", 
                         "SO_ElevDist", "SF_ElevDist", "PV_ElevDist", "PH_ElevDist") 

#For each direction 

  for (loc in 2:(length(PlantingLoc$Planting.Loc)-1)){
    
    loc_nm <- PlantingLoc$Planting.Loc[loc]
    plat <- PlantingLoc$Latitude[loc]
    plong <- PlantingLoc$Longitude[loc]
    pUTMx <- PlantingLoc$UTMx[loc]
    pUTMy <- PlantingLoc$UTMy[loc]
    pElev <- (PlantingLoc$Elevation[loc])
    
    
    lats <- SeedSourceLoc$Latitude
    longs <- SeedSourceLoc$Longitude
    sElev <- SeedSourceLoc$Elevation
    dir = 1
      
    UTM <- LongLatToUTM(longs, lats, 10) #convert them to UTM
    #seems kinda complicated but is just a way to get to the right column
    #Should work
    col_ind = loc -1 
  
    ##To Calculate Distances
    loc1 <- c(pUTMx, pUTMy)
    for (fam in 1:length(SeedSourceLoc$Family)){
      loc2 = c(UTM[fam,2],UTM[fam,3])
      
      #find distance
      dist <- Distance(loc1, loc2)
      
      #put the dist, in km, in the matrix
      unidirmov[fam, col_ind] <- dist/1000
      
      unidirmov[fam, col_ind + 4] <- sElev[fam] - pElev
    
    } #end family for loop
    
  } #end planting location loop


#Save the distances to some kind of CSV
write.csv(unidirmov, "../01ExampleRawData/02LocationData/DistancesDir.csv")
