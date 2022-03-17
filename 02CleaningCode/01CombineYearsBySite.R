##Combine Years by Site

#First find all the relevant files
PHFiles <- list.files(path="../01ExampleRawData/01CondGrowthPhen", pattern="*PH.csv", all.files=FALSE,
                      full.names=TRUE)

PVFiles <- list.files(path="../01ExampleRawData/01CondGrowthPhen", pattern="*PV.csv", all.files=FALSE,
                      full.names=TRUE)

SFFiles <- list.files(path="../01ExampleRawData/01CondGrowthPhen", pattern="*SF.csv", all.files=FALSE,
                      full.names=TRUE)

SOFiles <- list.files(path="../01ExampleRawData/01CondGrowthPhen", pattern="*SO.csv", all.files=FALSE,
                      full.names=TRUE)


#list out all the sites
sites <- c("PH", "PV", "SF", "SO")

#Go through each site and combine all the data from each year
for(s in 1:length(sites)){
  
  st <- sites[s] #get the site of interest
  
  #get the correct datasets
  if (st == "PH"){
    
    datasets <- lapply(PHFiles, read.csv)
    names(datasets) <- c("12", "13", "14", "15")
    
  } else if (st == "PV") {
    
    datasets <- lapply(PVFiles, read.csv)
    names(datasets) <- c("12", "13", "14", "15")
    
  } else if (st == "SF"){
    
    datasets <- lapply(SFFiles, read.csv)
    names(datasets) <- c("12", "13", "14", "15")
    
  } else {
    
    datasets <- lapply(SOFiles, read.csv)
    names(datasets) <- c("12", "13", "14", "15")
    
  }
  
  #rowbind all the datasets together
  combined <- rbind(datasets[[1]], datasets[[2]])
  combined <- rbind(combined, datasets[[3]])
  combined <- rbind(combined, datasets[[4]])
  
  #add the year so you know which is which
  combined$Year <- c(rep(12, length(datasets[[1]][,1])), rep(13, length(datasets[[2]][,1])), 
                     rep(14, length(datasets[[3]][,1])), rep(15, length(datasets[[4]][,1])))
  
  #Get rid of unnecessary row
  combined <- combined[,2:9]
  
  #save it as a CSV
  write.csv(combined, paste("../03ExampleCleanedData/", st, "Fulldata.csv", sep=""))
  
}
