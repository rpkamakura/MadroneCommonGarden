##Combine Years by Site

#First find all the relevant files
PHFiles <- list.files(path="../01ExampleRawData/01CondGrowthPhen", pattern="*PH.csv", all.files=FALSE,
                      full.names=FALSE)

PVFiles <- list.files(path="../01ExampleRawData/01CondGrowthPhen", pattern="*PV.csv", all.files=FALSE,
                      full.names=FALSE)

SFFiles <- list.files(path="../01ExampleRawData/01CondGrowthPhen", pattern="*SF.csv", all.files=FALSE,
                      full.names=FALSE)

SOFiles <- list.files(path="../01ExampleRawData/01CondGrowthPhen", pattern="*SO.csv", all.files=FALSE,
                      full.names=FALSE)


#list out all the sites
sites <- c("PH", "PV", "SF", "SO")

#Go through each site and combine all the data from each year
for(s in 1:length(sites)){
  
}
